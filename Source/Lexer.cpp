
void InitalizeLexer(Lexer *lex, uint32_t fileID, char *buffer) {
  assert(buffer != nullptr);
  lex->current = buffer;
  lex->lineNumber = 1;
  lex->columnNumber = 1;
  lex->fileID = fileID;
}

static inline bool IsNewline(char *s) {
  if (s[0] == '\r') return s[1] == '\n';
  return s[0] == '\n';
}

static inline bool IsAlpha(char c) {
  bool result = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
  return result;
}

static inline bool IsNumeric(char c) {
  bool result = (c >= '0') && (c <= '9');
  return result;
}

static inline bool IsAlphaNumeric(char c) {
  bool result = IsAlpha(c) || IsNumeric(c);
  return result;
}

void EatNewline(Lexer *lex) {
  assert(IsNewline(lex->current));
  if (*lex->current == '\r') lex->current += 2;
  else lex->current += 1;
  lex->lineNumber += 1;
  lex->columnNumber = 1;
}

void EatCurrent(Lexer *lex) {
  if (IsNewline(lex->current)) {
    EatNewline(lex);
  } else {
    lex->current += 1;
    lex->columnNumber += 1;
  }
}

void AppendCurrent(Token *t, Lexer *lex) {
  t->length += 1;
  EatCurrent(lex);
}

bool IsSymbolicUnaryToken(TokenType tokenType) {
  bool result = (tokenType == TokenType_SymbolAddress) ||
    (tokenType == TokenType_SymbolValue) ||
    (tokenType == TokenType_SymbolNot) ||
    (tokenType == TokenType_SymbolInvert);
  return result;
}

bool IsArithmeticToken(TokenType tokenType) {
  bool result = (tokenType == TokenType_SymbolAdd) ||
    (tokenType == TokenType_SymbolSub) ||
    (tokenType == TokenType_SymbolMul) ||
    (tokenType == TokenType_SymbolDiv) ||
    (tokenType == TokenType_SymbolMod);
  return result;
}

//A realitivly ineffiecant procedure to tokenize the current
//position of the lexer within the buffer.  Does a signfigant
//ineffeicant amount of string comparing for simplicity during
//prototyping phase.
Token GetToken(Lexer *lex) {
  Token token = {};
  token.location.fileID = lex->fileID;

  //Eat any trailing spaces
  while(*lex->current == ' ') EatCurrent(lex);

  if (IsNewline(lex->current)) {
    EatNewline(lex);
    if (IsNewline(lex->current)) {
      return GetToken(lex);
    }

    token.location.lineNumber = lex->lineNumber;
    token.location.columnNumber = lex->columnNumber;
    int indentLevel = 0;
    int spaceCount = 0;
    static const uint32_t INDENT_SPACE_COUNT = 2;
    while (*lex->current == ' ') {
      spaceCount++;
      EatCurrent(lex);
      if (spaceCount >= INDENT_SPACE_COUNT) {
        indentLevel += 1;
        spaceCount = 0;
      }
    }

    if (indentLevel > lex->currentIndentLevel) {
      token.type = TokenType_BlockOpen;
      lex->currentIndentLevel = indentLevel;
      token.tokenID = lex->internalTokenCounter++;
      return token;
    } else if (indentLevel < lex->currentIndentLevel) {
      token.type = TokenType_BlockClose;
      lex->currentIndentLevel = indentLevel;
      token.tokenID = lex->internalTokenCounter++;
      return token;
    }
  }

  if (*lex->current == '\t') {
    assert(false);
  }

  if (*lex->current == ' ') assert(false);
  while(*lex->current == ' ') EatCurrent(lex);
  token.type = TokenType_Invalid;
  token.text = lex->current;
  token.location.lineNumber = lex->lineNumber;
  token.location.columnNumber = lex->columnNumber;
  token.tokenID = lex->internalTokenCounter++;

  //This is a small helper function to make the lexer's code more simplifed.
  //This is a very inneffeciant procedure usefull to get the lexer working
  //and for rapid prototyping.
  auto SetTokenTypeIfMatch = [&token](TokenType type, const char *s, size_t length) -> bool {
    if (strncmp(token.text, s, length) == 0) {
      token.type = type;
      return true;
    }
    return false;
  };

  auto SetTokenTypeIfMatchAndAppend = [&token, lex](TokenType type, const char *s, size_t length) -> bool {
    if (strncmp(token.text, s, length) == 0) {
      token.type = type;
      for (size_t i = 0; i < length; i++) {
        AppendCurrent(&token, lex);
      }
      return true;
    }
    return false;
  };

  //Identifiers and Keywords
  if (IsAlpha(*lex->current) || *lex->current == '_') {
    while (IsAlphaNumeric(*lex->current) || *lex->current == '_')
      AppendCurrent(&token, lex);
    if (SetTokenTypeIfMatch(TokenType_KeywordType, LiteralAndLength("TYPE"))) return token;
    if (SetTokenTypeIfMatch(TokenType_KeywordIf, LiteralAndLength("IF"))) return token;
    if (SetTokenTypeIfMatch(TokenType_KeywordElse, LiteralAndLength("ELSE"))) return token;
    if (SetTokenTypeIfMatch(TokenType_KeywordWhile, LiteralAndLength("WHILE"))) return token;
    if (SetTokenTypeIfMatch(TokenType_KeywordReturn, LiteralAndLength("RETURN"))) return token;
    if (SetTokenTypeIfMatch(TokenType_KeywordForeign, LiteralAndLength("FOREIGN"))) return token;
    token.type = TokenType_Identifier;
    return token;
  }

  //Integer and Floating point contants
  //If the numeric constant is malformed(two decimals, etc) error
  //checking is defered to the parsing stage of the compiler
  else if (IsNumeric(*lex->current)) {
    token.type = TokenType_Number;
    while (IsNumeric(*lex->current) || *lex->current == '.') {
      AppendCurrent(&token, lex);
    }
  }

  //String literals
  //TODO Proper string escaping handling and
  //character inserting
  else if (*lex->current == '"') {
    token.type = TokenType_String;
    EatCurrent(lex);
    token.text = lex->current;
    while (*lex->current != '"') {
      AppendCurrent(&token, lex);
    }
    EatCurrent(lex);
  }

  //Comments
  else if (*lex->current == '#') {
    while(!IsNewline(lex->current)) {
      EatCurrent(lex);
    }
    EatNewline(lex);
    return GetToken(lex);
  }

  //All other symbols
  //TODO Very silly and ineffeciant way of determining symbol tokens
  //Usefull during prototying to get things done quick
  else if (SetTokenTypeIfMatchAndAppend(TokenType_ParenOpen, LiteralAndLength("("))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_ParenClose, LiteralAndLength(")"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_ArrayOpen, LiteralAndLength("["))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_ArrayClose, LiteralAndLength("]"))) return token;

  else if (SetTokenTypeIfMatchAndAppend(TokenType_SymbolAdd, LiteralAndLength("+"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_SymbolSub, LiteralAndLength("-"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_SymbolMul, LiteralAndLength("*"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_SymbolDiv, LiteralAndLength("/"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_SymbolMod, LiteralAndLength("%"))) return token;

  else if (SetTokenTypeIfMatchAndAppend(TokenType_SymbolColon, LiteralAndLength(":"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_SymbolEquals, LiteralAndLength("="))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_SymbolDot, LiteralAndLength("."))) return token;

  else if (SetTokenTypeIfMatchAndAppend(TokenType_SymbolLessThan, LiteralAndLength("<"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_SymbolGreaterThan, LiteralAndLength(">"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_SymbolOr, LiteralAndLength("|"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_SymbolAnd, LiteralAndLength("&"))) return token;

  else if (SetTokenTypeIfMatchAndAppend(TokenType_SymbolNot, LiteralAndLength("!"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_SymbolInvert, LiteralAndLength("~"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_SymbolAddress, LiteralAndLength("@"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_SymbolValue, LiteralAndLength("$"))) return token;

  else if (*lex->current == 0) {
    token.type = TokenType_EndOfBuffer;
  } else {
    AppendCurrent(&token, lex);
    token.type = TokenType_Invalid;
  }
  
  return token;
}