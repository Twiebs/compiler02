

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

static inline bool IsBase10Digit(char c) {
  bool result = (c >= '0') && (c <= '9');
  return result;
}

static inline bool IsBase16Digit(char c) {
  bool result = (c >= '0' && c <= '9') ||
    (c >= 'A' && c <= 'F');
  return result;
}

static inline bool IsBase2Digit(char c) {
  bool result = (c == '0') || (c == '1');
  return result;
}

void InitalizeLexer(Lexer *lex, uint32_t fileID, char *buffer) {
  assert(buffer != nullptr);
  lex->current = buffer;
  lex->lineNumber = 1;
  lex->columnNumber = 1;
  lex->fileID = fileID;
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

inline void EatNChars(Lexer *lex, int n) {
  for (int i = 0; i < n; i++) {
    EatCurrent(lex);
  }
}

inline void AppendNChars(Lexer *lex, Token *token, int n) {
  for (int i = 0; i < n; i++) {
    AppendCurrent(token, lex);
  }
}

bool IsUnaryToken(TokenType tokenType) {
  bool result = (tokenType == TokenType_SymbolAddress) ||
    (tokenType == TokenType_SymbolValue) ||
    (tokenType == TokenType_LogicalNot) ||
    (tokenType == TokenType_BitwiseNot);
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

static void LexHexLiteral(Lexer *lex, Token *token) {
  assert(IsBase16Digit(*lex->current));
  int digitCount = 0;
  while (IsBase16Digit(*lex->current)) {
    digitCount += 1;
    AppendCurrent(token, lex);
  }

  uint64_t value = 0;
  uint64_t scalar = 1;
  for (int i = 1; i < digitCount + 1; i++) {
    int c = (int)*(lex->current - i);
    if (c <= '9') c -= '0';
    else c -= 'A' + 10;
    value += c * scalar;
    scalar *= 16;
  }

  token->type = TokenType_Integer;
  token->unsignedValue = value;
}

static void LexBinaryLiteral(Lexer *lex, Token *token) {
  assert(IsBase2Digit(*lex->current));
  int digitCount = 0;
  while (IsBase2Digit(*lex->current)) {
    digitCount += 1;
    AppendCurrent(token, lex);
  }

  uint64_t value = 0;
  uint64_t scalar = 1;
  for (int i = 1; i < digitCount + 1; i++) {
    int c = (int)*(lex->current - i);
    c -= '0';
    value += c * scalar;
    scalar *= 2;
  }

  token->type = TokenType_Integer;
  token->unsignedValue = value;
}

void LexNumericLiteral(Lexer *lex, Token *token) {
  assert(IsNumeric(*lex->current));
  if (*lex->current == '0') {
    if (lex->current[1] == 'x') {
      AppendNChars(lex, token, 2);
      LexHexLiteral(lex, token);
      return;
    } else if (lex->current[1] == 'b') {
      AppendNChars(lex, token, 2);
      LexBinaryLiteral(lex, token);
      return;
    }
  }

  int digitCount = 0;
  char *firstPart = lex->current;
  while (IsBase10Digit(*lex->current)) {
    AppendCurrent(token, lex);
    digitCount += 1;
  }

  //handle this shit better
  if (*lex->current == '.') {
    int fractionDigitCount = 0;
    AppendCurrent(token, lex);
    char *secondPart = lex->current;
    while (IsBase10Digit(*lex->current)) {
      AppendCurrent(token, lex);
      fractionDigitCount += 1;
    }

    double value = 0.0;
    double scalar = 1.0;
    for (int i = digitCount - 1; i >= 0; i--) {
      int c = (int)(firstPart[i]);
      c -= '0';
      value += (double)c * scalar;
    scalar *= 10.0;
    }

    scalar = 0.1;
    for (int i = 0; i < fractionDigitCount; i++) {
      int c = (int)(secondPart[i]);
      c -= '0';
      value += (double)c * scalar;
      scalar *= 0.1;
    }

    token->type = TokenType_Float;
    token->floatValue = value;

  } else {
    uint64_t value = 0;
    uint64_t scalar = 1;
    for (int i = 1; i < digitCount + 1; i++) {
      int c = (int)*(lex->current - i);
      c -= '0';
      value += c * scalar;
      scalar *= 10;
    }

    token->type = TokenType_Integer;
    token->unsignedValue = value;
  }
}

void LexChar(Lexer *lex, Token *token, size_t writeIndex) {
  if (*lex->current == '\\') {
    AppendCurrent(token, lex);
    if (*lex->current == 'n') {
      token->text[writeIndex] = '\n';
    }
    token->length -= 1;
  } else {
    AppendCurrent(token, lex);
  }
}

//This is dirty AF.  We don't want to allocate
//memory or bother doing a copy so we just replace
//the escaped characters in the buffer as we go
void LexStringLiteral(Lexer *lex, Token *token) {
  assert(*lex->current == '"');
  EatCurrent(lex); //Eat "
  token->type = TokenType_String;
  token->text = lex->current;
  size_t writeIndex = 0;
  while (*lex->current != '"' && *lex->current != 0) {
    LexChar(lex, token, writeIndex);
    writeIndex += 1;
  }

  if (*lex->current == '"') {
    EatCurrent(lex); //Eat "
  }
}

//A realitivly ineffiecant procedure to tokenize the current
//position of the lexer within the buffer.  Does a signfigant
//ineffeicant amount of string comparing for simplicity during
//prototyping phase.
Token GetToken(Lexer *lex) {
  Token token = {};
  token.location.fileID = lex->fileID;

  if (lex->nextIndentLevel > lex->currentIndentLevel) {
    token.type = TokenType_BlockOpen;
    lex->currentIndentLevel += 1;
    token.tokenID = lex->internalTokenCounter++;
    return token;
  } else if (lex->nextIndentLevel < lex->currentIndentLevel) {
    token.type = TokenType_BlockClose;
    lex->currentIndentLevel -= 1;
    token.tokenID = lex->internalTokenCounter++;
    return token;
  }

  //Eat any trailing spaces
  while(*lex->current == ' ') EatCurrent(lex);

  if (IsNewline(lex->current)) {
    EatNewline(lex);
    lex->nextIndentLevel = 0;
    int spaceCount = 0;
    static const uint32_t INDENT_SPACE_COUNT = 2;
    while (*lex->current == ' ') {
      spaceCount++;
      EatCurrent(lex);
      if (spaceCount >= INDENT_SPACE_COUNT) {
        lex->nextIndentLevel += 1;
        spaceCount = 0;
      }
    }

    if (IsNewline(lex->current)) {
      lex->nextIndentLevel = lex->currentIndentLevel;
    }

    return GetToken(lex);


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
    if (SetTokenTypeIfMatch(TokenType_KeywordCast, LiteralAndLength("CAST"))) return token;
    if (SetTokenTypeIfMatch(TokenType_KeywordImport, LiteralAndLength("IMPORT"))) return token;
    token.type = TokenType_Identifier;
    return token;
  }

  //Integer and Floating point contants
  //If the numeric constant is malformed(two decimals, etc) error
  //checking is defered to the parsing stage of the compiler
  else if (IsNumeric(*lex->current)) {
    LexNumericLiteral(lex, &token);
  }

  //String literals
  //TODO Proper string escaping handling and
  //character inserting
  else if (*lex->current == '"') {
    LexStringLiteral(lex, &token);
  }

  else if (*lex->current == '\'') {
    EatCurrent(lex);
    token.type = TokenType_Char;
    token.text = lex->current;
    LexChar(lex, &token, 0);
    if (*lex->current != '\'') {
      assert(false);
    }
    EatCurrent(lex);
    token.unsignedValue = *token.text;
  }

  //Comments
  else if (*lex->current == '#') {
    EatCurrent(lex); // Eat #
    if (*lex->current == '#') {
      EatCurrent(lex);
      while (*lex->current != '#' && *lex->current != '\0') {
        EatCurrent(lex);
      }

      if (*lex->current == '#') {
        EatCurrent(lex);
        if (*lex->current == '#') {
          EatCurrent(lex);
          return GetToken(lex);
        }
      }

      token.type = TokenType_EndOfBuffer;

    } else {
      while(!IsNewline(lex->current) && *lex->current != '\0') {
        EatCurrent(lex);
      }

      if (IsNewline(lex->current)) {
        EatNewline(lex);
        return GetToken(lex);
      } 
    }

    token.type = TokenType_EndOfBuffer;
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

    else if (SetTokenTypeIfMatchAndAppend(TokenType_LogicalEqual, LiteralAndLength("=="))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_LogicalNotEqual, LiteralAndLength("!="))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_LogicalAnd, LiteralAndLength("&&"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_LogicalOr, LiteralAndLength("||"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_LogicalLessOrEqual, LiteralAndLength("<="))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_LogicalGreaterOrEqual, LiteralAndLength(">="))) return token;

  else if (SetTokenTypeIfMatchAndAppend(TokenType_DoubleColon, LiteralAndLength("::"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_SymbolColon, LiteralAndLength(":"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_SymbolEquals, LiteralAndLength("="))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_SymbolDot, LiteralAndLength("."))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_SymbolComma, LiteralAndLength(","))) return token;

  else if (SetTokenTypeIfMatchAndAppend(TokenType_BitwiseLeftShift, LiteralAndLength("<<"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_BitwiseRightShift, LiteralAndLength(">>"))) return token;

  else if (SetTokenTypeIfMatchAndAppend(TokenType_LogicalLessThan, LiteralAndLength("<"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_LogicalGreaterThan, LiteralAndLength(">"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_BitwiseOr, LiteralAndLength("|"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_BitwiseAnd, LiteralAndLength("&"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_BitwiseXor, LiteralAndLength("^"))) return token;

  else if (SetTokenTypeIfMatchAndAppend(TokenType_LogicalNot, LiteralAndLength("!"))) return token;
  else if (SetTokenTypeIfMatchAndAppend(TokenType_BitwiseNot, LiteralAndLength("~"))) return token;
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