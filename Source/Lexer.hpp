
enum TokenType {
  TokenType_Invalid,

  TokenType_Identifier,
  TokenType_Number,
  TokenType_String,

  TokenType_ParenOpen,
  TokenType_ParenClose,
  TokenType_BlockOpen,
  TokenType_BlockClose,
  TokenType_ArrayOpen,
  TokenType_ArrayClose,

  TokenType_SymbolAdd,
  TokenType_SymbolSub,
  TokenType_SymbolMul,
  TokenType_SymbolDiv,
  TokenType_SymbolMod,

  TokenType_SymbolEquals,
  TokenType_SymbolColon,
  TokenType_SymbolDot,

  TokenType_SymbolLessThan,
  TokenType_SymbolGreaterThan,
  TokenType_SymbolOr,
  TokenType_SymbolAnd,

  TokenType_SymbolNot,
  TokenType_SymbolInvert,
  TokenType_SymbolAddress,
  TokenType_SymbolValue,

  TokenType_KeywordType,
  TokenType_KeywordIf,
  TokenType_KeywordElse,
  TokenType_KeywordWhile,
  TokenType_KeywordReturn,
  TokenType_KeywordForeign,

  TokenType_EndOfBuffer
};

const char *TokenName[] = {
  "TokenType_Invalid",

  "TokenType_Identifier",
  "TokenType_Number",
  "TokenType_String",

  "TokenType_ParenOpen",
  "TokenType_ParenClose",
  "TokenType_BlockOpen",
  "TokenType_BlockClose",
  "TokenType_ArrayOpen",
  "TokenType_ArrayClose",

  "TokenType_SymbolAdd",
  "TokenType_SymbolSub",
  "TokenType_SymbolMul",
  "TokenType_SymbolDiv",
  "TokenType_SymbolMod",

  "TokenType_SymbolEquals",
  "TokenType_SymbolColon",
  "TokenType_SymbolDot",

  "TokenType_SymbolLessThan",
  "TokenType_SymbolGreaterThan",
  "TokenType_SymbolOr",
  "TokenType_SymbolAnd",

  "TokenType_SymbolNot",
  "TokenType_SymbolInvert",
  "TokenType_SymbolAddress",
  "TokenType_SymbolValue",

  "TokenType_KeywordType",
  "TokenType_KeywordIf",
  "TokenType_KeywordElse",
  "TokenType_KeywordWhile",
  "TokenType_KeywordReturn",
  "TokenType_KeywordForeign",

  "TokenType_EndOfBuffer"
};

const char *TokenString[] = {
  "INVALID_STRING_INVALID_TOKEN",

  "INVALID_STRING_IDENTIFIER",
  "INVALID_STRING_NUMBER",
  "INVALID_STRING_STRING",

  "(",
  ")",
  "{",
  "}",
  "[",
  "]",

  "+",
  "-",
  "*",
  "/",
  "%",

  "=",
  ":",
  ".",

  "<",
  ">",
  "|",
  "&",

  "!",
  "~",
  "@",
  "$",

  "TYPE",
  "IF",
  "ELSE",
  "WHILE",
  "RETURN",
  "FOREIGN",

  "INVALID_STRING_END_OF_BUFFER"
};

int TokenPrecedence[] = {
  -1, //TokenType_Invalid

  -1, //TokenType_Identifier
  -1, //TokenType_Number
  -1, //TokenType_String

  -1, //TokenType_ParenOpen
  -1, //TokenType_ParenClose
  -1, //TokenType_BlockOpen
  -1, //TokenType_BlockClose
  -1, //TokenType_ArrayOpen
  -1, //TokenType_ArrayClose

  20, //TokenType_SymbolAdd
  20, //TokenType_SymbolSub
  40, //TokenType_SymbolMul
  40, //TokenType_SymbolDiv
  5, //TokenType_SymbolMod

  -1, //TokenType_SymbolEquals
  -1, //TokenType_SymbolColon
  -1, //TokenType_SymbolDot

  10, //TokenType_SymbolLessThan
  10, //TokenType_SymbolGreaterThan
  10, //TokenType_SymbolOr
  10, //TokenType_SymbolAnd

  -1, //TokenType_SymbolNot
  -1, //TokenType_SymbolInvert
  -1, //TokenType_SymbolAddress
  -1, //TokenType_SymbolValue

  -1, //TokenType_KeywordType
  -1, //TokenType_KeywordIf
  -1, //TokenType_KeywordElse
  -1, //TokenType_KeywordWhile
  -1, //TokenType_KeywordReturn
  -1, //TokenType_KeywordForeign

  -1, //TokenType_EndOfBuffer
};

struct Token {
  TokenType type;
  uint32_t tokenID;
  SourceLocation location;  
  char *text;
  size_t length;
};

bool IsSymbolicUnaryToken(TokenType tokenType);
bool IsArithmeticToken(TokenType tokenType);

struct Lexer {
  char *current;
  uint32_t lineNumber;
  uint32_t columnNumber;
  uint32_t fileID;
  //This is used for debuging purposes to set conditional
  //breakpoints based on the tokenID that ticks up everytime
  //a new toke is lexed
  uint32_t internalTokenCounter;
  int currentIndentLevel;
};

void InitalizeLexer(Lexer *lex, uint32_t fileID, char *buffer);

//A realitivly ineffiecant procedure to tokenize the current
//position of the lexer within the buffer.  Does a signfigant
//ineffeicant amount of string comparing for simplicity during
//prototyping phase.
Token GetToken(Lexer *lex);
