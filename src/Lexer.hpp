
enum TokenType {
  TokenType_Invalid,

  TokenType_Identifier,
  TokenType_Integer,
  TokenType_Float,
  TokenType_String,
  TokenType_Char,

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
  TokenType_SymbolComma,
  TokenType_DoubleColon,

  TokenType_LogicalEqual,
  TokenType_LogicalNotEqual,
  TokenType_LogicalAnd,
  TokenType_LogicalOr,
  TokenType_LogicalLessThan,
  TokenType_LogicalGreaterThan,
  TokenType_LogicalLessOrEqual, 
  TokenType_LogicalGreaterOrEqual,

  TokenType_BitwiseOr,
  TokenType_BitwiseAnd,
  TokenType_BitwiseXor,
  TokenType_BitwiseLeftShift,
  TokenType_BitwiseRightShift,

  TokenType_BitwiseNot,
  TokenType_LogicalNot,
  TokenType_SymbolAddress,
  TokenType_SymbolValue,

  TokenType_KeywordType,
  TokenType_KeywordIf,
  TokenType_KeywordElse,
  TokenType_KeywordWhile,
  TokenType_KeywordReturn,
  TokenType_KeywordForeign,
  TokenType_KeywordCast,
  TokenType_KeywordImport,
  TokenType_KeywordSizeOf,

  TokenType_EndOfBuffer
};

const char *TokenName[] = {
  "TokenType_Invalid",

  "TokenType_Identifier",
  "TokenType_Integer",
  "TokenType_Float",
  "TokenType_String",
  "TokenType_Char",

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
  "TokenType_SymbolComma",
  "TokenType_DoubleColon",

  "TokenType_LogicalEqual",
  "TokenType_LogicalNotEqual",
  "TokenType_LogicalAnd",
  "TokenType_LogicalOr",
  "TokenType_LogicalLessThan",
  "TokenType_LogicalGreaterThan",
  "TokenType_LogicalLessOrEqual", 
  "TokenType_LogicalGreaterOrEqual",

  "TokenType_BitwiseOr",
  "TokenType_BitwiseAnd",
  "TokenType_BitwiseXor",
  "TokenType_BitwiseLeftShift",
  "TokenType_BitwiseRightShift",

  "TokenType_BitwiseNot",
  "TokenType_LogicalNot",
  "TokenType_SymbolAddress",
  "TokenType_SymbolValue",

  "TokenType_KeywordType",
  "TokenType_KeywordIf",
  "TokenType_KeywordElse",
  "TokenType_KeywordWhile",
  "TokenType_KeywordReturn",
  "TokenType_KeywordForeign",
  "TokenType_KeywordCast",
  "TokenType_KeywordImport",
  "TokenType_KeywordSizeOf",


  "TokenType_EndOfBuffer"
};

const char *TokenString[] = {
  "INVALID_STRING_INVALID_TOKEN",

  "INVALID_STRING_IDENTIFIER",
  "INVALID_STRING_INTEGER",
  "INVALID_STRING_FLOAT",
  "INVALID_STRING_STRING",
  "INVALID_STRING_CHAR",

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
  ",",
  "::",

  "==", //TokenType_LogicalEqual,
  "!=", //TokenType_LogicalNotEqual,
  "&&", //TokenType_LogicalAnd,
  "||", //TokenType_LogicalOr,
  "<", //TokenType_LogicalLessThan,
  ">", //TokenType_LogicalGreaterThan,
  "<=", //TokenType_LogicalLessOrEqual, 
  ">=", //TokenType_LogicalGreaterOrEqual,

  "|", //BitwiseOr
  "&", //BitwiseAnd
  "^", // TokenType_BitwiseXor,
  "<<", // TokenType_BitwiseLeftShift,
  ">>", // TokenType_BitwiseRightShift,

  "~", //BitwiseNot
  "!",

  "@",
  "$",

  "TYPE",
  "IF",
  "ELSE",
  "WHILE",
  "RETURN",
  "FOREIGN",
  "CAST",
  "IMPORT",
  "SIZEOF",

  "INVALID_STRING_END_OF_BUFFER"
};

int TokenPrecedence[] = {
  -1, //TokenType_Invalid

  -1, //TokenType_Identifier
  -1, //TokenType_Integer
  -1, //TokenType_Float
  -1, //TokenType_String
  -1, //TokenType_Char

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
  -1, //TokenType_SymbolComma
  -1, //TokenType_DoubleColon

  15, //TokenType_LogicalEqual,
  15, //TokenType_LogicalNotEqual,
  10, //TokenType_LogicalAnd,
  10, //TokenType_LogicalOr,
  15, //TokenType_LogicalLessThan,
  15, //TokenType_LogicalGreaterThan,
  15, //TokenType_LogicalLessOrEqual, 
  15, //TokenType_LogicalGreaterOrEqual,

  10, //TokenType_SymbolOr
  10, //TokenType_BitwiseAnd
  10, //TokenType_BitwiseXor,
  10, //TokenType_BitwiseLeftShift,
  10, //TokenType_BitwiseRightShift,

  -1, //TokenType_BitwiseNot
  -1, //TokenType_LogicalNot
  -1, //TokenType_SymbolAddress
  -1, //TokenType_SymbolValue

  -1, //TokenType_KeywordType
  -1, //TokenType_KeywordIf
  -1, //TokenType_KeywordElse
  -1, //TokenType_KeywordWhile
  -1, //TokenType_KeywordReturn
  -1, //TokenType_KeywordForeign
  -1, //TokenType_KeywordCast
  -1, //TokenType_KeywordImport
  -1, //TokenType_KeywordSizeOf

  -1, //TokenType_EndOfBuffer
};

struct Token {
  TokenType type;
  uint32_t tokenID;
  SourceLocation location;  
  char *text;
  size_t length;

  uint64_t unsignedValue;
  double floatValue;
};

bool IsSymbolicUnaryToken(TokenType tokenType);
bool IsArithmeticToken(TokenType tokenType);

struct Lexer {
  char *current;
  int currentIndentLevel;
  int nextIndentLevel;
  uint32_t lineNumber;
  uint32_t columnNumber;
  uint32_t fileID;
  //This is used for debuging purposes to set conditional
  //breakpoints based on the tokenID that ticks up everytime
  //a new toke is lexed
  uint32_t internalTokenCounter;
};

void InitalizeLexer(Lexer *lex, uint32_t fileID, char *buffer);

//A realitivly ineffiecant procedure to tokenize the current
//position of the lexer within the buffer.  Does a signfigant
//ineffeicant amount of string comparing for simplicity during
//prototyping phase.
Token GetToken(Lexer *lex);
