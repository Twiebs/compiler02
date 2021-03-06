
struct Parser {
  Token token;
  Lexer lexer;

  PersistantBlockAllocator *astAllocator;
  PersistantBlockAllocator *stringAllocator;

  uint16_t fileID;

  LogLevel logLevel;
  uint32_t internalStatementCounter;
  uint32_t internalExpressionCounter;

  Block *currentBlock;
  Compiler *compiler;
};

void NextToken(Parser *p);

//Will seek upwards through the AST block hiearchy untill it finds an
//identifier matching the provided string. If no match is found null is returned
Identifier *FindIdentifier(Block *block, Token token);

#define CreateStatement(type, location, parser) (type *)CreateStatementInternal(parser, StatementType_##type, location, sizeof(type))
#define CreateExpression(type, location, parser) (type *)CreateExpressionInternal(parser, ExpressionType_##type, location, sizeof(type))
Expression *CreateExpressionInternal(Parser *p, ExpressionType type, SourceLocation location, size_t size);
Statement *CreateStatementInternal(Parser *p, StatementType type, SourceLocation location, size_t size);
Identifier *CreateIdentifier(Parser *p, Token identToken);
TypeDeclaration *CreateBuiltinType(Parser *p, SourceLocation location, const char *name);

Expression *ParsePrimaryExpression(Parser *p);


bool ParseVariableAccess(Parser *p, VariableAccess *memberAccess, TypeInfo *outTypeInfo);


Statement *ParseStatement(Parser *p);
IfStatement *ParseIfStatement(Parser *p);

//void ParseBlock(Block *block);
bool ParseTypeInfo(Parser *p, TypeInfo *typeInfo);

Expression *ParseExpression(Parser *p);

bool ParseEntireFile(Compiler *compiler, uint32_t fileID);
bool ParseAllFiles(Compiler *compiler);

