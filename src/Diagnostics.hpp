
enum LogLevel {
  LogLevel_None,
  LogLevel_Verbose,
  LogLevel_Info,
};

struct Parser;

//User level logging routines that notify user of errors and warnings
//found while parsing or analyizing the code
void ReportError(Compiler *c, const char *fmt, ...);
void ReportError(Parser *p, const char *fmt, ...);
void ReportWarning(Parser *p, const char *fmt, ...);
void ReportError(Compiler *c, const SourceLocation& location, const char *fmt, ...);
void ReportError(Parser *p, const SourceLocation& location, const char *fmt, ...);
void ReportWarning(Parser *p, const SourceLocation& location, const char *fmt, ...);

//Internal logging routines for developer use and only
void LogInfo(Parser *p, const char *fmt, ...);
void LogVerbose(Parser *p, const char *fmt, ...);

//Debug procedures to pretty print AST represntation of
//parsed data structures to verifiy parsing is correct
void PrintStatement(Statement *statement, int blockDepth);
void PrintBlock(Block *block, int blockDepth = 0);
void PrintTypeDeclaration(TypeDeclaration *typeDecl, int blockDepth = 0);
void PrintProcedureDeclaration(ProcedureDeclaration *procDecl, int blockDepth = 0);
void PrintWhileStatement(WhileStatement *ws, int blockDepth = 0);
void PrintIfStatement(IfStatement *is, int blockDepth = 0);

void PrintVariableDeclaration(VariableDeclaration *varDecl, int blockDepth = 0);
void PrintVariableAssignment(VariableAssignment *varAssignment, int blockDepth = 0);
void PrintCallStatement(CallStatement *callStatement);
void PrintReturnStatement(ReturnStatement *returnStatement, int blockDepth = 0);

void PrintExpression(Expression *expression);
void PrintIntegerLiteral(IntegerLiteral *intLiteral);
void PrintFloatLiteral(FloatLiteral *floatLiteral);
void PrintStringLiteral(StringLiteral *stringLiteral);
void PrintVariableExpression(VariableExpression *varExpr);
void PrintCallExpression(CallExpression *callExpr);
void PrintBinaryOperation(BinaryOperation *binOp);
void PrintMemberAccessExpression(MemberAccessExpression *memberAccessExpr);
void PrintCastExpression(CastExpression *castExpr);