
bool ValidateStatement(Compiler *compiler, Statement *statement);
bool ValidateBlock(Compiler *compiler, Block *block);

bool ValidateVariableDeclaration(Compiler *compiler, VariableDeclaration *varDecl);
bool ValidateProcedureDeclaration(Compiler *compiler, ProcedureDeclaration *procDecl);
bool ValidateConstantDeclaration(Compiler *compiler, ConstantDeclaration *c);
bool ValidateTypeDeclaration(TypeDeclaration *typeDecl);

bool ValidateVariableAssignment(Compiler *compiler, VariableAssignment *varAssign);
bool ValidateCallStatement(Compiler *compiler, CallStatement *call);
bool ValidateReturnStatement(Compiler *compiler, ReturnStatement *returnStatement);

bool ValidateWhileStatement(Compiler *c, WhileStatement *ws);
bool ValidateIfStatement(Compiler *c, IfStatement *is);

//Validate Expressions
bool ValidateExpression(Compiler *compiler, Expression *expr);
void ValidateVariableExpression(Compiler *compiler, VariableExpression *expr);
bool ValidateUnaryOperation(Compiler *compiler, UnaryOperation *unaryOp);
bool ValidateBinaryOperation(Compiler *compiler, BinaryOperation *binOp);
bool ValidateCastExpression(Compiler *compiler, CastExpression *cast);
bool ValidateCallExpression(Compiler *c, CallExpression *call);

//Validation of Non AST Helper structs
void ValidateParameterInvokation(Compiler *compiler, ParameterInvokation *params, SourceLocation& loc);
TypeInfo ValidateVariableAccess(Compiler *compiler, SourceLocation location, VariableAccess *va);

//Internal procedures
bool AttemptTypeCoercionIfRequired(Compiler *compiler, TypeInfo *requestedType, Expression *expr);
