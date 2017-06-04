//This backend does a literal translation into a readable and pretty formatted
//C source code file.  Relies only on the inclusion of stdint.h and stddef.h

//This is the main public API call that should be used
void CodeGenEntireAST(Compiler *c, std::ostream& stream);

//Abstract codegen dispatch
void CodeGenStatement(Statement *statement, std::ostream& stream, int blockDepth);
void CodeGenExpression(Expression *expression, std::ostream& stream);

//Helper Printers
void CodeGenBlockDepthSpaces(int blockDepth, std::ostream& stream);
void CodeGenTypeNameAndPointers(TypeInfo *typeInfo, std::ostream& stream);
void CodeGenTypeArraySize(TypeInfo *typeInfo, std::ostream& stream);
void CodeGenParameterDeclaration(ParameterDeclaration *params, std::ostream& stream);
void CodeGenParameterInvokation(ParameterInvokation *params, std::ostream& stream);
void CodeGenVariableAccess(VariableAccess *va, std::ostream& stream);

//Compound Statements
void CodeGenBlock(Block *block, std::ostream& stream, int blockDepth);
void CodeGenTypeDeclaration(TypeDeclaration *typeDecl, std::ostream& stream, int blockDepth);
void CodeGenProcedureDeclaration(ProcedureDeclaration *procDecl, std::ostream& stream, int blockDepth);
void CodeGenWhileStatement(WhileStatement *ws, std::ostream& stream, int blockDepth);
void CodeGenIfStatement(IfStatement *is, std::ostream& stream, int blockDepth);

//Single Statements
void CodeGenVariableDeclaration(VariableDeclaration *varDecl, std::ostream& s);
void CodeGenConstantDeclaration(ConstantDeclaration *c, std::ostream& s);
void CodeGenVariableAssignment(VariableAssignment *varAssignment, std::ostream& s);
void CodeGenCallStatement(CallStatement *callStatement, std::ostream& s);
void CodeGenReturnStatement(ReturnStatement *returnStatement, std::ostream& s);

//Expressions
void CodeGenIntegerLiteral(IntegerLiteral *intLiteral, std::ostream& s);
void CodeGenFloatLiteral(FloatLiteral *floatLiteral, std::ostream& s);
void CodeGenStringLiteral(StringLiteral *stringLiteral, std::ostream& s);
void CodeGenVariableExpression(VariableExpression *varExpr, std::ostream& s);
void CodeGenConstantExpression(ConstantExpression *ce, std::ostream& s);
void CodeGenCallExpression(CallExpression *callExpr, std::ostream& s);
void CodeGenBinaryOperation(BinaryOperation *binOp, std::ostream& s);
void CodeGenUnaryOperation(UnaryOperation *unaryOp, std::ostream& s);
void CodeGenCastExpression(CastExpression *castExpr, std::ostream& s);
void CodeGenSizeOfExpression(SizeOfExpression *expression, std::ostream& stream);