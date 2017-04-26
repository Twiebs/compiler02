
struct LLVMCodegenerator {
  LLVMModuleRef module;
  LLVMBuilderRef builder;
};

void CodegenGlobalBlock(Compiler *compiler, Block *block);
void CodegenStatement(LLVMCodegenerator *cg, Statement *statement);
void CodegenProcedureDeclaration(LLVMCodegenerator *cg, ProcedureDeclaration *procDecl);
LLVMValueRef CodegenExpression(LLVMCodegenerator *cg, Expression *expr);
LLVMTypeRef GetLLVMType(LLVMCodegenerator *cg, VariableDeclaration *varDecl);

void CodegenTypeDeclaration(LLVMCodegenerator *cg, TypeDeclaration *typeDecl);
void CodegenProcedureDeclaration(LLVMCodegenerator *cg, ProcedureDeclaration *procDecl);
void CodegenVariableDeclaration(LLVMCodegenerator *cg, VariableDeclaration *varDecl);
void CodegenVariableAssignment(LLVMCodegenerator *cg, VariableAssignment *varAssignment);
void CodegenCallStatement(LLVMCodegenerator *cg, CallStatement *callStatement);
void CodegenReturnStatement(LLVMCodegenerator *cg, ReturnStatement *returnStatement);

LLVMValueRef CodegenIntegerLiteral(LLVMCodegenerator *cg, IntegerLiteral *intLiteral);
LLVMValueRef CodegenFloatLiteral(LLVMCodegenerator *cg, FloatLiteral *floatLiteral);
LLVMValueRef CodegenStringLiteral(LLVMCodegenerator *cg, StringLiteral *stringLiteral);
LLVMValueRef CodegenCastExpression(LLVMCodegenerator *cg, CastExpression *castExpr);
LLVMValueRef CodgenMemberAccessExpression(LLVMCodegenerator *cg, MemberAccessExpression *memberAccess);
LLVMValueRef CodegenBinaryOperation(LLVMCodegenerator *cg, BinaryOperation *binOp);
LLVMValueRef CodegenVariableExpression(LLVMCodegenerator *cg, VariableExpression *varExpr);
LLVMValueRef CodegenCallExpression(LLVMCodegenerator *cg, CallExpression *expr);
