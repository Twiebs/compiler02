
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetMachine.h"

struct LLVMCodegenerator {
  llvm::LLVMContext *context;
  llvm::Module *module;
  llvm::IRBuilder<> *builder;
  Compiler *compiler;
  llvm::Function *currentFunction;
};

void CodegenGlobalBlock(Compiler *compiler, Block *block);
void CodegenStatement(LLVMCodegenerator *cg, Statement *statement);
void CodegenProcedureDeclaration(LLVMCodegenerator *cg, ProcedureDeclaration *procDecl);
llvm::Value *CodegenExpression(LLVMCodegenerator *cg, Expression *expr);
llvm::Type *GetLLVMType(LLVMCodegenerator *cg, TypeInfo *typeInfo);

void CodegenBlock(LLVMCodegenerator *cg, Block *block);

void CodegenTypeDeclaration(LLVMCodegenerator *cg, TypeDeclaration *typeDecl);
void CodegenProcedureDeclaration(LLVMCodegenerator *cg, ProcedureDeclaration *procDecl);
void CodegenVariableDeclaration(LLVMCodegenerator *cg, VariableDeclaration *varDecl);
void CodegenVariableAssignment(LLVMCodegenerator *cg, VariableAssignment *varAssignment);
void CodegenCallStatement(LLVMCodegenerator *cg, CallStatement *callStatement);
void CodegenReturnStatement(LLVMCodegenerator *cg, ReturnStatement *returnStatement);

void CodegenWhileStatement(LLVMCodegenerator *cg, WhileStatement *ws);
void CodegenIfStatement(LLVMCodegenerator *cg, IfStatement *is);

llvm::Value *CodegenIntegerLiteral(LLVMCodegenerator *cg, IntegerLiteral *intLiteral);
llvm::Value *CodegenFloatLiteral(LLVMCodegenerator *cg, FloatLiteral *floatLiteral);
llvm::Value *CodegenStringLiteral(LLVMCodegenerator *cg, StringLiteral *stringLiteral);
llvm::Value *CodegenCastExpression(LLVMCodegenerator *cg, CastExpression *castExpr);
llvm::Value *CodgenMemberAccessExpression(LLVMCodegenerator *cg, MemberAccessExpression *memberAccess);
llvm::Value *CodegenUnaryOperation(LLVMCodegenerator *cg, UnaryOperation *unaryOp);
llvm::Value *CodegenBinaryOperation(LLVMCodegenerator *cg, BinaryOperation *binOp);
llvm::Value *CodegenVariableExpression(LLVMCodegenerator *cg, VariableExpression *varExpr);
llvm::Value *CodegenCallExpression(LLVMCodegenerator *cg, CallExpression *expr);