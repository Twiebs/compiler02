
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DIBuilder.h"

#include <unordered_map>

struct LLVMCodegenerator {
  llvm::LLVMContext *context;
  llvm::Module *module;
  llvm::IRBuilder<> *builder;
  Compiler *compiler;

  ProcedureDeclaration *currentProcedure;
  llvm::Function *currentFunction;

  //Debug Info Creation
  bool emitDebugInfo;
  llvm::DIBuilder *dibuilder;
  llvm::DICompileUnit *debugCompileUnit;
  std::vector<llvm::DIFile *> diFiles;
  llvm::DIScope *currentDebugScope;
  std::unordered_map<char *, llvm::DIType *> diTypeMap;

  //Debug Info helper procedures
  llvm::DIType *GetDIType(TypeInfo *type);

  //Procedures for creating dwarf debug information
  void CreateDebugInfoForProcedure(ProcedureDeclaration *procedure);

};

static void CodegenReturnValuesForCurrentProcedure(LLVMCodegenerator *cg);

void CodegenGlobalBlock(Compiler *compiler, Block *block);
void CodegenStatement(LLVMCodegenerator *cg, Statement *statement);
void CodegenProcedureDeclaration(LLVMCodegenerator *cg, ProcedureDeclaration *procDecl);
llvm::Value *CodegenExpression(LLVMCodegenerator *cg, Expression *expr);
llvm::Type *GetLLVMType(LLVMCodegenerator *cg, TypeInfo *typeInfo);

void CodegenBlock(LLVMCodegenerator *cg, Block *block);

void CodegenTypeDeclaration(LLVMCodegenerator *cg, TypeDeclaration *typeDecl);
void CodegenProcedureDeclaration(LLVMCodegenerator *cg, ProcedureDeclaration *procDecl);
void CodegenVariableDeclaration(LLVMCodegenerator *cg, VariableDeclaration *varDecl);
void CodegenConstantDeclaration(LLVMCodegenerator *cg, ConstantDeclaration *c);

void CodegenVariableAssignment(LLVMCodegenerator *cg, VariableAssignment *varAssignment);
void CodegenCallStatement(LLVMCodegenerator *cg, CallStatement *callStatement);
void CodegenReturnStatement(LLVMCodegenerator *cg, ReturnStatement *returnStatement);

void CodegenWhileStatement(LLVMCodegenerator *cg, WhileStatement *ws);
void CodegenIfStatement(LLVMCodegenerator *cg, IfStatement *is);

llvm::Value *CodegenIntegerLiteral(LLVMCodegenerator *cg, IntegerLiteral *intLiteral);
llvm::Value *CodegenFloatLiteral(LLVMCodegenerator *cg, FloatLiteral *floatLiteral);
llvm::Value *CodegenStringLiteral(LLVMCodegenerator *cg, StringLiteral *stringLiteral);

llvm::Value *CodegenVariableExpression(LLVMCodegenerator *cg, VariableExpression *varExpr);
llvm::Value *CodegenCallExpression(LLVMCodegenerator *cg, CallExpression *expr);
llvm::Value *CodegenConstantExpression(LLVMCodegenerator *cg, ConstantExpression *ce);

llvm::Value *CodegenCastExpression(LLVMCodegenerator *cg, CastExpression *castExpr);
llvm::Value *CodegenUnaryOperation(LLVMCodegenerator *cg, UnaryOperation *unaryOp);
llvm::Value *CodegenBinaryOperation(LLVMCodegenerator *cg, BinaryOperation *binOp);

llvm::Value *CodegenSizeOfExpression(LLVMCodegenerator *cg, SizeOfExpression *expr);