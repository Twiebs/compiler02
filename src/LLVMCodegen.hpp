
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DIBuilder.h"

#include <unordered_map>

struct Backend_LLVM {
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

static void CodegenReturnValuesForCurrentProcedure(Backend_LLVM *cg);

void CodegenGlobalBlock(Compiler *compiler, Block *block);
void CodegenStatement(Backend_LLVM *cg, Statement *statement);
void CodegenProcedureDeclaration(Backend_LLVM *cg, ProcedureDeclaration *procDecl);
llvm::Value *CodegenExpression(Backend_LLVM *cg, Expression *expr);
llvm::Type *GetLLVMType(Backend_LLVM *cg, TypeInfo *typeInfo);

void CodegenBlock(Backend_LLVM *cg, Block *block);

void CodegenTypeDeclaration(Backend_LLVM *cg, TypeDeclaration *typeDecl);
void CodegenProcedureDeclaration(Backend_LLVM *cg, ProcedureDeclaration *procDecl);
void CodegenVariableDeclaration(Backend_LLVM *cg, VariableDeclaration *varDecl);
void CodegenConstantDeclaration(Backend_LLVM *cg, ConstantDeclaration *c);

void CodegenVariableAssignment(Backend_LLVM *cg, VariableAssignment *varAssignment);
void CodegenCallStatement(Backend_LLVM *cg, CallStatement *callStatement);
void CodegenReturnStatement(Backend_LLVM *cg, ReturnStatement *returnStatement);

void CodegenWhileStatement(Backend_LLVM *cg, WhileStatement *ws);
void CodegenIfStatement(Backend_LLVM *cg, IfStatement *is);

llvm::Value *CodegenIntegerLiteral(Backend_LLVM *cg, IntegerLiteral *intLiteral);
llvm::Value *CodegenFloatLiteral(Backend_LLVM *cg, FloatLiteral *floatLiteral);
llvm::Value *CodegenStringLiteral(Backend_LLVM *cg, StringLiteral *stringLiteral);

llvm::Value *CodegenVariableExpression(Backend_LLVM *cg, VariableExpression *varExpr);
llvm::Value *CodegenCallExpression(Backend_LLVM *cg, CallExpression *expr);
llvm::Value *CodegenConstantExpression(Backend_LLVM *cg, ConstantExpression *ce);

llvm::Value *CodegenCastExpression(Backend_LLVM *cg, CastExpression *castExpr);
llvm::Value *CodegenUnaryOperation(Backend_LLVM *cg, UnaryOperation *unaryOp);
llvm::Value *CodegenBinaryOperation(Backend_LLVM *cg, BinaryOperation *binOp);

llvm::Value *CodegenSizeOfExpression(Backend_LLVM *cg, SizeOfExpression *expr);

//Debgug Info generator
void InitalizeCompileUnitAndFiles(Backend_LLVM *llvmBE);