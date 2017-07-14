#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_os_ostream.h"


void CodegenStatement(Backend_LLVM *cg, Statement *statement) {
  switch (statement->statementType) {
    case StatementType_TypeDeclaration: CodegenTypeDeclaration(cg, (TypeDeclaration *)statement); break;
    case StatementType_ProcedureDeclaration: CodegenProcedureDeclaration(cg, (ProcedureDeclaration *)statement); break;
    case StatementType_VariableDeclaration: CodegenVariableDeclaration(cg, (VariableDeclaration *)statement); break;
    case StatementType_ConstantDeclaration: CodegenConstantDeclaration(cg, (ConstantDeclaration *)statement); break;

    case StatementType_VariableAssignment: CodegenVariableAssignment(cg, (VariableAssignment *)statement); break;
    case StatementType_ReturnStatement: CodegenReturnStatement(cg, (ReturnStatement *)statement); break;
    case StatementType_CallStatement: CodegenCallStatement(cg, (CallStatement *)statement); break;

    case StatementType_WhileStatement: CodegenWhileStatement(cg, (WhileStatement *)statement); break;
    case StatementType_IfStatement: CodegenIfStatement(cg, (IfStatement *)statement); break;

    default: assert(false); break;
  };
}

llvm::Value *CodegenExpression(Backend_LLVM *cg, Expression *expr) {
  switch (expr->expressionType) {
    case ExpressionType_IntegerLiteral: return CodegenIntegerLiteral(cg, (IntegerLiteral *)expr);
    case ExpressionType_FloatLiteral:  return CodegenFloatLiteral(cg, (FloatLiteral *)expr);
    case ExpressionType_StringLiteral: return CodegenStringLiteral(cg, (StringLiteral *)expr);
    case ExpressionType_CastExpression: return CodegenCastExpression(cg, (CastExpression *)expr);
    case ExpressionType_ConstantExpression: return CodegenConstantExpression(cg, (ConstantExpression *)expr);

    case ExpressionType_UnaryOperation: return CodegenUnaryOperation(cg, (UnaryOperation *)expr);
    case ExpressionType_BinaryOperation: return CodegenBinaryOperation(cg, (BinaryOperation *)expr);
    case ExpressionType_VariableExpression: return CodegenVariableExpression(cg, (VariableExpression *)expr);
    case ExpressionType_CallExpression: return CodegenCallExpression(cg, (CallExpression *)expr);

    case ExpressionType_SizeOfExpression: return CodegenSizeOfExpression(cg, (SizeOfExpression *)expr);
    default: assert(false); break;
  };

  assert(false);
  return nullptr;
}

llvm::Value *CodegenBranchCondition(Backend_LLVM *cg, Expression *e) {
  llvm::Value *condExpr = CodegenExpression(cg, e);
  if (e->expressionType == ExpressionType_IntegerLiteral) {
    llvm::Type *destType = cg->builder->getInt1Ty();
    llvm::Value *result = cg->builder->CreateIntCast(condExpr, destType, false);
    return result;
  }
  return condExpr;
}

void CodegenIfStatement(Backend_LLVM *cg, IfStatement *is) {
  llvm::BasicBlock *condBlock = llvm::BasicBlock::Create(*cg->context, "if.condition", cg->currentFunction);
  llvm::BasicBlock *bodyBlock = llvm::BasicBlock::Create(*cg->context, "if.body", cg->currentFunction);
  llvm::BasicBlock *exitBlock = llvm::BasicBlock::Create(*cg->context, "merge", cg->currentFunction);
  cg->builder->CreateBr(condBlock);

  cg->builder->SetInsertPoint(condBlock);
  ElseStatement *currentElse = is->elseStatement;
  llvm::BasicBlock *nextBlock = exitBlock;
  if (currentElse != nullptr)
    nextBlock = llvm::BasicBlock::Create(*cg->context, "else.entry", cg->currentFunction);

  llvm::Value *condExpr = CodegenBranchCondition(cg, is->condition);
  cg->builder->CreateCondBr(condExpr, bodyBlock, nextBlock);

  cg->builder->SetInsertPoint(bodyBlock);
  CodegenBlock(cg, is);
  if (is->lastStatement->statementType != StatementType_ReturnStatement)
    cg->builder->CreateBr(exitBlock);

  {
    llvm::BasicBlock *currentCond = nextBlock;
    while (currentElse != nullptr) {
      llvm::BasicBlock *nextExit = exitBlock;
      llvm::BasicBlock *body = llvm::BasicBlock::Create(*cg->context, "else.body", cg->currentFunction);
      cg->builder->SetInsertPoint(currentCond);
      if (currentElse->condition != nullptr) {
        llvm::Value *condExpr = CodegenBranchCondition(cg, currentElse->condition);
        if (currentElse->nextElse != nullptr) {
          nextExit = llvm::BasicBlock::Create(*cg->context, "else.cond", cg->currentFunction);
        }
        cg->builder->CreateCondBr(condExpr, body, nextExit);
      } else {
        cg->builder->CreateBr(body);
      }

      cg->builder->SetInsertPoint(body);
      CodegenBlock(cg, currentElse);
      if (currentElse->lastStatement->statementType != StatementType_ReturnStatement)
        cg->builder->CreateBr(exitBlock);
      currentElse = currentElse->nextElse;
      currentCond = nextExit;
    }
  }
  
  cg->builder->SetInsertPoint(exitBlock);
}

void CodegenWhileStatement(Backend_LLVM *cg, WhileStatement *ws) {
  llvm::BasicBlock *condBlock = llvm::BasicBlock::Create(*cg->context, "while.condition", cg->currentFunction);
  llvm::BasicBlock *bodyBlock = llvm::BasicBlock::Create(*cg->context, "while.body", cg->currentFunction);
  llvm::BasicBlock *exitBlock = llvm::BasicBlock::Create(*cg->context, "while.exit", cg->currentFunction);

  //branch from previous block
  cg->builder->CreateBr(condBlock);

  cg->builder->SetInsertPoint(condBlock);
  llvm::Value *condExpr = CodegenExpression(cg, ws->condition);
  //llvm::Value *load = cg->builder->CreateLoad(condExpr, "cond.load");
  cg->builder->CreateCondBr(condExpr, bodyBlock, exitBlock);

  cg->builder->SetInsertPoint(bodyBlock);
  CodegenBlock(cg, ws);
  cg->builder->CreateBr(condBlock);

  cg->builder->SetInsertPoint(exitBlock);
}

void CodegenBlock(Backend_LLVM *cg, Block *block) {
  Statement *currentStatement = block->firstStatement;
  while (currentStatement != nullptr) {
    CodegenStatement(cg, currentStatement);
    currentStatement = currentStatement->next;
  }
}

bool WriteNativeObjectToFile(Compiler *compiler, llvm::Module *module, const char *file) {
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmPrinters();
  llvm::InitializeAllAsmParsers();

	llvm::PassRegistry* registry = llvm::PassRegistry::getPassRegistry();
	llvm::initializeCore(*registry);
	llvm::initializeCodeGen(*registry);
	llvm::initializeLowerIntrinsicsPass(*registry);
	llvm::initializeLoopStrengthReducePass(*registry);

  llvm::Triple triple;
  triple.setTriple(llvm::sys::getDefaultTargetTriple());
  std::string error;

  const llvm::Target *target = llvm::TargetRegistry::lookupTarget(triple.normalize(), error);
  llvm::TargetOptions options = {};
  llvm::CodeGenOpt::Level optimizationLevel = llvm::CodeGenOpt::Default;
  //LLVM uses its own namespace in CommandFlags.h so we cant use these!
  //std::string cpuStr = llvm::getCPUStr();
  //std::string featureStr = llvm::getFeatureStr();
  llvm::StringRef cpuStr = llvm::sys::getHostCPUName();
  std::string featureStr = "";
  llvm::Reloc::Model relocModel = llvm::Reloc::PIC_;
  llvm::CodeModel::Model codeModel = llvm::CodeModel::Default;
  llvm::TargetMachine *tm = target->createTargetMachine(triple.getTriple(), 
    cpuStr, featureStr, options, relocModel, codeModel, optimizationLevel);

  llvm::legacy::PassManager passManager;
  llvm::PassManagerBuilder passBuilder;
  passBuilder.OptLevel = 2;
  passBuilder.SizeLevel = 0;
  passBuilder.populateModulePassManager(passManager);

  module->setDataLayout(tm->createDataLayout());
  module->setTargetTriple(triple.getTriple());
  llvm::TargetMachine::CodeGenFileType fileType = llvm::TargetMachine::CGFT_ObjectFile;
  // TargetMachine::CGFT_AssemblyFile

  std::error_code errorCode;
  llvm::sys::fs::OpenFlags flags = (llvm::sys::fs::OpenFlags)0;
  llvm::raw_fd_ostream llvmStream(file, errorCode, flags);
  tm->addPassesToEmitFile(passManager, llvmStream, fileType);
  passManager.run(*module);
  llvmStream.flush();
  return true;
}

void CodegenGlobalBlock(Compiler *compiler, Block *block) {
  Backend_LLVM llvmCG = {};
  llvmCG.context = new llvm::LLVMContext();
  llvmCG.module = new llvm::Module("Compiler", *llvmCG.context);
  llvmCG.builder = new llvm::IRBuilder<>(*llvmCG.context);
  llvmCG.dibuilder = new llvm::DIBuilder(*llvmCG.module);
  llvmCG.compiler = compiler;

  compiler->typeDeclU8->llvmType = llvm::Type::getInt8Ty(*llvmCG.context);
  compiler->typeDeclU16->llvmType = llvm::Type::getInt16Ty(*llvmCG.context);
  compiler->typeDeclU32->llvmType = llvm::Type::getInt32Ty(*llvmCG.context);
  compiler->typeDeclU64->llvmType = llvm::Type::getInt64Ty(*llvmCG.context);
  compiler->typeDeclS8->llvmType = llvm::Type::getInt8Ty(*llvmCG.context);
  compiler->typeDeclS16->llvmType = llvm::Type::getInt16Ty(*llvmCG.context);
  compiler->typeDeclS32->llvmType = llvm::Type::getInt32Ty(*llvmCG.context);
  compiler->typeDeclS64->llvmType = llvm::Type::getInt64Ty(*llvmCG.context);
  compiler->typeDeclF32->llvmType = llvm::Type::getFloatTy(*llvmCG.context);
  compiler->typeDeclF64->llvmType = llvm::Type::getDoubleTy(*llvmCG.context);

  InitalizeCompileUnitAndFiles(&llvmCG);

  CodegenBlock(&llvmCG, block);

  llvm::raw_os_ostream stdoutStream(std::cout);
  TemporaryString s("%s%s.o", compiler->settings.outputDirectory.c_str(), compiler->settings.projectName.c_str());
  if (llvm::verifyModule(*llvmCG.module, &stdoutStream) == false) {
    WriteNativeObjectToFile(compiler, llvmCG.module, s.getString());
    s.set("objdump -M intel -d %s%s.o > %s%s.asm",
      compiler->settings.outputDirectory.c_str(), compiler->settings.projectName.c_str(),
      compiler->settings.outputDirectory.c_str(), compiler->settings.projectName.c_str());
    system(s.getString());

    s.set("clang -O0 -g -lSDL2 -lm %s%s.o -o %s%s", 
      compiler->settings.outputDirectory.c_str(), compiler->settings.projectName.c_str(), 
      compiler->settings.outputDirectory.c_str(), compiler->settings.projectName.c_str());
    system(s.getString());
  } else {
    ReportInternalError(compiler, "\n\nFAILED TO VERIFY LLVM MODULE");
  }

  {
    s.set("%s%s.ll", compiler->settings.outputDirectory.c_str(), compiler->settings.projectName.c_str());
    std::ofstream stream(s.getString());
    llvm::raw_os_ostream llvmStream(stream);
    llvmCG.module->print(llvmStream, nullptr);
  }
}

void CodegenTypeDeclaration(Backend_LLVM *cg, TypeDeclaration *typeDecl) {
  if (typeDecl->statementCount > 0) {
    llvm::Type **memberTypeRefs = (llvm::Type **)alloca(sizeof(llvm::Type *) * typeDecl->statementCount);
    VariableDeclaration *currentVarDecl = (VariableDeclaration *)typeDecl->firstStatement;
    size_t currentVarIndex = 0;
    while (currentVarDecl != nullptr) {
      assert(currentVarDecl->typeInfo.type->llvmType != nullptr);
      memberTypeRefs[currentVarIndex++] = GetLLVMType(cg, &currentVarDecl->typeInfo);
      currentVarDecl = (VariableDeclaration *)currentVarDecl->next; 
    }

    llvm::ArrayRef<llvm::Type *> arrayRef(memberTypeRefs, typeDecl->statementCount);
    llvm::Type *type = llvm::StructType::create(*cg->context, arrayRef, typeDecl->identifier->name.string, false);
    typeDecl->llvmType = type;
  }
}

llvm::Type *GetLLVMType(Backend_LLVM *cg, TypeInfo *typeInfo) {
  assert(typeInfo->type->llvmType != nullptr);
  llvm::Type *currentType = typeInfo->type->llvmType;
  if (typeInfo->indirectionLevel > 0) {
    for (int i = 0; i < typeInfo->indirectionLevel; i++)
      currentType = llvm::PointerType::get(currentType, 0);
  } else if (typeInfo->arraySize > 0) {
    currentType = llvm::ArrayType::get(currentType, typeInfo->arraySize);
  }
  return currentType;
}


static void GenerateLinearizedAllocasForBlock(Backend_LLVM *cg, Block *block) {
  assert(block->statementType != StatementType_TypeDeclaration);
  Statement *current = block->firstStatement;
  while (current != nullptr) {
    if (current->statementType == StatementType_VariableDeclaration) {
      VariableDeclaration *varDecl = (VariableDeclaration *)current;
      llvm::Type *llvmType = GetLLVMType(cg, &varDecl->typeInfo);
      llvm::Value *arraySize = nullptr;
      TempStringBuilder sb;
      sb.append(varDecl->identifier->name.string);
      sb.append(".stack_addr");
      varDecl->llvmAlloca = cg->builder->CreateAlloca(llvmType, arraySize, sb.get());

    } else if (current->statementType == StatementType_WhileStatement) {
      GenerateLinearizedAllocasForBlock(cg, (Block *)current);
    } else if (current->statementType == StatementType_IfStatement) {
      IfStatement *is = (IfStatement *)current;
      GenerateLinearizedAllocasForBlock(cg, (Block *)current);
      if (is->elseStatement != nullptr) {
        ElseStatement *es = is->elseStatement;
        while (es != nullptr) {
          GenerateLinearizedAllocasForBlock(cg, es);
          es = es->nextElse;
        }
      }
    }
    current = current->next;
  }
}

void CodegenProcedureDeclaration(Backend_LLVM *cg, ProcedureDeclaration *procDecl) {
  llvm::Type **argumentTypes = (llvm::Type **)alloca(sizeof(llvm::Type *) * procDecl->inputParameters.parameterCount);
  VariableDeclaration *currentDecl = procDecl->inputParameters.firstParameter;
  size_t currentArgumentIndex = 0;
  while (currentDecl != nullptr) {
    assert(currentDecl->statementType == StatementType_VariableDeclaration);
    argumentTypes[currentArgumentIndex++] = GetLLVMType(cg, &currentDecl->typeInfo);
    currentDecl = (VariableDeclaration *)currentDecl->next;
  }

  llvm::Type *returnType = llvm::Type::getVoidTy(*cg->context);
  if (procDecl->outputParameters.parameterCount > 0)
    returnType = GetLLVMType(cg, &procDecl->outputParameters.firstParameter->typeInfo);

  llvm::ArrayRef<llvm::Type *> arrayRef(argumentTypes, procDecl->inputParameters.parameterCount);
  llvm::FunctionType *functionType = llvm::FunctionType::get(returnType, arrayRef, false);
  llvm::Function::LinkageTypes linkage = llvm::Function::ExternalLinkage;
  llvm::Function *llvmFunction = llvm::Function::Create(functionType, linkage,
    procDecl->identifier->name.string, cg->module);
  procDecl->llvmFunction = llvmFunction;

  //We need to know the context a return statement is found in
  //So we save the last procedure and update it with the procedure we are
  //about to generate.  The state is restored when this proc exits
  llvm::Function *lastFunction = cg->currentFunction;
  ProcedureDeclaration *lastProcedure = cg->currentProcedure;
  cg->currentFunction = llvmFunction;
  cg->currentProcedure = procDecl;

  if (procDecl->statementCount > 0) {
    llvm::BasicBlock *lastBlock = cg->builder->GetInsertBlock();
    llvm::BasicBlock *entryBlock = llvm::BasicBlock::Create(*cg->context, "entry", llvmFunction);

    cg->builder->SetInsertPoint(entryBlock);

    { //Allocate storage for params on stack and store values
      VariableDeclaration *currentDecl = procDecl->inputParameters.firstParameter;
      for (auto iter = llvmFunction->arg_begin(); iter != llvmFunction->arg_end(); iter++) {
        llvm::Value *arraySize = nullptr;
        TempStringBuilder sb;
        sb.append(currentDecl->identifier->name.string);
        sb.append(".stack_addr");
        currentDecl->llvmAlloca = cg->builder->CreateAlloca(GetLLVMType(cg, &currentDecl->typeInfo), arraySize, sb.get());
        currentDecl = (VariableDeclaration *)currentDecl->next;
      }

      currentDecl = procDecl->inputParameters.firstParameter;
      for (auto iter = llvmFunction->arg_begin(); iter != llvmFunction->arg_end(); iter++) {
        llvm::Value *value = (llvm::Value *)&(*iter);
        cg->builder->CreateStore(value, currentDecl->llvmAlloca);
        currentDecl = (VariableDeclaration *)currentDecl->next;
      }


      if (procDecl->outputParameters.parameterCount > 0) {
        currentDecl = procDecl->outputParameters.firstParameter;
        TempStringBuilder sb;
        sb.append(currentDecl->identifier->name.string);
        sb.append(".stack_addr");
        currentDecl->llvmAlloca = cg->builder->CreateAlloca(GetLLVMType(cg, &currentDecl->typeInfo), nullptr, sb.get());
      }
    }

    GenerateLinearizedAllocasForBlock(cg, procDecl);


    Statement *currentStatement = procDecl->firstStatement;
    while (currentStatement != nullptr) {
      CodegenStatement(cg, currentStatement);
      currentStatement = currentStatement->next;
    }

    CodegenReturnValuesForCurrentProcedure(cg);
  }

  //Restore the last procedure context
  cg->currentFunction = lastFunction;
  cg->currentProcedure = lastProcedure;
}

void CodegenVariableDeclaration(Backend_LLVM *cg, VariableDeclaration *varDecl) {
  if (varDecl->initalExpression != nullptr) {
    llvm::Value *llvmValue = CodegenExpression(cg, varDecl->initalExpression);
    llvm::Value *store = cg->builder->CreateStore(llvmValue, varDecl->llvmAlloca);
  }
}

void CodegenConstantDeclaration(Backend_LLVM *cg, ConstantDeclaration *cd) {
  cd->backendPointer = CodegenExpression(cg, cd->expression);
}

llvm::Value *CodegenVariableAccess(Backend_LLVM *cg, VariableAccess *va) {
  int currentIndex = 0;
  llvm::Value *currentPtr = va->variable->llvmAlloca;
  VariableDeclaration *currentVar = va->variable;

  while (currentIndex < va->accessCount) {
    if (currentIndex > 0) {
      currentVar = GetVariableAtIndex(&currentVar->typeInfo, va->indices[currentIndex]);
    }


    if (va->subscriptExpressions[currentIndex] != 0) {
      if (currentVar->typeInfo.indirectionLevel > 0) {
        currentPtr = cg->builder->CreateLoad(currentPtr);
      }

      llvm::Value *e = CodegenExpression(cg, va->subscriptExpressions[currentIndex]);
      if (currentVar->typeInfo.arraySize > 0) { 
        llvm::Value *indices[2];
        indices[0] = cg->builder->getInt32(0);
        indices[1] = e;
        llvm::ArrayRef<llvm::Value *> arrayRef(indices, 2);
        currentPtr = cg->builder->CreateGEP(currentPtr, arrayRef);
      } else {
        currentPtr = cg->builder->CreateGEP(currentPtr, e);
      }

      if (currentIndex + 1 < va->accessCount) {
        llvm::Value **indices = (llvm::Value **)alloca(2 * sizeof(llvm::Value *));
        indices[0] = cg->builder->getInt32(0);
        indices[1] = cg->builder->getInt32(va->indices[currentIndex + 1]);
        llvm::ArrayRef<llvm::Value *> arrayRef(indices, 2);
        currentPtr = cg->builder->CreateGEP(currentPtr, arrayRef);
      }
    }
    
    else if (currentIndex + 1 < va->accessCount) {
      if (currentVar->typeInfo.indirectionLevel > 0) {
        currentPtr = cg->builder->CreateLoad(currentPtr);
      }

      llvm::Value **indices = (llvm::Value **)alloca(2 * sizeof(llvm::Value *));
      indices[0] = cg->builder->getInt32(0);
      indices[1] = cg->builder->getInt32(va->indices[currentIndex + 1]);
      llvm::ArrayRef<llvm::Value *> arrayRef(indices, 2);
      currentPtr = cg->builder->CreateGEP(currentPtr, arrayRef);
    }

    currentIndex += 1;
  }

  return currentPtr;
}

void CodegenVariableAssignment(Backend_LLVM *cg, VariableAssignment *varAssignment) {
  llvm::Value *value = CodegenExpression(cg, varAssignment->expression);
  llvm::Value *storePtr = CodegenVariableAccess(cg, &varAssignment->variableAccess);
  llvm::Value *store = cg->builder->CreateStore(value, storePtr);
}

void CodegenCallStatement(Backend_LLVM *cg, CallStatement *callStatement) {
  llvm::Value **argValues = (llvm::Value **)alloca(sizeof(llvm::Value *) * callStatement->params.parameterExpressionCount);
  Expression *currentArgument = callStatement->params.firstParameterExpression;
  int currentArgumentIndex = 0;
  while (currentArgument != nullptr) {
    argValues[currentArgumentIndex++] = CodegenExpression(cg, currentArgument);
    currentArgument = currentArgument->next;
  }

  llvm::ArrayRef<llvm::Value *> arrayRef(argValues, callStatement->params.parameterExpressionCount);
  llvm::Value *value = cg->builder->CreateCall(callStatement->procedure->llvmFunction, arrayRef);
}

static void CodegenReturnValuesForCurrentProcedure(Backend_LLVM *cg) {
  if (cg->currentProcedure->outputParameters.parameterCount > 0) {
    VariableDeclaration *currentDecl = cg->currentProcedure->outputParameters.firstParameter;
    llvm::Value *load = cg->builder->CreateLoad(currentDecl->llvmAlloca);
    cg->builder->CreateRet(load);
  } else {
    cg->builder->CreateRetVoid();
  }
}

void CodegenReturnStatement(Backend_LLVM *cg, ReturnStatement *returnStatement) {
  CodegenReturnValuesForCurrentProcedure(cg);
}

llvm::Value *CodegenIntegerLiteral(Backend_LLVM *cg, IntegerLiteral *intLiteral) {
  //this int lieral was implicitly coerced into a ptr type
  if (intLiteral->typeInfo.indirectionLevel > 0) {
    llvm::Type *ptrType = GetLLVMType(cg, &intLiteral->typeInfo);
    llvm::Value *inttmp = cg->builder->getInt64(intLiteral->unsignedValue);
    llvm::Value *result = cg->builder->CreateIntToPtr(inttmp, ptrType);
    return result;
  }

  llvm::Value *result = nullptr;
  if (intLiteral->typeInfo.type == cg->compiler->typeDeclS8 || intLiteral->typeInfo.type == cg->compiler->typeDeclU8)
   result = cg->builder->getInt8(intLiteral->unsignedValue);
  else if (intLiteral->typeInfo.type == cg->compiler->typeDeclS16 || intLiteral->typeInfo.type == cg->compiler->typeDeclU16)
   result = cg->builder->getInt16(intLiteral->unsignedValue);
  else if (intLiteral->typeInfo.type == cg->compiler->typeDeclS32 || intLiteral->typeInfo.type == cg->compiler->typeDeclU32)
   result = cg->builder->getInt32(intLiteral->unsignedValue);
  else if (intLiteral->typeInfo.type == cg->compiler->typeDeclS64 || intLiteral->typeInfo.type == cg->compiler->typeDeclU64)
   result = cg->builder->getInt64(intLiteral->unsignedValue);
  return result;
}

llvm::Value *CodegenFloatLiteral(Backend_LLVM *cg, FloatLiteral *floatLiteral) {
  llvm::Value *result;
  if (floatLiteral->typeInfo.type == cg->compiler->typeDeclF32) {
    result = llvm::ConstantFP::get(llvm::Type::getFloatTy(*cg->context), floatLiteral->value);
  } else {
    result = llvm::ConstantFP::get(llvm::Type::getDoubleTy(*cg->context), floatLiteral->value);
  }

  return result;
}

llvm::Value *CodegenStringLiteral(Backend_LLVM *cg, StringLiteral *stringLiteral) {
  llvm::Value *result = cg->builder->CreateGlobalStringPtr(stringLiteral->value.string);
  return result;
}

llvm::Value *CodegenCastExpression(Backend_LLVM *cg, CastExpression *castExpr) {
  TypeInfo *dest = &castExpr->typeInfo;
  TypeInfo *src = &castExpr->expression->typeInfo;
  llvm::Value *value = CodegenExpression(cg, castExpr->expression);
  llvm::Type *castType = castExpr->typeInfo.type->llvmType;

  if (src->arraySize > 0) {
    assert(castExpr->expression->expressionType == ExpressionType_VariableExpression);
    VariableExpression *varExpr = (VariableExpression *)castExpr->expression;
    llvm::Value *result = CodegenVariableAccess(cg, &varExpr->variableAccess);
    llvm::Type *t = GetLLVMType(cg, dest);
    result = cg->builder->CreatePointerCast(result, t);
    return result;
  }

  if (castExpr->typeInfo.indirectionLevel > 0) {
    for (int i = 0; i < castExpr->typeInfo.indirectionLevel; i++)
      castType = llvm::PointerType::get(castType, 0);
    if (castExpr->expression->typeInfo.indirectionLevel == 0) {
      llvm::Value *result = cg->builder->CreateIntToPtr(value, castType);
      return result;
    }
  }


  llvm::Value *result = nullptr;
  if (dest->indirectionLevel > 0 && src->indirectionLevel > 0) {
    result = cg->builder->CreatePointerCast(value, castType);
  } else if (dest->indirectionLevel == 0 && src->indirectionLevel > 0) {
    result = cg->builder->CreatePtrToInt(value, castType);
  } else if (IsIntegerType(dest->type, cg->compiler) && IsIntegerType(src->type, cg->compiler)) {
    result = cg->builder->CreateIntCast(value, castType, true);
  } else if (IsFloatType(dest->type, cg->compiler) && IsFloatType(src->type, cg->compiler)) {
    result = cg->builder->CreateFPCast(value, castType);
  } else if (IsFloatType(dest->type, cg->compiler) && IsIntegerType(src->type, cg->compiler)) {
      if (IsUnsignedIntegerType(src->type, cg->compiler)) {
        result = cg->builder->CreateUIToFP(value, castType);
      } else {
        result = cg->builder->CreateSIToFP(value, castType);
      }
  } else if (IsIntegerType(dest->type, cg->compiler) && IsFloatType(src->type, cg->compiler)) {
      if (IsUnsignedIntegerType(dest->type, cg->compiler)) {
        result = cg->builder->CreateFPToUI(value, castType);
      } else {
        result = cg->builder->CreateFPToSI(value, castType);
      }
  }

  assert(result != nullptr);
  return result;
}

llvm::Value *CodegenBinaryOperation(Backend_LLVM *cg, BinaryOperation *binop) {
  llvm::Value *lhsValue = CodegenExpression(cg, binop->lhs);
  llvm::Value *rhsValue = CodegenExpression(cg, binop->rhs);
  assert(lhsValue != nullptr && rhsValue != nullptr);
  if (binop->binopToken == TokenType_LogicalOr || 
      binop->binopToken == TokenType_LogicalAnd) {
    assert(lhsValue->getType() == cg->builder->getInt1Ty());
    assert(rhsValue->getType() == cg->builder->getInt1Ty());
    llvm::Value *lhsIsTrue = cg->builder->CreateICmpEQ(lhsValue, cg->builder->getInt1(1));
    llvm::Value *rhsIsTrue = cg->builder->CreateICmpEQ(rhsValue, cg->builder->getInt1(1));
    llvm::Value *result = nullptr;
    if (binop->binopToken == TokenType_LogicalOr) {
      result = cg->builder->CreateOr(lhsValue, rhsValue);
    } else { 
      result = cg->builder->CreateAnd(lhsValue, rhsValue);
    }
    return result;
  }

  if (IsBitwiseBinOp(binop->binopToken) || IsArithmeticToken(binop->binopToken)) {
    llvm::Instruction::BinaryOps opcode = (llvm::Instruction::BinaryOps)0;
    if (binop->binopToken == TokenType_SymbolAdd) {
      if (IsFloatType(binop->typeInfo.type, cg->compiler)) {
        opcode = llvm::Instruction::FAdd;
      } else {
        opcode = llvm::Instruction::Add;
      }
    } else if (binop->binopToken == TokenType_SymbolSub) {
      if (IsFloatType(binop->typeInfo.type, cg->compiler)) {
        opcode = llvm::Instruction::FSub;
      } else {
        opcode = llvm::Instruction::Sub;
      }
    } else if (binop->binopToken == TokenType_SymbolMul) {
      if (IsFloatType(binop->typeInfo.type, cg->compiler)) {
        opcode = llvm::Instruction::FMul;
      } else {
        opcode = llvm::Instruction::Mul;
      }
    } else if (binop->binopToken == TokenType_SymbolDiv) {
      if (IsFloatType(binop->typeInfo.type, cg->compiler)) {
        opcode = llvm::Instruction::FDiv;
      } else {
        if (IsUnsignedIntegerType(binop->typeInfo.type, cg->compiler)) {
          opcode = llvm::Instruction::UDiv;
        } else {
          opcode = llvm::Instruction::SDiv;
        }
      }
    } else if (binop->binopToken == TokenType_SymbolMod) {
      opcode = llvm::Instruction::SRem;
      if (IsUnsignedIntegerType(binop->lhs->typeInfo.type, cg->compiler)) { //TODO does not hande pointers!
        opcode = llvm::Instruction::URem;
      }
    } else if (binop->binopToken == TokenType_BitwiseLeftShift) {
      opcode = llvm::Instruction::Shl;
    } else if (binop->binopToken == TokenType_BitwiseRightShift) {
      opcode = llvm::Instruction::LShr;
    } else if (binop->binopToken == TokenType_BitwiseAnd) {
      opcode = llvm::Instruction::And;
    } else if (binop->binopToken == TokenType_BitwiseOr) {
      opcode = llvm::Instruction::Or;
    } else if (binop->binopToken == TokenType_BitwiseXor) {
      opcode = llvm::Instruction::Xor;
    }

    assert(opcode != 0);
    llvm::Value *result = cg->builder->CreateBinOp(opcode, lhsValue, rhsValue);
    return result;
  }

  if (IsFloatType(binop->typeInfo.type, cg->compiler)) {
    llvm::CmpInst::Predicate pred = llvm::CmpInst::FCMP_FALSE;
    if (binop->binopToken == TokenType_LogicalEqual) {
      pred = llvm::CmpInst::FCMP_UEQ;
    } else if (binop->binopToken == TokenType_LogicalNotEqual) {
      pred = llvm::CmpInst::FCMP_UNE;
    } else if (binop->binopToken == TokenType_LogicalGreaterThan) {
      pred = llvm::CmpInst::FCMP_UGT;
    } else if (binop->binopToken == TokenType_LogicalGreaterOrEqual) {
      pred = llvm::CmpInst::FCMP_UGE;
    } else if (binop->binopToken == TokenType_LogicalLessThan) {
      pred = llvm::CmpInst::FCMP_ULT;
    } else if (binop->binopToken == TokenType_LogicalLessOrEqual) {
      pred = llvm::CmpInst::FCMP_ULE;
    }

    llvm::Value *result = cg->builder->CreateFCmp(pred, lhsValue, rhsValue);
    return result;
  }

  llvm::CmpInst::Predicate pred = (llvm::CmpInst::Predicate)0xFF;
  if (binop->binopToken == TokenType_LogicalEqual) {
    pred = llvm::CmpInst::ICMP_EQ;
  } else if (binop->binopToken == TokenType_LogicalNotEqual) {
    pred = llvm::CmpInst::ICMP_NE;
  } else {
    pred = llvm::CmpInst::ICMP_UGT;
    if (binop->binopToken == TokenType_LogicalGreaterOrEqual) {
      pred = llvm::CmpInst::ICMP_UGE;
    } else if (binop->binopToken == TokenType_LogicalLessThan) {
      pred = llvm::CmpInst::ICMP_ULT;
    } else if (binop->binopToken == TokenType_LogicalLessOrEqual) {
      pred = llvm::CmpInst::ICMP_ULE;
    }

    if (IsSignedIntegerType(binop->typeInfo.type, cg->compiler)) {
      int predValue = (int)pred;
      predValue += 4;
      pred = (llvm::CmpInst::Predicate)predValue;
    }
  }

  llvm::Type *intType = GetLLVMType(cg, &binop->typeInfo);
  llvm::Value *result = cg->builder->CreateICmp(pred, lhsValue, rhsValue);
  //llvm::Value *result = cg->builder->CreateIntCast(cmp, intType, IsSignedIntegerType(binop->typeInfo.type, cg->compiler));
  return result;
}

llvm::Value *CodegenVariableExpression(Backend_LLVM *cg, VariableExpression *varExpr) {
  llvm::Value *ptr = CodegenVariableAccess(cg, &varExpr->variableAccess);
  llvm::Value *load = cg->builder->CreateLoad(ptr);
  return load;
}

llvm::Value *CodegenConstantExpression(Backend_LLVM *cg, ConstantExpression *ce) {
  llvm::Value *result = (llvm::Value *)ce->constant->backendPointer;
  return result;
}

llvm::Value *CodegenCallExpression(Backend_LLVM *cg, CallExpression *callExpr) {
  assert(callExpr->procedure->llvmFunction != nullptr);
  llvm::Value **argValues = (llvm::Value **)alloca(sizeof(llvm::Value *) * callExpr->params.parameterExpressionCount);
  Expression *currentArgument = callExpr->params.firstParameterExpression;
  int currentArgumentIndex = 0;
  while (currentArgument != nullptr) {
    argValues[currentArgumentIndex++] = CodegenExpression(cg, currentArgument);
    currentArgument = currentArgument->next;
  }

  llvm::ArrayRef<llvm::Value *> arrayRef(argValues, callExpr->params.parameterExpressionCount);
  llvm::Value *value = cg->builder->CreateCall(callExpr->procedure->llvmFunction, arrayRef);
  return value;
}

llvm::Value *CodegenUnaryOperation(Backend_LLVM *cg, UnaryOperation *unaryOp) {
  if (unaryOp->unaryToken == TokenType_SymbolAddress) {
    llvm::Value *exprValue = nullptr;
    if (unaryOp->expression->expressionType == ExpressionType_VariableExpression) {
      VariableExpression *varExpr = (VariableExpression *)unaryOp->expression;
      exprValue = CodegenVariableAccess(cg, &varExpr->variableAccess);
    } else {
      exprValue = CodegenExpression(cg, unaryOp->expression);
    }

    if (exprValue == nullptr) return nullptr;
    assert(unaryOp->unaryCount > 0);
    llvm::Value *zeroValue = cg->builder->getInt64(0);
    llvm::Value *result = exprValue;
    for (size_t i = 0; i < unaryOp->unaryCount; i++)
      result = cg->builder->CreateGEP(result, zeroValue);
    return result;
  }

  if (unaryOp->unaryToken == TokenType_SymbolSub) {
    llvm::Value *exprValue = CodegenExpression(cg, unaryOp->expression);
    llvm::Value *result = nullptr;
    if (IsFloatType(unaryOp->typeInfo.type, cg->compiler)) {
      result = cg->builder->CreateFNeg(exprValue);
    } else {
      result = cg->builder->CreateNeg(exprValue);
    }
    
    return result;
  }

  if (unaryOp->unaryToken == TokenType_LogicalNot) {
    llvm::Value *exprValue = CodegenExpression(cg, unaryOp->expression);
    llvm::Type *llvmType = GetLLVMType(cg, &unaryOp->expression->typeInfo);
    llvm::Value *zeroValue = llvm::ConstantInt::get(llvmType, 0);
    llvm::Value *cmp = cg->builder->CreateICmpEQ(exprValue, zeroValue);
    bool isSigned = IsSignedIntegerType(unaryOp->expression->typeInfo.type, cg->compiler);
    llvm::Value *result = cg->builder->CreateIntCast(cmp, llvmType, isSigned);
    return result;
  }


  if (unaryOp->unaryToken == TokenType_BitwiseNot) {
    llvm::Value *exprValue = CodegenExpression(cg, unaryOp->expression);
    llvm::Value *result = cg->builder->CreateNot(exprValue);
    return result;
  }

  assert(false);
  return nullptr;
}

llvm::Value *CodegenSizeOfExpression(Backend_LLVM *cg, SizeOfExpression *expr) {
  const llvm::DataLayout& dataLayout = cg->module->getDataLayout();
  llvm::Type *type = GetLLVMType(cg, &expr->sizeOfTypeInfo);
  uint64_t size = dataLayout.getTypeSizeInBits(type) / 8;
  return cg->builder->getInt64(size);
}


void InitalizeCompileUnitAndFiles(Backend_LLVM *llvmBE) {
  Compiler *c = llvmBE->compiler;
  llvmBE->diFiles.reserve(c->sourceFiles.size());
  for (size_t i = 0; i < c->sourceFiles.size(); i++) {
    SourceFile *file = &c->sourceFiles[i];
    llvmBE->diFiles.push_back(llvmBE->dibuilder->createFile(file->absolutePath.string, file->absolutePath.string));
  }

  uint32_t lang = llvm::dwarf::DW_LANG_C;
  const char *producer = "skylang";
  bool isOptimized = false;    
  llvmBE->dibuilder->createCompileUnit(lang, llvmBE->diFiles[0], producer, isOptimized, "", 0);
}

llvm::DIType *Backend_LLVM::GetDIType(TypeInfo *typeInfo) {
  char *name = typeInfo->type->identifier->name.string;
  llvm::DIType *result = diTypeMap[name];
  for (auto i = 0; i < typeInfo->indirectionLevel; i++) {
    result = dibuilder->createPointerType(result, 64);
  }

  return result;
}

void Backend_LLVM::CreateDebugInfoForProcedure(ProcedureDeclaration *procedure) {
  llvm::DIFile *file = diFiles[procedure->location.fileID];
  llvm::DIType *parameterTypes[procedure->inputParameters.parameterCount];
  //llvm::ArrayRef<llvm::DIType *> arrayRef(parameterTypes, procedure->params.parameterCount);
  //DITypeArrayRef diArrayRef = dibuilder->getOrCreateTypeArray(arrayRef);
  //Return type is at zero index
  llvm::DISubroutineType *type = 0; //dibuilder->createSubroutineType();

  llvm::DISubprogram *subprogram = dibuilder->createFunction(
    currentDebugScope, procedure->identifier->name.string, "", file, procedure->location.lineNumber,
    type, false /* internal linkage */, true /* definition */, procedure->location.lineNumber);
}