
void CodegenStatement(LLVMCodegenerator *cg, Statement *statement) {
  switch (statement->statementType) {
    case StatementType_TypeDeclaration: CodegenTypeDeclaration(cg, (TypeDeclaration *)statement); break;
    case StatementType_ProcedureDeclaration: CodegenProcedureDeclaration(cg, (ProcedureDeclaration *)statement); break;
    case StatementType_VariableDeclaration: CodegenVariableDeclaration(cg, (VariableDeclaration *)statement); break;
    case StatementType_VariableAssignment: CodegenVariableAssignment(cg, (VariableAssignment *)statement); break;
    case StatementType_ReturnStatement: CodegenReturnStatement(cg, (ReturnStatement *)statement); break;
    case StatementType_CallStatement: CodegenCallStatement(cg, (CallStatement *)statement); break;

    case StatementType_WhileStatement: CodegenWhileStatement(cg, (WhileStatement *)statement); break;
    case StatementType_IfStatement: CodegenIfStatement(cg, (IfStatement *)statement); break;

    default: assert(false); break;
  };
}

llvm::Value *CodegenExpression(LLVMCodegenerator *cg, Expression *expr) {
  switch (expr->expressionType) {
    case ExpressionType_IntegerLiteral: return CodegenIntegerLiteral(cg, (IntegerLiteral *)expr);
    case ExpressionType_FloatLiteral:  return CodegenFloatLiteral(cg, (FloatLiteral *)expr);
    case ExpressionType_StringLiteral: return CodegenStringLiteral(cg, (StringLiteral *)expr);
    case ExpressionType_CastExpression: return CodegenCastExpression(cg, (CastExpression *)expr);
    case ExpressionType_MemberAccessExpression: return CodgenMemberAccessExpression(cg, (MemberAccessExpression *)expr);
    case ExpressionType_UnaryOperation: return CodegenUnaryOperation(cg, (UnaryOperation *)expr);
    case ExpressionType_BinaryOperation: return CodegenBinaryOperation(cg, (BinaryOperation *)expr);
    case ExpressionType_VariableExpression: return CodegenVariableExpression(cg, (VariableExpression *)expr);
    case ExpressionType_CallExpression: return CodegenCallExpression(cg, (CallExpression *)expr);
    default: assert(false); break;
  };

  assert(false);
  return nullptr;
}

llvm::Value *CodegenBranchCondition(LLVMCodegenerator *cg, Expression *e) {
  llvm::Value *condExpr = CodegenExpression(cg, e);
  if (e->expressionType == ExpressionType_IntegerLiteral) {
    llvm::Type *destType = cg->builder->getInt1Ty();
    llvm::Value *result = cg->builder->CreateIntCast(condExpr, destType, false);
    return result;
  }
  return condExpr;
}


void CodegenIfStatement(LLVMCodegenerator *cg, IfStatement *is) {
  llvm::BasicBlock *condBlock = llvm::BasicBlock::Create(*cg->context, "if.condition", cg->currentFunction);
  llvm::BasicBlock *bodyBlock = llvm::BasicBlock::Create(*cg->context, "if.body", cg->currentFunction);
  llvm::BasicBlock *exitBlock = llvm::BasicBlock::Create(*cg->context, "if.exit", cg->currentFunction);
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
  cg->builder->CreateBr(nextBlock);

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
      cg->builder->CreateBr(exitBlock);
      currentElse = currentElse->nextElse;
      currentCond = nextExit;
    }
  }
  
  cg->builder->SetInsertPoint(exitBlock);
}

void CodegenWhileStatement(LLVMCodegenerator *cg, WhileStatement *ws) {
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

void CodegenBlock(LLVMCodegenerator *cg, Block *block) {
  Statement *currentStatement = block->firstStatement;
  while (currentStatement != nullptr) {
    CodegenStatement(cg, currentStatement);
    currentStatement = currentStatement->next;
  }
}

bool WriteNativeObject(llvm::Module *module) {
  llvm::Triple triple;
  triple.setTriple(llvm::sys::getDefaultTargetTriple());
  std::string error;
  const llvm::Target *target = llvm::TargetRegistry::lookupTarget(triple.normalize(), error);
  return false;
}

void CodegenGlobalBlock(Compiler *compiler, Block *block) {
  LLVMCodegenerator llvmCG = {};
  llvmCG.context = new llvm::LLVMContext();
  llvmCG.module = new llvm::Module("Compiler", *llvmCG.context);
  llvmCG.builder = new llvm::IRBuilder<>(*llvmCG.context);
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

  CodegenBlock(&llvmCG, block);

  {
    llvmCG.module->dump();
    std::ofstream stream("test.ll");
    llvm::raw_os_ostream llvmStream(stream);
    llvmCG.module->print(llvmStream, nullptr);
  }

  
  llvm::raw_os_ostream stdoutStream(std::cout);
  if (llvm::verifyModule(*llvmCG.module, &stdoutStream) == false) {
    system("llc -filetype=obj test.ll -o test.o");
    system("clang++ test.o -o test");
  }
}

void CodegenTypeDeclaration(LLVMCodegenerator *cg, TypeDeclaration *typeDecl) {
  if (typeDecl->statementCount > 0) {
    llvm::Type **memberTypeRefs = (llvm::Type **)alloca(sizeof(llvm::Type *) * typeDecl->statementCount);
    VariableDeclaration *currentVarDecl = (VariableDeclaration *)typeDecl->firstStatement;
    size_t currentVarIndex = 0;
    while (currentVarDecl != nullptr) {
      assert(currentVarDecl->typeInfo.type->llvmType != nullptr);
      memberTypeRefs[currentVarIndex++] = currentVarDecl->typeInfo.type->llvmType;
      currentVarDecl = (VariableDeclaration *)currentVarDecl->next; 
    }

    llvm::ArrayRef<llvm::Type *> arrayRef(memberTypeRefs, typeDecl->statementCount);
    llvm::Type *type = llvm::StructType::create(*cg->context, arrayRef, typeDecl->identifier->name.string, false);
    typeDecl->llvmType = type;
  }
}

llvm::Type *GetLLVMType(LLVMCodegenerator *cg, TypeInfo *typeInfo) {
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

void CodegenProcedureDeclaration(LLVMCodegenerator *cg, ProcedureDeclaration *procDecl) {
  llvm::Type **argumentTypes = (llvm::Type **)alloca(sizeof(llvm::Type *) * procDecl->params.parameterCount);
  VariableDeclaration *currentDecl = procDecl->params.firstParameter;
  size_t currentArgumentIndex = 0;
  while (currentDecl != nullptr) {
    assert(currentDecl->statementType == StatementType_VariableDeclaration);
    argumentTypes[currentArgumentIndex++] = GetLLVMType(cg, &currentDecl->typeInfo);
    currentDecl = (VariableDeclaration *)currentDecl->next;
  }

  llvm::Type *returnType = llvm::Type::getVoidTy(*cg->context);
  if (procDecl->returnTypeInfo.type != nullptr)
    returnType = procDecl->returnTypeInfo.type->llvmType;

  llvm::ArrayRef<llvm::Type *> arrayRef(argumentTypes, procDecl->params.parameterCount);
  llvm::FunctionType *functionType = llvm::FunctionType::get(returnType, arrayRef, false);
  llvm::Function::LinkageTypes linkage = llvm::Function::ExternalLinkage;
  llvm::Function *llvmFunction = llvm::Function::Create(functionType, linkage,
    procDecl->identifier->name.string, cg->module);
  procDecl->llvmFunction = llvmFunction;

  if (procDecl->statementCount > 0) {
    llvm::Function *lastFunction = cg->currentFunction;
    cg->currentFunction = llvmFunction;
    llvm::BasicBlock *lastBlock = cg->builder->GetInsertBlock();
    llvm::BasicBlock *entryBlock = llvm::BasicBlock::Create(*cg->context, "entry", llvmFunction);

    cg->builder->SetInsertPoint(entryBlock);
    VariableDeclaration *currentDecl = procDecl->params.firstParameter;
    for (auto iter = llvmFunction->arg_begin(); iter != llvmFunction->arg_end(); iter++) {
      llvm::Value *value = (llvm::Value *)&(*iter);
      llvm::Value *arraySize = cg->builder->getInt32(currentDecl->typeInfo.arraySize);
      currentDecl->llvmAlloca = cg->builder->CreateAlloca(GetLLVMType(cg, &currentDecl->typeInfo), arraySize);
      cg->builder->CreateStore(value, currentDecl->llvmAlloca);
      currentDecl = (VariableDeclaration *)currentDecl->next;
    }


    Statement *currentStatement = procDecl->firstStatement;
    while (currentStatement != nullptr) {
      CodegenStatement(cg, currentStatement);
      currentStatement = currentStatement->next;
    }

    if (procDecl->returnTypeInfo.type == nullptr) cg->builder->CreateRetVoid();
    cg->currentFunction = lastFunction;
  }
}

void CodegenVariableDeclaration(LLVMCodegenerator *cg, VariableDeclaration *varDecl) {
  llvm::Type *llvmType = GetLLVMType(cg, &varDecl->typeInfo);
  llvm::Value *arraySize = cg->builder->getInt64(varDecl->typeInfo.arraySize);
  varDecl->llvmAlloca = cg->builder->CreateAlloca(llvmType, arraySize, varDecl->identifier->name.string);
  if (varDecl->initalExpression != nullptr) {
    llvm::Value *llvmValue = CodegenExpression(cg, varDecl->initalExpression);
    llvm::Value *store = cg->builder->CreateStore(llvmValue, varDecl->llvmAlloca);
  }
}

llvm::Value *CodegenMemberAccess(LLVMCodegenerator *cg, llvm::Value *ptr, TypeInfo *firstType, TypeMemberAccess *ma) {
  assert(ma->indexCount > 0);
  llvm::Value *currentPtr = ptr;
  TypeInfo *currentType = firstType;
  int currentIndex = 0;
  while (currentIndex < ma->indexCount) {
    if (currentType->indirectionLevel > 0) {
      currentPtr = cg->builder->CreateLoad(currentPtr);
    }

    currentType = GetSubTypeAtIndex(currentType, ma->indices[currentIndex]);
    assert(currentType != nullptr);
      
    llvm::Value **indices = (llvm::Value **)alloca(2 * sizeof(llvm::Value *));
    indices[0] = cg->builder->getInt32(0);
    indices[1] = cg->builder->getInt32(ma->indices[currentIndex]);
    llvm::ArrayRef<llvm::Value *> arrayRef(indices, 2);
    currentPtr = cg->builder->CreateGEP(currentPtr, arrayRef);
    currentIndex += 1;
  }

  return currentPtr;
}

void CodegenVariableAssignment(LLVMCodegenerator *cg, VariableAssignment *varAssignment) {
  llvm::Value *value = CodegenExpression(cg, varAssignment->expression);
  llvm::Value *storePtr = varAssignment->varDecl->llvmAlloca;
  if (varAssignment->memberAccess.indexCount > 0)
    storePtr = CodegenMemberAccess(cg, storePtr, &varAssignment->varDecl->typeInfo, &varAssignment->memberAccess);
  if (varAssignment->subscriptExpression != nullptr) {
    if (varAssignment->memberAccess.indexCount == 0) {
      storePtr = cg->builder->CreateLoad(storePtr);
    }
    llvm::Value *subscriptValue = CodegenExpression(cg, varAssignment->subscriptExpression);
    storePtr = cg->builder->CreateGEP(storePtr, subscriptValue);
  }
  llvm::Value *store = cg->builder->CreateStore(value, storePtr);
}

void CodegenCallStatement(LLVMCodegenerator *cg, CallStatement *callStatement) {
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

void CodegenReturnStatement(LLVMCodegenerator *cg, ReturnStatement *returnStatement) {
  llvm::Value *value = CodegenExpression(cg, returnStatement->returnValue);
  llvm::Value *ret = cg->builder->CreateRet(value);
}

llvm::Value *CodegenIntegerLiteral(LLVMCodegenerator *cg, IntegerLiteral *intLiteral) {
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

llvm::Value *CodegenFloatLiteral(LLVMCodegenerator *cg, FloatLiteral *floatLiteral) {
  llvm::Value *result;
  if (floatLiteral->typeInfo.type == cg->compiler->typeDeclF32) {
    result = llvm::ConstantFP::get(llvm::Type::getFloatTy(*cg->context), floatLiteral->value);
  } else {
    result = llvm::ConstantFP::get(llvm::Type::getDoubleTy(*cg->context), floatLiteral->value);
  }

  return result;
}

llvm::Value *CodegenStringLiteral(LLVMCodegenerator *cg, StringLiteral *stringLiteral) {
  llvm::Value *result = cg->builder->CreateGlobalStringPtr(stringLiteral->value.string);
  return result;
}

llvm::Value *CodegenCastExpression(LLVMCodegenerator *cg, CastExpression *castExpr) {
  llvm::Value *value = CodegenExpression(cg, castExpr->expression);
  llvm::Type *castType = castExpr->typeInfo.type->llvmType;
  if (castExpr->typeInfo.indirectionLevel > 0) {
    for (int i = 0; i < castExpr->typeInfo.indirectionLevel; i++)
      castType = llvm::PointerType::get(castType, 0);
    if (castExpr->expression->typeInfo.indirectionLevel == 0) {
      llvm::Value *result = cg->builder->CreateIntToPtr(value, castType);
      return result;
    }
  }

  TypeInfo *dest = &castExpr->typeInfo;
  TypeInfo *src = &castExpr->expression->typeInfo;
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

llvm::Value *CodgenMemberAccessExpression(LLVMCodegenerator *cg, MemberAccessExpression *memberAccess) {
  llvm::Value *value = CodegenMemberAccess(cg, memberAccess->varDecl->llvmAlloca, &memberAccess->varDecl->typeInfo, &memberAccess->memberAccess);
  llvm::Value *result = cg->builder->CreateLoad(value);
  return result;
}

llvm::Value *CodegenBinaryOperation(LLVMCodegenerator *cg, BinaryOperation *binop) {
  llvm::Value *lhsValue = CodegenExpression(cg, binop->lhs);
  llvm::Value *rhsValue = CodegenExpression(cg, binop->rhs);
  assert(lhsValue != nullptr);
  assert(rhsValue != nullptr);

  if (IsBitwiseBinOp(binop->binopToken) || IsArithmeticToken(binop->binopToken)) {
    llvm::Instruction::BinaryOps opcode = (llvm::Instruction::BinaryOps)0;
    if (binop->binopToken == TokenType_SymbolAdd) {
      opcode = llvm::Instruction::Add;
    } else if (binop->binopToken == TokenType_SymbolSub) {
      opcode = llvm::Instruction::Sub;
    } else if (binop->binopToken == TokenType_SymbolMul) {
      opcode = llvm::Instruction::Mul;
    } else if (binop->binopToken == TokenType_SymbolDiv) {
      opcode = llvm::Instruction::SDiv;
    } else if (binop->binopToken == TokenType_SymbolMod) {
      opcode = llvm::Instruction::SRem;
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
    llvm::Value *result = cg->builder->CreateBinOp(opcode, lhsValue, rhsValue, "binop");
    return result;
  }


  if (IsFloatType(binop->typeInfo.type, cg->compiler)) {
    llvm::CmpInst::Predicate pred = llvm::CmpInst::FCMP_FALSE;
    assert(false);
    return nullptr;
  }


  llvm::CmpInst::Predicate pred = llvm::CmpInst::ICMP_EQ;
  if (binop->binopToken == TokenType_LogicalNotEqual) {
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

llvm::Value *CodegenVariableExpression(LLVMCodegenerator *cg, VariableExpression *varExpr) {
  llvm::Value *load = cg->builder->CreateLoad(varExpr->varDecl->llvmAlloca);
  return load;
}

llvm::Value *CodegenCallExpression(LLVMCodegenerator *cg, CallExpression *callExpr) {
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

llvm::Value *CodegenUnaryOperation(LLVMCodegenerator *cg, UnaryOperation *unaryOp) {
  if (unaryOp->unaryToken == TokenType_SymbolAddress) {
    llvm::Value *exprValue = nullptr;
    if (unaryOp->expression->expressionType == ExpressionType_VariableExpression) {
      VariableExpression *varExpr = (VariableExpression *)unaryOp->expression;
      exprValue = varExpr->varDecl->llvmAlloca;
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
    llvm::Value *result = cg->builder->CreateNeg(exprValue);
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

  if (unaryOp->unaryToken == TokenType_ArrayOpen) {
    llvm::Value *subscriptValue = CodegenExpression(cg, unaryOp->subscriptExpression);
    assert(unaryOp->expression->expressionType == ExpressionType_VariableExpression ||
          unaryOp->expression->expressionType == ExpressionType_MemberAccessExpression);

    llvm::Value *ptr = nullptr;
    if (unaryOp->expression->expressionType == ExpressionType_MemberAccessExpression) {
      MemberAccessExpression *ma = (MemberAccessExpression *)unaryOp->expression;
      ptr = CodegenMemberAccess(cg, ma->varDecl->llvmAlloca, &ma->varDecl->typeInfo, &ma->memberAccess);
    } else {
      VariableExpression *varExpr = (VariableExpression *)unaryOp->expression;
      ptr = varExpr->varDecl->llvmAlloca;
    }

    if (unaryOp->expression->typeInfo.indirectionLevel > 0) {
      ptr = cg->builder->CreateLoad(ptr);
    }
    
    assert(ptr != nullptr);
    llvm::Value *value = cg->builder->CreateGEP(ptr, subscriptValue);
    llvm::Value *result = cg->builder->CreateLoad(value);
    return result;
  }

  assert(false);
  return nullptr;
}