
void CodegenStatement(LLVMCodegenerator *cg, Statement *statement) {
  switch(statement->statementType) {
    case StatementType_TypeDeclaration: CodegenTypeDeclaration(cg, (TypeDeclaration *)statement); break;
    case StatementType_ProcedureDeclaration: CodegenProcedureDeclaration(cg, (ProcedureDeclaration *)statement); break;
    case StatementType_VariableDeclaration: CodegenVariableDeclaration(cg, (VariableDeclaration *)statement); break;
    case StatementType_VariableAssignment: CodegenVariableAssignment(cg, (VariableAssignment *)statement); break;
    case StatementType_ReturnStatement: CodegenReturnStatement(cg, (ReturnStatement *)statement); break;
    case StatementType_CallStatement: CodegenCallStatement(cg, (CallStatement *)statement); break;
    default: assert(false); break;
  };
}

LLVMValueRef CodegenExpression(LLVMCodegenerator *cg, Expression *expr) {
  switch (expr->expressionType) {
    case ExpressionType_IntegerLiteral: return CodegenIntegerLiteral(cg, (IntegerLiteral *)expr);
    case ExpressionType_FloatLiteral:  return CodegenFloatLiteral(cg, (FloatLiteral *)expr);
    case ExpressionType_StringLiteral: return CodegenStringLiteral(cg, (StringLiteral *)expr);
    case ExpressionType_CastExpression: return CodegenCastExpression(cg, (CastExpression *)expr);
    case ExpressionType_MemberAccessExpression: return CodgenMemberAccessExpression(cg, (MemberAccessExpression *)expr);
    case ExpressionType_BinaryOperation: return CodegenBinaryOperation(cg, (BinaryOperation *)expr);
    case ExpressionType_VariableExpression: return CodegenVariableExpression(cg, (VariableExpression *)expr);
    case ExpressionType_CallExpression: return CodegenCallExpression(cg, (CallExpression *)expr);
    default: assert(false); break;
  };

  assert(false);
  return nullptr;
}

void CodegenGlobalBlock(Compiler *compiler, Block *block) {
  LLVMCodegenerator codegenerator = {};
  codegenerator.module = LLVMModuleCreateWithName("Compiler");
  codegenerator.builder = LLVMCreateBuilder();

  compiler->typeDeclU8->llvmType = LLVMInt8Type();
  compiler->typeDeclU16->llvmType = LLVMInt16Type();
  compiler->typeDeclU32->llvmType = LLVMInt32Type();
  compiler->typeDeclU64->llvmType = LLVMInt64Type();
  compiler->typeDeclS8->llvmType = LLVMInt8Type();
  compiler->typeDeclS16->llvmType = LLVMInt16Type();
  compiler->typeDeclS32->llvmType = LLVMInt32Type();
  compiler->typeDeclS64->llvmType = LLVMInt64Type();
  compiler->typeDeclF32->llvmType = LLVMFloatType();
  compiler->typeDeclF64->llvmType = LLVMDoubleType();

  Statement *currentStatement = block->firstStatement;
  while (currentStatement != nullptr) {
    CodegenStatement(&codegenerator, currentStatement);
    currentStatement = currentStatement->next;
  }

  LLVMDumpModule(codegenerator.module);
  char *errorMessage = nullptr;
  LLVMPrintModuleToFile(codegenerator.module, "test.ll", &errorMessage);
}

void CodegenTypeDeclaration(LLVMCodegenerator *cg, TypeDeclaration *typeDecl) {
  if (typeDecl->statementCount > 0) {
    LLVMTypeRef type = LLVMStructCreateNamed(LLVMGetGlobalContext(), typeDecl->identifier->name.string);
    typeDecl->llvmType = type;
    LLVMTypeRef *memberTypeRefs = (LLVMTypeRef *)alloca(sizeof(LLVMTypeRef) * typeDecl->statementCount);
    VariableDeclaration *currentVarDecl = (VariableDeclaration *)typeDecl->firstStatement;
    size_t currentVarIndex = 0;
    while (currentVarDecl != nullptr) {
      assert(currentVarDecl->typeDeclaration->llvmType != nullptr);
      memberTypeRefs[currentVarIndex++] = currentVarDecl->typeDeclaration->llvmType;
      currentVarDecl = (VariableDeclaration *)currentVarDecl->next; 
    }
    LLVMStructSetBody(type, memberTypeRefs, typeDecl->statementCount, false);
  }
}

LLVMTypeRef GetLLVMType(LLVMCodegenerator *cg, VariableDeclaration *varDecl) {
  assert(varDecl->typeDeclaration->llvmType != nullptr);
  LLVMTypeRef currentType = varDecl->typeDeclaration->llvmType;
  if (varDecl->indirectionLevel > 0) {
    for (int i = 0; i < varDecl->indirectionLevel; i++)
      currentType = LLVMPointerType(currentType, 0);
  } else if (varDecl->arraySize > 0) {
    currentType = LLVMArrayType(currentType, varDecl->arraySize);
  }
  return currentType;
}

void CodegenProcedureDeclaration(LLVMCodegenerator *cg, ProcedureDeclaration *procDecl) {
  LLVMTypeRef *argumentTypes = (LLVMTypeRef *)alloca(sizeof(LLVMTypeRef) * procDecl->argumentCount);
  VariableDeclaration *currentDecl = procDecl->firstArgument;
  size_t currentArgumentIndex = 0;
  while (currentDecl != nullptr) {
    assert(currentDecl->statementType == StatementType_VariableDeclaration);
    argumentTypes[currentArgumentIndex++] = GetLLVMType(cg, currentDecl);
    currentDecl = (VariableDeclaration *)currentDecl->next;
  }

  LLVMTypeRef returnType = LLVMVoidType();
  if (procDecl->returnType != nullptr)
    returnType = procDecl->returnType->llvmType;

  LLVMTypeRef functionType = LLVMFunctionType(returnType,
    argumentTypes, procDecl->argumentCount, false);
  LLVMValueRef llvmFunction = LLVMAddFunction(cg->module, 
    procDecl->identifier->name.string, functionType);
  procDecl->llvmFunction = llvmFunction;

  if (procDecl->statementCount > 0) {
    LLVMBasicBlockRef lastBlock = LLVMGetInsertBlock(cg->builder);
    LLVMBasicBlockRef entryBlock = LLVMAppendBasicBlock(procDecl->llvmFunction, "entry");
    LLVMPositionBuilderAtEnd(cg->builder, entryBlock);
    VariableDeclaration *currentArg = procDecl->firstArgument;
    while (currentArg != nullptr) {
      assert(currentArg->statementType == StatementType_VariableDeclaration);
      currentArg->llvmAlloca = LLVMBuildAlloca(cg->builder, GetLLVMType(cg, currentArg), currentArg->identifier->name.string);
      currentArg = (VariableDeclaration *)currentArg->next;
    }

    Statement *currentStatement = procDecl->firstStatement;
    while (currentStatement != nullptr) {
      CodegenStatement(cg, currentStatement);
      currentStatement = currentStatement->next;
    }
  }

  LLVMVerifyFunction(procDecl->llvmFunction, LLVMPrintMessageAction);
}

void CodegenVariableDeclaration(LLVMCodegenerator *cg, VariableDeclaration *varDecl) {
  LLVMTypeRef llvmType = GetLLVMType(cg, varDecl);
  varDecl->llvmAlloca = LLVMBuildAlloca(cg->builder, llvmType, varDecl->identifier->name.string);
  if (varDecl->initalExpression != nullptr) {
    LLVMValueRef llvmValue = CodegenExpression(cg, varDecl->initalExpression);
    LLVMValueRef store = LLVMBuildStore(cg->builder, llvmValue, varDecl->llvmAlloca);
  }
}

void CodegenVariableAssignment(LLVMCodegenerator *cg, VariableAssignment *varAssignment) {
  LLVMValueRef value = CodegenExpression(cg, varAssignment->expression);
  LLVMValueRef store = LLVMBuildStore(cg->builder, value, varAssignment->varDecl->llvmAlloca);
}

void CodegenCallStatement(LLVMCodegenerator *cg, CallStatement *callStatement) {
  LLVMValueRef *argValues = (LLVMValueRef *)alloca(sizeof(LLVMValueRef) * callStatement->argumentCount);
  Expression *currentArgument = callStatement->firstArgument;
  int currentArgumentIndex = 0;
  while (currentArgument != nullptr) {
    argValues[currentArgumentIndex] = CodegenExpression(cg, currentArgument);
    currentArgument = currentArgument->next;
  }

  LLVMValueRef value = LLVMBuildCall(cg->builder, callStatement->procedure->llvmFunction,
    argValues, callStatement->argumentCount, "call");
}

void CodegenReturnStatement(LLVMCodegenerator *cg, ReturnStatement *returnStatement) {
  LLVMValueRef value = CodegenExpression(cg, returnStatement->returnValue);
  LLVMValueRef ret = LLVMBuildRet(cg->builder, value);
}

LLVMValueRef CodegenIntegerLiteral(LLVMCodegenerator *cg, IntegerLiteral *intLiteral) {
  LLVMValueRef result = LLVMConstInt(LLVMInt32Type(), intLiteral->value, true);
  return result;
}

LLVMValueRef CodegenFloatLiteral(LLVMCodegenerator *cg, FloatLiteral *floatLiteral) {
  LLVMValueRef result = LLVMConstReal(LLVMFloatType(), floatLiteral->value);
  return result;
}

LLVMValueRef CodegenStringLiteral(LLVMCodegenerator *cg, StringLiteral *stringLiteral) {
  LLVMValueRef result = LLVMBuildGlobalStringPtr(cg->builder, stringLiteral->value.string, "string");
  return result;
}

LLVMValueRef CodegenCastExpression(LLVMCodegenerator *cg, CastExpression *castExpr) {
  LLVMValueRef value = CodegenExpression(cg, castExpr->expression);
  LLVMTypeRef castType = castExpr->typeDeclaration->llvmType;
  if (castExpr->unaryCount > 0) {
    for (int i = 0; i < castExpr->unaryCount; i++)
      castType = LLVMPointerType(castType, 0);
    if (castExpr->expression->unaryCount == 0) {
      LLVMValueRef result = LLVMBuildIntToPtr(cg->builder, value, castType, "IntToPtr");
      return result;
    }
  }

  assert(false);
  return nullptr;
}

LLVMValueRef CodgenMemberAccessExpression(LLVMCodegenerator *cg, MemberAccessExpression *memberAccess) {
  LLVMValueRef *indices = (LLVMValueRef *)alloca(memberAccess->memberAccess.indexCount * sizeof(LLVMValueRef));
  for (size_t i = 0; i < memberAccess->memberAccess.indexCount; i++)
    indices[i] = LLVMConstInt(LLVMInt32Type(), memberAccess->memberAccess.indices[i], false);
  LLVMValueRef value = LLVMBuildGEP(cg->builder, memberAccess->varDecl->llvmAlloca, 
    indices, memberAccess->memberAccess.indexCount, "GEP");
  if (memberAccess->unaryCount > 0) {
    return value;
  } else {
    return LLVMBuildLoad(cg->builder, value, "load");
  }
}

LLVMValueRef CodegenBinaryOperation(LLVMCodegenerator *cg, BinaryOperation *binop) {
  LLVMOpcode opcode = LLVMAdd;
  LLVMValueRef lhsValue = CodegenExpression(cg, binop->lhs);
  LLVMValueRef rhsValue = CodegenExpression(cg, binop->rhs);
  LLVMValueRef result = LLVMBuildBinOp(cg->builder, opcode, lhsValue, rhsValue, "binop");
  return result;
}

LLVMValueRef CodegenVariableExpression(LLVMCodegenerator *cg, VariableExpression *varExpr) {
  LLVMValueRef load = LLVMBuildLoad(cg->builder, varExpr->varDecl->llvmAlloca, "load");
  return load;
}

LLVMValueRef CodegenCallExpression(LLVMCodegenerator *cg, CallExpression *callExpr) {
  LLVMValueRef *argValues = (LLVMValueRef *)alloca(sizeof(LLVMValueRef) * callExpr->argumentCount);
  Expression *currentArgument = callExpr->firstArgument;
  int currentArgumentIndex = 0;
  while (currentArgument != nullptr) {
    argValues[currentArgumentIndex] = CodegenExpression(cg, currentArgument);
    currentArgument = currentArgument->next;
  }

  LLVMValueRef value = LLVMBuildCall(cg->builder, callExpr->procDecl->llvmFunction,
    argValues, callExpr->argumentCount, "calltmp");
  return value;
}