
bool ValidateStatement(Compiler *compiler, Statement *statement) {
  switch(statement->statementType) {
    case StatementType_VariableDeclaration: return ValidateVariableDeclaration(compiler, (VariableDeclaration *)statement);
    case StatementType_ProcedureDeclaration: ValidateProcedureDeclaration(compiler, (ProcedureDeclaration *)statement); return true; break;
    case StatementType_TypeDeclaration: return ValidateTypeDeclaration((TypeDeclaration *)statement);
    case StatementType_ConstantDeclaration: return ValidateConstantDeclaration(compiler, (ConstantDeclaration *)statement);

    case StatementType_VariableAssignment: return ValidateVariableAssignment(compiler, (VariableAssignment *)statement);
    case StatementType_CallStatement: return ValidateCallStatement(compiler, (CallStatement *)statement);
    case StatementType_ReturnStatement: return ValidateReturnStatement(compiler, (ReturnStatement *)statement);

    case StatementType_WhileStatement: return ValidateWhileStatement(compiler, (WhileStatement*)statement);
    case StatementType_IfStatement: return ValidateIfStatement(compiler, (IfStatement *)statement);


    default: assert(false);
  }

  assert(false);
  return false; //for debug
}

bool ValidateTypeDeclaration(TypeDeclaration *typeDecl) {
  return true;
}

bool ValidateExpression(Compiler *c, Expression *expr) {

  switch(expr->expressionType) {

    case ExpressionType_UnaryOperation: return ValidateUnaryOperation(c, (UnaryOperation *)expr);
    case ExpressionType_BinaryOperation: return ValidateBinaryOperation(c, (BinaryOperation *)expr);
    case ExpressionType_CastExpression: return ValidateCastExpression(c, (CastExpression *)expr);
    case ExpressionType_CallExpression: return ValidateCallExpression(c, (CallExpression *)expr);
    case ExpressionType_VariableExpression: ValidateVariableExpression(c, (VariableExpression *)expr); break;

    case ExpressionType_ConstantExpression:
    case ExpressionType_IntegerLiteral:
    case ExpressionType_FloatLiteral:
    case ExpressionType_StringLiteral:
    case ExpressionType_SizeOfExpression:
      return true;

    default: assert(false);
  }

  return true;
}

void iterate_expression_list(Expression *head, std::function<void(Expression *)> procedure) {
  Expression *current = head;
  while (current != nullptr) {
    procedure(current);
    current = current->next;
  }
}

bool ValidateCallExpression(Compiler *c, CallExpression *call) {
  if (call->procedure == nullptr) return false;

  if (call->procedure->outputParameters.parameterCount == 0) {
    ReportErrorC(c, FrontendErrorType_Unspecified, call->location, "Call to procedure " << call->procedure->identifier <<
    " is being used as an expression but the return type is Void\n");
  }

  call->typeInfo = call->procedure->outputParameters.firstParameter->typeInfo;
  ValidateParameterInvokation(c, &call->params, call->location, call->procedure->identifier); 
  return true;
}

bool ValidateIfStatement(Compiler *c, IfStatement *is) {
  if (ValidateExpression(c, is->condition) == false) return false;
  if (ValidateBlock(c, is) == false) return false;

  ElseStatement *current = is->elseStatement;
  while (current != nullptr) {
    if (current->condition != nullptr) {
      if (ValidateExpression(c, current->condition) == false) return false;
    }
    
    if (ValidateBlock(c, current) == false) return false;
    current = current->nextElse;
  }

  return true;
}

bool ValidateWhileStatement(Compiler *c, WhileStatement *ws) {
  if (ValidateExpression(c, ws->condition) == false) return false;
  return ValidateBlock(c, ws);
}

bool ValidateReturnStatement(Compiler *compiler, ReturnStatement *returnStatement) {
  return true;
}


TypeInfo ValidateVariableAccess(Compiler *compiler, SourceLocation location, VariableAccess *va) {
  VariableDeclaration *var = va->variable;
  for (int i = 0; i < va->accessCount; i++) {
    if (va->subscriptExpressions[i] != nullptr) {
      //TODO Make sure this thing is not a string, boolean, etc
      ValidateExpression(compiler, va->subscriptExpressions[i]);

      //TODO Figure out where we store location
      if (var->typeInfo.indirectionLevel == 0 && var->typeInfo.arraySize == 0) {
        ReportErrorC(compiler, FrontendErrorType_DerefrenceOfNonPointer, location, "Cannot derefrence non pointer type " <<
          &var->typeInfo << "\n");
      }

      //TODO This case needs to be handled properly
      if (var->typeInfo.indirectionLevel > 2 && i != va->accessCount - 1) {
        ReportErrorC(compiler, FrontendErrorType_Unspecified, location, "Pointer has to many levels of indirection to deref in the middle of a var access");
      }
    }

    if (i + 1 < va->accessCount) {
      var = GetVariableAtIndex(&var->typeInfo, va->indices[i + 1]);
    }
  }

  TypeInfo finalType = var->typeInfo;
  if (va->subscriptExpressions[va->accessCount - 1]) {
    if (finalType.indirectionLevel > 0) finalType.indirectionLevel--;
    else finalType.arraySize = 0;
  }

  return finalType;
}


void ValidateParameterInvokation(Compiler *compiler, ParameterInvokation *params, SourceLocation& loc, Identifier *ident) {
  if (params->parameterExpressionCount != params->parameterList->parameterCount) {
    ReportErrorC(compiler, FrontendErrorType_TypeMismatch, loc, "Parameter count mismatch\n    Call to procedre " << ident << params->parameterList << "does not match\n    the same number of" <<
      " parameters in call " << ident << params << "\n\n");
  }
  
  {
    Expression *current = params->firstParameterExpression;
    while (current != nullptr) {
      ValidateExpression(compiler, current);
      current = current->next;
    }
  }

  {
    ParameterDeclaration *paramDecl = params->parameterList;
    VariableDeclaration *currentVar = paramDecl->firstParameter;
    Expression *current = params->firstParameterExpression;
    while (current != nullptr) {
      //This currently(2017-06-15) happens when a call expression did not have its identifier resolved
      if (current->typeInfo.type != nullptr) {
        assert(currentVar != nullptr);
        if (AttemptTypeCoercionIfRequired(compiler, &currentVar->typeInfo, current) == false) {
          ReportErrorC(compiler, FrontendErrorType_TypeMismatch, current->location, "Parameter Type Mismatch:\n" <<
            "    In parameter invokation " << ident << params << " expression " << current << " of type " << &current->typeInfo <<
            " does not match \n    parameter " << currentVar->identifier->name.string << " of type " <<
            &currentVar->typeInfo << " in parameter declaration " << ident << paramDecl << "\n\n");
        } 
      }

      current = current->next;
      currentVar = (VariableDeclaration *)currentVar->next;
    }
  }
}

bool ValidateCallStatement(Compiler *compiler, CallStatement *call) {
  if (call->procedure == nullptr) return false;
  //TODO better error
  ValidateParameterInvokation(compiler, &call->params, call->location, call->procedure->identifier);
  return true;
}

//0: not a liteal
//1: int
//2: float
int CheckNumericLiteralAggregate(Compiler *c, Expression *e) {
  if (e->expressionType == ExpressionType_IntegerLiteral) return 1;
  if (e->expressionType == ExpressionType_FloatLiteral) return 2;
  if (e->expressionType == ExpressionType_UnaryOperation) {
    UnaryOperation *unary = (UnaryOperation *)e;
    return CheckNumericLiteralAggregate(c, unary->expression);
  }

  if (e->expressionType == ExpressionType_CastExpression) {
    CastExpression *cast = (CastExpression *)e;
    return CheckNumericLiteralAggregate(c, cast->expression);
  }

  if (e->expressionType == ExpressionType_BinaryOperation) {
    BinaryOperation *binop = (BinaryOperation *)e;
    int lhs = CheckNumericLiteralAggregate(c, binop->lhs);
    int rhs = CheckNumericLiteralAggregate(c, binop->rhs);
    if (lhs != 0 && rhs != 0) {
      assert(lhs == rhs);
      return lhs;
    }
  }

  return 0;
}


bool ValidateConstantDeclaration(Compiler *compiler, ConstantDeclaration *c) {
  ValidateExpression(compiler, c->expression);
  if (!IsLiteralExpression(c->expression)) {
    ReportErrorC(compiler, FrontendErrorType_NonConstantExpression, c->expression->location, "constant must be literal");
    return false;
  }

  return true;
}

static void PropagateTypeInfoToChildren(Expression *e, TypeInfo *ti) {
  if (e->expressionType == ExpressionType_BinaryOperation) {
    BinaryOperation *binop = (BinaryOperation *)e;
    PropagateTypeInfoToChildren(binop->lhs, ti);
    PropagateTypeInfoToChildren(binop->lhs, ti);
  } else if (e->expressionType == ExpressionType_UnaryOperation) {
    UnaryOperation *unary = (UnaryOperation *)e;
    PropagateTypeInfoToChildren(unary->expression, ti);
  }

  e->typeInfo = *ti;
}

bool AttemptTypeCoercionIfRequiredExpr(Compiler *compiler, Expression *a, Expression *b) {
  Expression *primary = a;
  Expression *secondary = b;
  if (a->expressionType == ExpressionType_IntegerLiteral) {
    primary = b;
    secondary = a;
  }

  return AttemptTypeCoercionIfRequired(compiler, &primary->typeInfo, secondary);
}

bool ValidateBinaryOperation(Compiler *compiler, BinaryOperation *binOp) {
  assert(binOp->typeInfo.type == nullptr); //Type should not be resolved yet
  ValidateExpression(compiler, binOp->lhs);
  ValidateExpression(compiler, binOp->rhs);

  bool result = AttemptTypeCoercionIfRequiredExpr(compiler, binOp->lhs, binOp->rhs);
  if (result == false) {
    ReportErrorC(compiler, FrontendErrorType_TypeMismatch, binOp->location, "Binary Operand Type Mismatch:\n  LHS expression '" <<
      binOp->lhs << "' of type '" << &binOp->lhs->typeInfo << "' does not match RHS expression '" <<
      binOp->rhs << "' of type '" << &binOp->rhs->typeInfo << "'\n");
  } 

  if (IsBitwiseBinOp(binOp->binopToken)) {
    if (!IsIntegerType(binOp->lhs->typeInfo.type, compiler) || !IsIntegerType(binOp->rhs->typeInfo.type, compiler)) {
      ReportErrorC(compiler, FrontendErrorType_InvalidStatement, binOp->location, "cannot perdorm bitwise op on non int types");
      return false;
    }
  }
  
  binOp->typeInfo = binOp->lhs->typeInfo;
  assert(binOp->typeInfo.type != nullptr);
  return result;
}

//TODO make sure casts are Valid here
bool ValidateCastExpression(Compiler *compiler, CastExpression *cast) {
  if (ValidateExpression(compiler, cast->expression) == false) return false;
  return true;
}

bool ValidateVariableAssignment(Compiler *compiler, VariableAssignment *varAssign) {
  TypeInfo type = ValidateVariableAccess(compiler, varAssign->location, &varAssign->variableAccess);
  varAssign->typeInfo = type;
  if (ValidateExpression(compiler, varAssign->expression) == false) return false;
  if (AttemptTypeCoercionIfRequired(compiler, &type, varAssign->expression) == false) {
    Identifier *varIdent = varAssign->variableAccess.variable->identifier;
    Identifier *typeIdent = varAssign->expression->typeInfo.type->identifier;
    ReportErrorC(compiler, FrontendErrorType_TypeMismatch, varAssign->location, "Cannot assign expression " <<
      varAssign->expression << " of type " << &varAssign->expression->typeInfo <<
      " to variable " << &varAssign->variableAccess << " of type " << &varAssign->typeInfo << "\n");
    return false;
  }
  return true;
}

bool AttemptTypeCoercionIfRequired(Compiler *compiler, TypeInfo *requestedType, Expression *expr) {
  if (Equals(requestedType, &expr->typeInfo)) return true;

  if (expr->expressionType == ExpressionType_UnaryOperation) {
    UnaryOperation *unaryOp = (UnaryOperation *)expr;
    if (AttemptTypeCoercionIfRequired(compiler, requestedType, unaryOp->expression)) {
      unaryOp->typeInfo = unaryOp->expression->typeInfo;
      return true;
    }
    return false;
  }

  int literalType = CheckNumericLiteralAggregate(compiler, expr);
  if (expr->expressionType == ExpressionType_ConstantExpression) {
    ConstantExpression *c = (ConstantExpression *)expr;
    if (c->constant->expression->expressionType == ExpressionType_IntegerLiteral) {
      literalType = 1;
    } else if (c->constant->expression->expressionType == ExpressionType_FloatLiteral) {
      literalType = 2;
    }
  }

  if (literalType == 1 && IsIntegerType(requestedType->type, compiler)) {
    IntegerLiteral *intLiteral = (IntegerLiteral *)expr;
    bool literalFitsInRequestedType = true;

    if (requestedType->type == compiler->typeDeclU8 || requestedType->type == compiler->typeDeclS8) {
      if ((intLiteral->unsignedValue & ~0xFF) > 0) literalFitsInRequestedType = false;
    } else if (requestedType->type == compiler->typeDeclU16 || requestedType->type == compiler->typeDeclS16) {
      if ((intLiteral->unsignedValue & ~0xFFFF) > 0) literalFitsInRequestedType = false;
    } else if (requestedType->type == compiler->typeDeclU32 || requestedType->type == compiler->typeDeclS32) {
      if ((intLiteral->unsignedValue & ~0xFFFFFFFF) > 0) literalFitsInRequestedType = false;
    }

    if (literalFitsInRequestedType == false) {
      ReportErrorC(compiler, FrontendErrorType_InvalidStatement, intLiteral->location, "Literal does not fit");
      return false;
    }

    PropagateTypeInfoToChildren(expr, requestedType);
    return true;
  }

  if (literalType == 2 && IsFloatType(requestedType->type, compiler)) {
    PropagateTypeInfoToChildren(expr, requestedType);
    return true;
  }

  return false;
}

bool ValidateVariableDeclaration(Compiler *compiler, VariableDeclaration *varDecl) {
  if (varDecl->initalExpression == nullptr) return true;
  if (ValidateExpression(compiler, varDecl->initalExpression) == false) {
    return false;
  }

  if (AttemptTypeCoercionIfRequired(compiler, &varDecl->typeInfo, varDecl->initalExpression) == false) {
    ReportErrorC(compiler, FrontendErrorType_TypeMismatch, varDecl->location, "Variable Declaration Type mismatch: Expected expression of type " <<
      &varDecl->typeInfo << " Actual type was " << &varDecl->initalExpression->typeInfo << "\n");
    return false;
  }

  return true;
}

void ValidateProcedureDeclaration(Compiler *compiler, ProcedureDeclaration *procDecl) {
  //ValidateParameterDeclaration(procDecl->inputParameters);
  //ValidateParametersDeclaration(procDecl->outputParameters);
  if (procDecl->outputParameters.parameterCount > 1) {
    ReportErrorC(compiler, FrontendErrorType_Unspecified, procDecl->location, "Compiler does noy support " <<
      "multipule return values at this time");
  }
  ValidateBlock(compiler, procDecl);
}

bool ValidateBlock(Compiler *compiler, Block *block) {
  Statement *statement = block->firstStatement;
  bool returnStatementSeen = false;
  while (statement != nullptr) {
    if (returnStatementSeen) {
      ReportErrorC(compiler, FrontendErrorType_InvalidStatement, statement->location, "Cannot declare statement after return\n");
    }
    ValidateStatement(compiler, statement);

    if (statement->statementType == StatementType_ReturnStatement) {
      returnStatementSeen = true;
    }
    statement = statement->next;
  }
  return true;
}

void ValidateVariableExpression(Compiler *compiler, VariableExpression *expr) {
  TypeInfo t = ValidateVariableAccess(compiler, expr->location, &expr->variableAccess);
  expr->typeInfo = t;
}

bool ValidateUnaryOperation(Compiler *compiler, UnaryOperation *unaryOp) {
  ValidateExpression(compiler, unaryOp->expression);
  unaryOp->typeInfo = unaryOp->expression->typeInfo;
  if (unaryOp->unaryToken == TokenType_SymbolAddress) {
    unaryOp->typeInfo.indirectionLevel += unaryOp->unaryCount;
  }

  if (unaryOp->unaryToken == TokenType_SymbolAddress) {
    if (unaryOp->expression->expressionType == ExpressionType_IntegerLiteral ||
        unaryOp->expression->expressionType == ExpressionType_FloatLiteral ||
        unaryOp->expression->expressionType == ExpressionType_StringLiteral) {
      ReportErrorC(compiler, FrontendErrorType_InvalidStatement, unaryOp->location, "cannot take address of literal");
      return false;
    }
  }

  if (unaryOp->unaryToken == TokenType_LogicalNot ||
      unaryOp->unaryToken == TokenType_BitwiseNot ||
      unaryOp->unaryToken == TokenType_SymbolSub) {
    if (unaryOp->unaryCount > 1 ) {
      ReportErrorC(compiler, FrontendErrorType_InvalidStatement, unaryOp->location, "Only one unary token is permitted");
      return false;
    }
  }
  
  return true;
}

