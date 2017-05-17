
bool ValidateStatement(Compiler *compiler, Statement *statement) {
  switch(statement->statementType) {
    case StatementType_VariableDeclaration: return ValidateVariableDeclaration(compiler, (VariableDeclaration *)statement);
    case StatementType_ProcedureDeclaration: return ValidateProcedureDeclaration(compiler, (ProcedureDeclaration *)statement);
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

    case ExpressionType_ConstantExpression:
    case ExpressionType_MemberAccessExpression:
    case ExpressionType_VariableExpression:
    case ExpressionType_IntegerLiteral:
    case ExpressionType_FloatLiteral:
    case ExpressionType_StringLiteral:
      return true;

    default: assert(false);
  }

  assert(false);
  return false;
}

bool ValidateCallExpression(Compiler *c, CallExpression *call) {
  return ValidateParameterInvokation(c, &call->params); 
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
  return ValidateExpression(compiler, returnStatement->returnValue);
}

bool ValidateParameterInvokation(Compiler *compiler, ParameterInvokation *params) {
  if (params->parameterExpressionCount != params->parameterList->parameterCount) {
    ReportError(compiler, "parameter count does not match");
    return false;
  }
  
  {
    Expression *current = params->firstParameterExpression;
    while (current != nullptr) {
      if (ValidateExpression(compiler, current) == false) return false;
      current = current->next;
    }
  }

  {
    ParameterDeclaration *paramDecl = params->parameterList;
    VariableDeclaration *currentVar = paramDecl->firstParameter;
    Expression *current = params->firstParameterExpression;
    while (current != nullptr) {
      assert(currentVar != nullptr);
      if (AttemptTypeCoercionIfRequired(compiler, &currentVar->typeInfo, current) == false) {
        ReportErrorT(compiler, current->location, "In parameter invokation '",
          TERMINAL_COLOR_CYAN, params, TERMINAL_COLOR_DEFAULT,
          "': \n   Expression '", TERMINAL_COLOR_CYAN, current, TERMINAL_COLOR_DEFAULT,
          "' of type '", TERMINAL_COLOR_CYAN, &current->typeInfo, TERMINAL_COLOR_DEFAULT, 
          "' does not match parameter declaration",
          TERMINAL_COLOR_CYAN, paramDecl, TERMINAL_COLOR_DEFAULT, "\n");
        return false;
      } 
      current = current->next;
      currentVar = (VariableDeclaration *)currentVar->next;
    }
  }

  return true;
}

bool ValidateCallStatement(Compiler *compiler, CallStatement *call) {
  return ValidateParameterInvokation(compiler, &call->params);
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
    ReportError(compiler, "constant must be literal");
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

bool ValidateBinaryOperation(Compiler *compiler, BinaryOperation *binOp) {
  if (ValidateExpression(compiler, binOp->lhs) == false) return false;
  if (ValidateExpression(compiler, binOp->rhs) == false) return false;

  bool result = AttemptTypeCoercionIfRequired(compiler, &binOp->lhs->typeInfo, binOp->rhs);
  if (result == false) {

#if 0
    ReportErrorT(compiler, binOp->location, "Binary Operand Type Mismatch:\n  LHS expression '",
      TERMINAL_COLOR_BLUE, binOp->lhs, TERMINAL_COLOR_DEFAULT, "' of type '",
      TERMINAL_COLOR_LIGHTGREEN, &binOp->lhs->typeInfo, TERMINAL_COLOR_DEFAULT,
      "' does not match RHS expression '", TERMINAL_COLOR_BLUE, binOp->rhs, TERMINAL_COLOR_DEFAULT,
      "' of type '", TERMINAL_COLOR_LIGHTGREEN, &binOp->rhs->typeInfo, TERMINAL_COLOR_DEFAULT, "'\n");
#endif

    ReportErrorC(compiler, binOp->location, "Binary Operand Type Mismatch:\n  LHS expression '" <<
      binOp->lhs << "' of type '" << &binOp->lhs->typeInfo << "' does not match RHS expression '" <<
      binOp->rhs << "' of type '" << &binOp->rhs->typeInfo << "'\n");

  } 

  if (IsBitwiseBinOp(binOp->binopToken)) {
    if (!IsIntegerType(binOp->lhs->typeInfo.type, compiler) || !IsIntegerType(binOp->rhs->typeInfo.type, compiler)) {
      ReportError(compiler, binOp->location, "cannot perdorm bitwise op on non int types");
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
  if (varAssign->subscriptExpression != nullptr) {
    if (varAssign->typeInfo.indirectionLevel == 0) {
      ReportError(compiler, varAssign->location, "Cannot dereference a non pointer type");
      return false;
    }

    if (ValidateExpression(compiler, varAssign->subscriptExpression) == false) return false;
    varAssign->typeInfo.indirectionLevel -= 1;
  }

  if (ValidateExpression(compiler, varAssign->expression) == false) return false;
  if (AttemptTypeCoercionIfRequired(compiler, &varAssign->typeInfo, varAssign->expression) == false) {
    Identifier *varIdent = varAssign->varDecl->identifier;
    Identifier *typeIdent = varAssign->expression->typeInfo.type->identifier;
    ReportError(compiler, varAssign->location, "Cannot assign Variable(%s) to Expression(%s)",
      varIdent->name.string, typeIdent->name.string);
    return false;
  }
  return true;
}

bool AttemptTypeCoercionIfRequired(Compiler *compiler, TypeInfo *requestedType, Expression *expr) {
  assert(expr->typeInfo.type != nullptr);
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
      ReportError(compiler, "Literal does not fit");
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

  if (AttemptTypeCoercionIfRequired(compiler, &varDecl->typeInfo, varDecl->initalExpression)) return true;
  Identifier *expected = varDecl->typeInfo.type->identifier;
  Identifier *actual = varDecl->initalExpression->typeInfo.type->identifier;
  ReportError(compiler, varDecl->location, "Variable Declaration Type mismatch: Expected expression of type '%s'."
    "  Actual type was '%s'", expected->name.string, actual->name.string);
  return false;
}

bool ValidateProcedureDeclaration(Compiler *compiler, ProcedureDeclaration *procDecl) {
  return ValidateBlock(compiler, procDecl);
}

bool ValidateBlock(Compiler *compiler, Block *block) {
  Statement *statement = block->firstStatement;
  while (statement != nullptr) {
    ValidateStatement(compiler, statement);
    statement = statement->next;
  }
  return true;
}

bool ValidateUnaryOperation(Compiler *compiler, UnaryOperation *unaryOp) {
  if (unaryOp->unaryToken == TokenType_ArrayOpen) {
    TypeInfo *exprType = &unaryOp->expression->typeInfo;
    if (exprType->indirectionLevel == 0 && exprType->arraySize == 0) {
      ReportError(compiler, unaryOp->location, "Cannot subscript into non ptr type");
      return false;
    }

    if (ValidateExpression(compiler, unaryOp->subscriptExpression) == false) return false;
    if (!IsIntegerType(unaryOp->subscriptExpression->typeInfo.type, compiler)) {
      ReportError(compiler, "Array subscript must be integer type");
      return false;
    }

    unaryOp->typeInfo.indirectionLevel -= 1;
    return true;
  }

  if (unaryOp->unaryToken == TokenType_ArrayOpen || unaryOp->unaryToken == TokenType_SymbolAddress) {
    if (unaryOp->expression->expressionType == ExpressionType_IntegerLiteral ||
        unaryOp->expression->expressionType == ExpressionType_FloatLiteral ||
        unaryOp->expression->expressionType == ExpressionType_StringLiteral) {
      ReportError(compiler, unaryOp->location, "cannot take address of literal");
      return false;
    }
  }

  if (unaryOp->unaryToken == TokenType_SymbolValue) {
    TypeInfo *exprType = &unaryOp->expression->typeInfo;
    if (exprType->indirectionLevel == 0 && exprType->arraySize == 0) {
      ReportError(compiler, unaryOp->location, "Cannot derefrence non pointer or array type");
      return false;
    }
    unaryOp->typeInfo.indirectionLevel -= unaryOp->unaryCount;
  }

  if (unaryOp->unaryToken == TokenType_LogicalNot ||
      unaryOp->unaryToken == TokenType_BitwiseNot ||
      unaryOp->unaryToken == TokenType_SymbolSub) {
    if (unaryOp->unaryCount > 1 ) {
      ReportError(compiler, unaryOp->location, "Only one unary token is permitted");
      return false;
    }
  }
  
  return true;
}