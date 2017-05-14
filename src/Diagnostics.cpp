
void ReportError(Compiler *c, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vprintf(fmt, args);
  printf("\n");
  c->errorCount++;
}

void ReportError(Parser *p, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vprintf(fmt, args);
  printf("\n");
  p->compiler->errorCount++;
}


void ReportError(Compiler *compiler, const SourceLocation& location, const char *fmt, ...) {
  SourceFile *file = &compiler->sourceFiles[location.fileID];
  printf("\033[31;1m");
  printf("[%.*s:%d:%d] ", (int)file->path.length, file->path.string, (int)location.lineNumber, (int)location.columnNumber);
  va_list args;
  va_start(args, fmt);
  vprintf(fmt, args);
  printf("\033[0m\n");
  compiler->errorCount++;
}

void ReportError(Parser *p, const SourceLocation& location, const char *fmt, ...) {
  Compiler *compiler = p->compiler;
  SourceFile *file = &compiler->sourceFiles[location.fileID];
  printf("\033[31;1m");
  printf("[%.*s:%d:%d] ", (int)file->path.length, file->path.string, (int)location.lineNumber, (int)location.columnNumber);
  va_list args;
  va_start(args, fmt);
  vprintf(fmt, args);
  printf("\033[0m\n");
  p->compiler->errorCount++;
}

void LogInfo(Parser *p, const char *fmt, ...) {
  if (p->logLevel > LogLevel_Info) return;
  va_list args;
  va_start(args, fmt);
  printf("[Info] ");
  vprintf(fmt, args);
  printf("\n");
}

void LogVerbose(Parser *p, const char *fmt, ...) {
  if (p->logLevel > LogLevel_Verbose) return;
  va_list args;
  va_start(args, fmt);
  printf("[Verbose] ");
  vprintf(fmt, args);
  printf("\n");
}

//=================================================================================

static inline void PrintBlockSpaces(int count) {
  for (size_t i = 0; i < count; i++) {
    printf("  ");
  }
}

void PrintStatement(Statement *statement, int blockDepth) {
  PrintBlockSpaces(blockDepth);
  switch (statement->statementType) {
    case StatementType_Block: PrintBlock((Block *)statement, blockDepth); break;
    case StatementType_VariableDeclaration: PrintVariableDeclaration((VariableDeclaration *)statement); break;
    case StatementType_TypeDeclaration: PrintTypeDeclaration((TypeDeclaration *)statement, blockDepth); break;
    case StatementType_ProcedureDeclaration: PrintProcedureDeclaration((ProcedureDeclaration *)statement, blockDepth); break;
    case StatementType_VariableAssignment: PrintVariableAssignment((VariableAssignment *)statement, blockDepth); break;
    case StatementType_CallStatement: PrintCallStatement((CallStatement *)statement); break;
    case StatementType_ReturnStatement: PrintReturnStatement((ReturnStatement *)statement, blockDepth); break;

    case StatementType_WhileStatement: PrintWhileStatement((WhileStatement *)statement, blockDepth); break;
    case StatementType_IfStatement: PrintIfStatement((IfStatement *)statement, blockDepth); break;
    default: {
      assert(false);
      printf("INVALID_STATEMENT");
    } break;
  };

  //printf(" [StatementID: %u]", statement->statementID);
}

void PrintWhileStatement(WhileStatement *ws, int blockDepth) {
  printf("WHILE ");
  PrintExpression(ws->condition);
  printf("\n");
  PrintBlock(ws, blockDepth + 1);
  printf("\n");
}

void PrintIfStatement(IfStatement *is, int blockDepth) {
  printf("IF ");
  PrintExpression(is->condition);
  printf("\n");
  PrintBlock(is, blockDepth + 1);
  ElseStatement *currentElse = is->elseStatement;
  while (currentElse != nullptr) {
    PrintBlockSpaces(blockDepth);
    printf("ELSE ");
    if (currentElse->condition != nullptr) {
      printf("IF ");
      PrintExpression(currentElse->condition);
    }

    printf("\n");
    PrintBlock(currentElse, blockDepth + 1);
    currentElse = currentElse->nextElse;
  }
  printf("\n");
}

void PrintUnaryOperation(UnaryOperation *unaryOp) {
  if (unaryOp->unaryToken == TokenType_ArrayOpen) {
    printf("[");
    PrintExpression(unaryOp->subscriptExpression);
    printf("]");
  } else {
    for (size_t i = 0; i < unaryOp->unaryCount; i++)
      printf("%s", TokenString[unaryOp->unaryToken]);
  }

  PrintExpression(unaryOp->expression);
}

void PrintExpression(Expression *expression) {
  switch (expression->expressionType) {
    case ExpressionType_IntegerLiteral: PrintIntegerLiteral((IntegerLiteral *)expression); break;
    case ExpressionType_FloatLiteral: PrintFloatLiteral((FloatLiteral *)expression); break;
    case ExpressionType_StringLiteral: PrintStringLiteral((StringLiteral *)expression); break;

    case ExpressionType_VariableExpression: PrintVariableExpression((VariableExpression *)expression); break;
    case ExpressionType_CallExpression: PrintCallExpression((CallExpression *)expression); break;
    case ExpressionType_UnaryOperation: PrintUnaryOperation((UnaryOperation *)expression); break;
    case ExpressionType_BinaryOperation: PrintBinaryOperation((BinaryOperation *)expression); break;
    case ExpressionType_MemberAccessExpression: PrintMemberAccessExpression((MemberAccessExpression *)expression); break;
    case ExpressionType_CastExpression: PrintCastExpression((CastExpression *)expression); break;

    default: {
      printf("UNIMPLEMENTED PRINT EXPRESSION\n");
    } break;
  };
}

void PrintBlock(Block *block, int blockDepth) {
  Statement *currentStatement = block->firstStatement;
  while (currentStatement != nullptr) {
    PrintStatement(currentStatement, blockDepth);
    printf("\n");
    currentStatement = currentStatement->next;
  }
}

void PrintVariableDeclaration(VariableDeclaration *varDecl) {
  Identifier *varIdent = varDecl->identifier;
  Identifier *typeIdent = varDecl->typeInfo.type->identifier;
  printf("%.*s: ", (int)varIdent->name.length, varIdent->name.string);
  for (size_t i = 0; i < varDecl->typeInfo.indirectionLevel; i++) printf("@");
  printf("%.*s", (int)typeIdent->name.length, typeIdent->name.string);
  if (varDecl->typeInfo.arraySize > 0) printf("[%d]", varDecl->typeInfo.arraySize);
  if (varDecl->initalExpression != nullptr) {
    printf(" = ");
    PrintExpression(varDecl->initalExpression);
  }
}

void PrintTypeDeclaration(TypeDeclaration *typeDecl, int blockDepth) {
  Identifier *ident = typeDecl->identifier;
  if (typeDecl->firstStatement != nullptr) {
    printf("%.*s :: TYPE\n", (int)ident->name.length, ident->name.string);
    Statement *currentStatement = typeDecl->firstStatement;
    while (currentStatement != nullptr) {
      PrintStatement(currentStatement, blockDepth + 1);
      printf("\n");
      currentStatement = currentStatement->next;
    }
  } else {
    assert(false);
  }

  printf("\n");
}

void PrintParameterDeclaration(ParameterDeclaration *params) {
  printf("(");
  VariableDeclaration *current = params->firstParameter;
  while (current != nullptr) {
    PrintVariableDeclaration(current);
    current = (VariableDeclaration *)current->next;
    if (current != nullptr) printf(", ");
  }
  printf(")\n");
}

void PrintParameterInvokation(ParameterInvokation *params) {
  printf("(");
  Expression *current = params->firstParameterExpression;
  while (current != nullptr) {
    PrintExpression(current);
    current = current->next;
    if (current != nullptr) printf(", ");
  }
  printf(")\n");
}

void PrintProcedureDeclaration(ProcedureDeclaration *procDecl, int blockDepth) {
  Identifier *ident = procDecl->identifier;
  printf("%.*s :: ", (int)ident->name.length, ident->name.string);
  PrintParameterDeclaration(&procDecl->params);
  Statement *currentStatement = procDecl->firstStatement;
  while (currentStatement != nullptr) {
    PrintStatement(currentStatement, blockDepth + 1);
    printf("\n");
    currentStatement = currentStatement->next;
  }
  printf("\n");
}

void PrintVariableAssignment(VariableAssignment *varAssignment, int blockDepth) {
  Identifier *ident = varAssignment->varDecl->identifier;
  printf("%.*s", (int)ident->name.length, ident->name.string);

  TypeDeclaration *currentType = (TypeDeclaration *)varAssignment->varDecl->typeInfo.type;
  for (size_t i = 0; i < varAssignment->memberAccess.indexCount; i++) {
    VariableDeclaration *currentMember = (VariableDeclaration *)currentType->firstStatement;
    for (size_t j = 0; j < varAssignment->memberAccess.indices[i]; j++)
      currentMember = (VariableDeclaration *)currentMember->next;
    Identifier *memberIdent = currentMember->identifier;
    printf(".%.*s", (int)memberIdent->name.length, memberIdent->name.string);
    currentType = (TypeDeclaration *)currentMember->typeInfo.type;
  }

  printf(" = ");
  PrintExpression(varAssignment->expression);
}

void PrintCallStatement(CallStatement *callStatement) {
  Identifier *procIdent = callStatement->procedure->identifier;
  printf("%s", procIdent->name.string);
  PrintParameterInvokation(&callStatement->params);
}

void PrintReturnStatement(ReturnStatement *returnStatement, int blockDepth) {
  printf("RETURN ");
  PrintExpression(returnStatement->returnValue);
}

void PrintIntegerLiteral(IntegerLiteral *intLiteral) {
  printf("%lu", intLiteral->unsignedValue);
}

void PrintFloatLiteral(FloatLiteral *floatLiteral) {
  printf("%lf", floatLiteral->value);
}

void PrintStringLiteral(StringLiteral *stringLiteral) {
  printf("\"%.*s\"", (int)stringLiteral->value.length, stringLiteral->value.string);
}

void PrintVariableExpression(VariableExpression *varExpr) {
  printf("%.*s", (int)varExpr->varDecl->identifier->name.length, varExpr->varDecl->identifier->name.string);
}

void PrintCastExpression(CastExpression *castExpr) {
  Identifier *typeIdent = castExpr->typeInfo.type->identifier;
  printf("%.*s(", (int)typeIdent->name.length, typeIdent->name.string);
  PrintExpression(castExpr->expression);
  printf(")");
}

void PrintCallExpression(CallExpression *callExpr) {
  Identifier *procIdent = callExpr->procedure->identifier;
  printf("%s", procIdent->name.string);
  PrintParameterInvokation(&callExpr->params);
}

void PrintMemberAccessExpression(MemberAccessExpression *expr) {
  Identifier *ident = expr->varDecl->identifier;
  printf("%.*s", (int)ident->name.length, ident->name.string);
  TypeDeclaration *currentType = (TypeDeclaration *)expr->varDecl->typeInfo.type;
  for (size_t i = 0; i < expr->memberAccess.indexCount; i++) {
    VariableDeclaration *currentMember = (VariableDeclaration *)currentType->firstStatement;
    for (size_t j = 0; j < expr->memberAccess.indices[i]; i++)
      currentMember = (VariableDeclaration *)currentMember->next;
    Identifier *memberIdent = currentMember->identifier;
    printf(".%.*s", (int)memberIdent->name.length, memberIdent->name.string);
    currentType = (TypeDeclaration *)currentMember->typeInfo.type;
  }
}

void PrintBinaryOperation(BinaryOperation *binOp) {
  PrintExpression(binOp->lhs);
  printf(" %s ", TokenString[binOp->binopToken]);
  PrintExpression(binOp->rhs);
}