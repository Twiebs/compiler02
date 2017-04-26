
void ReportError(Compiler *c, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vprintf(fmt, args);
  printf("\n");
}

void ReportError(Parser *p, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vprintf(fmt, args);
  printf("\n");
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
    case StatementType_VariableDeclaration: PrintVariableDeclaration((VariableDeclaration *)statement, blockDepth); break;
    case StatementType_TypeDeclaration: PrintTypeDeclaration((TypeDeclaration *)statement, blockDepth); break;
    case StatementType_ProcedureDeclaration: PrintProcedureDeclaration((ProcedureDeclaration *)statement, blockDepth); break;
    case StatementType_VariableAssignment: PrintVariableAssignment((VariableAssignment *)statement, blockDepth); break;
    case StatementType_CallStatement: PrintCallStatement((CallStatement *)statement); break;
    case StatementType_ReturnStatement: PrintReturnStatement((ReturnStatement *)statement, blockDepth); break;
    default: {
      printf("INVALID_STATEMENT");
    } break;
  };

  //printf(" [StatementID: %u]", statement->statementID);
}

void PrintUnaryOperators(Expression *expression) {
  for (size_t i = 0; i < expression->unaryCount; i++)
    printf("%s", TokenString[expression->unaryToken]);
}

void PrintExpression(Expression *expression) {
  PrintUnaryOperators(expression);
  switch (expression->expressionType) {
    case ExpressionType_IntegerLiteral: PrintIntegerLiteral((IntegerLiteral *)expression); break;
    case ExpressionType_FloatLiteral: PrintFloatLiteral((FloatLiteral *)expression); break;
    case ExpressionType_StringLiteral: PrintStringLiteral((StringLiteral *)expression); break;

    case ExpressionType_VariableExpression: PrintVariableExpression((VariableExpression *)expression); break;
    case ExpressionType_CallExpression: PrintCallExpression((CallExpression *)expression); break;
    case ExpressionType_BinaryOperation: PrintBinaryOperation((BinaryOperation *)expression); break;
    case ExpressionType_MemberAccessExpression: PrintMemberAccessExpression((MemberAccessExpression *)expression); break;
    case ExpressionType_CastExpression: PrintCastExpression((CastExpression *)expression); break;
    default: {
      printf("INVALID_EXPRESSION\n");
    } break;
  };
}

void PrintBlock(Block *block, int blockDepth) {
  Statement *currentStatement = block->firstStatement;
  while (currentStatement != nullptr) {
    PrintStatement(currentStatement);
    currentStatement = currentStatement->next;
  }
}

void PrintVariableDeclaration(VariableDeclaration *varDecl, int blockDepth) {
  Identifier *varIdent = varDecl->identifier;
  Identifier *typeIdent = varDecl->typeDeclaration->identifier;
  printf("%.*s: ", (int)varIdent->name.length, varIdent->name.string);
  for (size_t i = 0; i < varDecl->indirectionLevel; i++) printf("@");
  printf("%.*s", (int)typeIdent->name.length, typeIdent->name.string);
  if (varDecl->arraySize > 0) printf("[%d]", varDecl->arraySize);
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

void PrintProcedureDeclaration(ProcedureDeclaration *procDecl, int blockDepth) {
  Identifier *ident = procDecl->identifier;
  printf("%.*s :: (", (int)ident->name.length, ident->name.string);
  VariableDeclaration *currentArgument = procDecl->firstArgument;
  while (currentArgument != nullptr) {
    PrintVariableDeclaration(currentArgument, blockDepth);
    currentArgument = (VariableDeclaration *)currentArgument->next;
    if (currentArgument != nullptr) printf(", ");
  }

  printf(")\n");
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

  TypeDeclaration *currentType = (TypeDeclaration *)varAssignment->varDecl->typeDeclaration;
  for (size_t i = 0; i < varAssignment->memberAccess.indexCount; i++) {
    VariableDeclaration *currentMember = (VariableDeclaration *)currentType->firstStatement;
    for (size_t j = 0; j < varAssignment->memberAccess.indices[i]; j++)
      currentMember = (VariableDeclaration *)currentMember->next;
    Identifier *memberIdent = currentMember->identifier;
    printf(".%.*s", (int)memberIdent->name.length, memberIdent->name.string);
    currentType = (TypeDeclaration *)currentMember->typeDeclaration;
  }

  printf(" = ");
  PrintExpression(varAssignment->expression);
}

void PrintCallStatement(CallStatement *callStatement) {
  Identifier *procIdent = callStatement->procedure->identifier;
  printf("%s(", procIdent->name.string);
  Expression *currentArgument = callStatement->firstArgument;
  while (currentArgument != nullptr) {
    PrintExpression(currentArgument);
    printf(" ");
    currentArgument = currentArgument->next;
  }
  printf(")");
}

void PrintReturnStatement(ReturnStatement *returnStatement, int blockDepth) {
  printf("RETURN ");
  PrintExpression(returnStatement->returnValue);
}

void PrintIntegerLiteral(IntegerLiteral *intLiteral) {
  printf("%ld", intLiteral->value);
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
  Identifier *typeIdent = castExpr->typeDeclaration->identifier;
  printf("%.*s(", (int)typeIdent->name.length, typeIdent->name.string);
  PrintExpression(castExpr->expression);
  printf(")");
}

void PrintCallExpression(CallExpression *callExpr) {
  Identifier *procIdent = callExpr->procDecl->identifier;
  printf("%.*s(", (int)procIdent->name.length, procIdent->name.string);
  Expression *currentArgument = callExpr->firstArgument;
  while (currentArgument != nullptr) {
    PrintExpression(currentArgument);
    currentArgument = currentArgument->next;
    if (currentArgument != nullptr) printf(", ");
  }
  printf(")");
}

void PrintMemberAccessExpression(MemberAccessExpression *expr) {
  Identifier *ident = expr->varDecl->identifier;
  printf("%.*s", (int)ident->name.length, ident->name.string);
  TypeDeclaration *currentType = (TypeDeclaration *)expr->varDecl->typeDeclaration;
  for (size_t i = 0; i < expr->memberAccess.indexCount; i++) {
    VariableDeclaration *currentMember = (VariableDeclaration *)currentType->firstStatement;
    for (size_t j = 0; j < expr->memberAccess.indices[i]; i++)
      currentMember = (VariableDeclaration *)currentMember->next;
    Identifier *memberIdent = currentMember->identifier;
    printf(".%.*s", (int)memberIdent->name.length, memberIdent->name.string);
    currentType = (TypeDeclaration *)currentMember->typeDeclaration;
  }
}

void PrintBinaryOperation(BinaryOperation *binOp) {
  PrintExpression(binOp->lhs);
  printf(" %s ", TokenString[binOp->binopToken]);
  PrintExpression(binOp->rhs);
}