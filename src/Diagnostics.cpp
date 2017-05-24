
void ErrorReportBegin(Compiler *c, SourceLocation& location) {
  std::ostream& stream = std::cout;
  SourceFile *file = &c->sourceFiles[location.fileID];
  stream << "\033[31;1m" << "[" << file->absolutePath.string << ":" <<
    location.lineNumber << ":" << location.columnNumber << "] " << "\033[0m";
  c->errorCount++;;
}

void ErrorReportStreamEnd(Compiler *c) {

}

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
  printf("[%.*s:%d:%d] ", (int)file->absolutePath.length, file->absolutePath.string, (int)location.lineNumber, (int)location.columnNumber);
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
  printf("[%.*s:%d:%d] ", (int)file->absolutePath.length, file->absolutePath.string, (int)location.lineNumber, (int)location.columnNumber);
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
    case StatementType_ConstantDeclaration: PrintConstantDeclaration((ConstantDeclaration *)statement, std::cout); break;

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
    case ExpressionType_ConstantExpression: PrintConstantExpression((ConstantExpression *)expression, std::cout); break;
    case ExpressionType_CallExpression: PrintCallExpression((CallExpression *)expression); break;
    case ExpressionType_MemberAccessExpression: PrintMemberAccessExpression((MemberAccessExpression *)expression); break;

    case ExpressionType_UnaryOperation: PrintUnaryOperation((UnaryOperation *)expression); break;
    case ExpressionType_BinaryOperation: PrintBinaryOperation((BinaryOperation *)expression); break;
    case ExpressionType_CastExpression: PrintCastExpression((CastExpression *)expression); break;

    default: {
      assert(false);
      printf("UNIMPLEMENTED PRINT EXPRESSION");
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

void PrintTypeInfo(TypeInfo *typeInfo, std::ostream& stream) {
  Identifier *typeIdent = typeInfo->type->identifier;
  for (size_t i = 0; i < typeInfo->indirectionLevel; i++) stream << "@";
  if (typeInfo->arraySize > 0) stream << "[" << typeInfo->arraySize << "]";
  stream << typeIdent->name.string;
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
}

void PrintParameterDeclaration(ParameterDeclaration *params) {
  printf("(");
  VariableDeclaration *current = params->firstParameter;
  while (current != nullptr) {
    PrintVariableDeclaration(current);
    current = (VariableDeclaration *)current->next;
    if (current != nullptr) printf(", ");
  }
  printf(")");
}

void PrintParameterInvokation(ParameterInvokation *params) {
  printf("(");
  Expression *current = params->firstParameterExpression;
  while (current != nullptr) {
    PrintExpression(current);
    current = current->next;
    if (current != nullptr) printf(", ");
  }
  printf(")");
}

void PrintConstantDeclaration(ConstantDeclaration *c, std::ostream& s) {
  s << c->identifier->name.string << " :: ";
  PrintExpression(c->expression);
}

void PrintProcedureDeclaration(ProcedureDeclaration *procDecl, int blockDepth) {
  Identifier *ident = procDecl->identifier;
  printf("%.*s :: ", (int)ident->name.length, ident->name.string);
  PrintParameterDeclaration(&procDecl->params);
  if (procDecl->returnTypeInfo.type != 0) {
    printf(" >> ");
    PrintTypeInfo(&procDecl->returnTypeInfo, std::cout);
  }

  printf("\n");
  Statement *currentStatement = procDecl->firstStatement;
  while (currentStatement != nullptr) {
    PrintStatement(currentStatement, blockDepth + 1);
    printf("\n");
    currentStatement = currentStatement->next;
  }
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
  printf("CAST(");
  PrintTypeInfo(&castExpr->typeInfo, std::cout);
  printf(")");
  PrintExpression(castExpr->expression);
}

void PrintCallExpression(CallExpression *callExpr) {
  Identifier *procIdent = callExpr->procedure->identifier;
  printf("%s", procIdent->name.string);
  PrintParameterInvokation(&callExpr->params);
}

void PrintConstantExpression(ConstantExpression *ce, std::ostream& s) {
  s << ce->constant->identifier->name.string;
}

void PrintMemberAccessExpression(MemberAccessExpression *expr) {
  Identifier *ident = expr->varDecl->identifier;
  printf("%.*s", (int)ident->name.length, ident->name.string);
  TypeDeclaration *currentType = (TypeDeclaration *)expr->varDecl->typeInfo.type;
  for (size_t i = 0; i < expr->memberAccess.indexCount; i++) {
    VariableDeclaration *currentMember = (VariableDeclaration *)currentType->firstStatement;
    for (size_t j = 0; j < expr->memberAccess.indices[i]; j++)
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

//=================================================

void CodePrinter::setColor(TerminalColor color) {
  *stream << TerminalColorStrings[(int)color];
}

CodePrinter& CodePrinter::operator<<(const char *string) {
  *stream << string;
  return *this;
}

CodePrinter& CodePrinter::operator<<(Identifier *ident) {
  *stream << ident->name.string;
  return *this;
}

CodePrinter& CodePrinter::operator<<(Expression *expression) {
  switch (expression->expressionType) {
    case ExpressionType_IntegerLiteral: PrintIntegerLiteral((IntegerLiteral *)expression); break;
    case ExpressionType_FloatLiteral: PrintFloatLiteral((FloatLiteral *)expression); break;
    case ExpressionType_StringLiteral: PrintStringLiteral((StringLiteral *)expression); break;

    case ExpressionType_VariableExpression: PrintVariableExpression((VariableExpression *)expression); break;
    case ExpressionType_ConstantExpression: PrintConstantExpression((ConstantExpression *)expression, std::cout); break;
    case ExpressionType_CallExpression: PrintCallExpression((CallExpression *)expression); break;
    case ExpressionType_MemberAccessExpression: PrintMemberAccessExpression((MemberAccessExpression *)expression); break;

    case ExpressionType_UnaryOperation: PrintUnaryOperation((UnaryOperation *)expression); break;
    case ExpressionType_BinaryOperation: PrintBinaryOperation((BinaryOperation *)expression); break;
    case ExpressionType_CastExpression: *this << (CastExpression *)expression; break;
    case ExpressionType_SizeOfExpression: *this << (SizeOfExpression *)expression; break;

    default: {
      assert(false);
      printf("UNIMPLEMENTED PRINT EXPRESSION");
    } break;
  };
  return *this;
}



CodePrinter& CodePrinter::operator<<(VariableAssignment *varAssignment) {
  Identifier *ident = varAssignment->varDecl->identifier;
  *stream << ident->name.string;
  TypeDeclaration *currentType = (TypeDeclaration *)varAssignment->varDecl->typeInfo.type;
  for (size_t i = 0; i < varAssignment->memberAccess.indexCount; i++) {
    VariableDeclaration *currentMember = (VariableDeclaration *)currentType->firstStatement;
    for (size_t j = 0; j < varAssignment->memberAccess.indices[i]; j++)
      currentMember = (VariableDeclaration *)currentMember->next;
    Identifier *memberIdent = currentMember->identifier;
    *stream << "." << memberIdent->name.string;
    currentType = (TypeDeclaration *)currentMember->typeInfo.type;
  }
  return *this;
}

CodePrinter& CodePrinter::operator<<(CastExpression *cast) {
  setColor(keywordColor);
  *stream << "CAST";
  setColor(defaultColor);
  *stream << "(";
  *this << &cast->typeInfo;
  *stream << ")";
  *this << cast->expression;
  return *this;
}

CodePrinter& CodePrinter::operator<<(SizeOfExpression *expression) {
  setColor(keywordColor);
  *stream << "SIZEOF";
  setColor(defaultColor);
  *stream << "(";
  *this << &expression->sizeOfTypeInfo;
  *stream << ")";
  return *this;
}

//================================================================

CodePrinter& CodePrinter::operator<<(ParameterInvokation *params) {
  PrintParameterInvokation(params);
  return *this;
}

CodePrinter& CodePrinter::operator<<(ParameterDeclaration *params) {
  PrintParameterDeclaration(params);
  return *this;
}

CodePrinter& CodePrinter::operator<<(TypeInfo *typeInfo) {
  *stream << TerminalColorStrings[(int)typeColor];
  Identifier *typeIdent = typeInfo->type->identifier;
  for (size_t i = 0; i < typeInfo->indirectionLevel; i++) *stream << "@";
  if (typeInfo->arraySize > 0) *stream << "[" << typeInfo->arraySize << "]";
  *stream << typeIdent->name.string;
  *stream << TerminalColorStrings[(int)defaultColor];
  return *this;
}

CodePrinter& CodePrinter::operator<<(TypeMemberAccess *memberAccess) {
  return *this;
}