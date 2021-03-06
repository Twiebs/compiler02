
void ErrorReportBegin(Compiler *c, FrontendErrorType type, SourceLocation& location) {
  SourceFile *file = &c->sourceFiles[location.fileID];
  stream << "\033[31;1m" << "[" << file->absolutePath.string << ":" <<
    location.lineNumber << ":" << location.columnNumber << "] " << "\033[0m";
  FrontendErrorMessage error = {};
  error.type = type;
  error.location = location;
  stream >> error.message;
  c->errors.push_back(error);
}

void ReportInternalError(Compiler *c, const char *fmt) {
  printf("%s", fmt);
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

    case ExpressionType_ConstantExpression: PrintConstantExpression((ConstantExpression *)expression, std::cout); break;
    case ExpressionType_CallExpression: PrintCallExpression((CallExpression *)expression); break;

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
  PrintParameterDeclaration(&procDecl->inputParameters);
  //TODO removed outputs

  printf("\n");
  Statement *currentStatement = procDecl->firstStatement;
  while (currentStatement != nullptr) {
    PrintStatement(currentStatement, blockDepth + 1);
    printf("\n");
    currentStatement = currentStatement->next;
  }
}


void PrintCallStatement(CallStatement *callStatement) {
  Identifier *procIdent = callStatement->procedure->identifier;
  printf("%s", procIdent->name.string);
  PrintParameterInvokation(&callStatement->params);
}

void PrintReturnStatement(ReturnStatement *returnStatement, int blockDepth) {
  printf("RETURN");
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

void PrintBinaryOperation(BinaryOperation *binOp) {
  PrintExpression(binOp->lhs);
  printf(" %s ", TokenString[binOp->binopToken]);
  PrintExpression(binOp->rhs);
}

//=================================================

void CodePrinter::setColor(TerminalColor color) {
  allocator_stack_push_string_literal(stack_allocator, TerminalColorStrings[(int)color]);
}

CodePrinter& CodePrinter::operator<<(const char *string) {
  //TODO cleaner version of this
  allocator_stack_push_size(stack_allocator,string, strlen(string));
  return *this;
}

CodePrinter& CodePrinter::operator<<(Identifier *ident) {
  allocator_stack_push_size(stack_allocator, ident->name.string, ident->name.length);
  return *this;
}

CodePrinter& CodePrinter::operator<<(Expression *expression) {
  switch (expression->expressionType) {
    case ExpressionType_IntegerLiteral: PrintIntegerLiteral((IntegerLiteral *)expression); break;
    case ExpressionType_FloatLiteral: PrintFloatLiteral((FloatLiteral *)expression); break;
    case ExpressionType_StringLiteral: PrintStringLiteral((StringLiteral *)expression); break;

    case ExpressionType_VariableExpression: *this << (VariableExpression *)expression; break;
    case ExpressionType_ConstantExpression: PrintConstantExpression((ConstantExpression *)expression, std::cout); break;
    case ExpressionType_CallExpression: *this << (CallExpression *)expression; break;

    case ExpressionType_UnaryOperation: *this << (UnaryOperation *)expression; break;
    case ExpressionType_BinaryOperation: *this << (BinaryOperation *)expression; break;
    case ExpressionType_CastExpression: *this << (CastExpression *)expression; break;
    case ExpressionType_SizeOfExpression: *this << (SizeOfExpression *)expression; break;

    default: {
      assert(false);
      printf("UNIMPLEMENTED PRINT EXPRESSION");
    } break;
  };
  return *this;
}

CodePrinter& CodePrinter::operator<<(VariableDeclaration *varDecl) {
  Identifier *varIdent = varDecl->identifier;
  setColor(variableColor);
  *this << varIdent;
  setColor(defaultColor);
  allocator_stack_push_string_literal(stack_allocator, ": ");
  *this << &varDecl->typeInfo;
  if (varDecl->initalExpression != nullptr) {
    allocator_stack_push_string_literal(stack_allocator, " = ");
    *this << varDecl->initalExpression;
  }
  return *this;
}


CodePrinter& CodePrinter::operator<<(VariableAssignment *varAssignment) {
  *this << &varAssignment->variableAccess;
  allocator_stack_push_string_literal(stack_allocator, " = ");
  *this << varAssignment->expression;
  return *this;
}

CodePrinter& CodePrinter::operator<<(VariableExpression *expression) {
  *this << &expression->variableAccess;
  return *this;
}

CodePrinter& CodePrinter::operator<<(CastExpression *cast) {
  setColor(keywordColor);
  allocator_stack_push_string_literal(stack_allocator, "CAST");
  setColor(defaultColor);
  allocator_stack_push_string_literal(stack_allocator, "(");
  *this << &cast->typeInfo;
  allocator_stack_push_string_literal(stack_allocator, ")");
  *this << cast->expression;
  return *this;
}

CodePrinter& CodePrinter::operator<<(CallExpression *callExpr) {
  if (callExpr->procedure == nullptr) return *this;
  Identifier *procIdent = callExpr->procedure->identifier;
  *this << procIdent;
  *this << &callExpr->params;
  return *this;
}

CodePrinter& CodePrinter::operator<<(SizeOfExpression *expression) {
  setColor(keywordColor);
  allocator_stack_push_string_literal(stack_allocator, "SIZEOF");
  setColor(defaultColor);
  allocator_stack_push_string_literal(stack_allocator, "(");
  *this << &expression->sizeOfTypeInfo;
  allocator_stack_push_string_literal(stack_allocator, ")");
  return *this;
}


CodePrinter& CodePrinter::operator<<(UnaryOperation *unaryOp) {
  for (size_t i = 0; i < unaryOp->unaryCount; i++)
    allocator_stack_push_string_literal(stack_allocator, TokenString[unaryOp->unaryToken]);
  *this << unaryOp->expression;
  return *this;
}

CodePrinter& CodePrinter::operator<<(BinaryOperation *binOp) {
  *this << binOp->lhs;
  allocator_stack_push_string_literal(stack_allocator, " ");
  allocator_stack_push_string_literal(stack_allocator, TokenString[binOp->binopToken]);
  allocator_stack_push_string_literal(stack_allocator, " ");
  *this << binOp->rhs;
  return *this;
}

//================================================================

CodePrinter& CodePrinter::operator<<(ParameterInvokation *params) {
  allocator_stack_push_string_literal(stack_allocator, "(");
  Expression *current = params->firstParameterExpression;
  while (current != nullptr) {
    *this << current;
    current = current->next;
    if (current != nullptr) allocator_stack_push_string_literal(stack_allocator, ", ");
  }
  allocator_stack_push_string_literal(stack_allocator, ")");
  return *this;
}

CodePrinter& CodePrinter::operator<<(ParameterDeclaration *params) {
  allocator_stack_push_string_literal(stack_allocator, "(");
  VariableDeclaration *current = params->firstParameter;
  while (current != nullptr) {
    *this << current;
    current = (VariableDeclaration *)current->next;
    if (current != nullptr) allocator_stack_push_string_literal(stack_allocator, ", ");
  }

  allocator_stack_push_string_literal(stack_allocator, ")");
  return *this;
}

CodePrinter& CodePrinter::operator<<(TypeInfo *typeInfo) {
  setColor(typeColor);
  Identifier *typeIdent = typeInfo->type->identifier;
  for (size_t i = 0; i < typeInfo->indirectionLevel; i++) { 
    allocator_stack_push_string_literal(stack_allocator, "@");
  }

  if (typeInfo->arraySize > 0) { 
    allocator_stack_push_string_literal(stack_allocator, "[");
    allocator_stack_push_string_fmt(stack_allocator, "%d", typeInfo->arraySize);
    allocator_stack_push_string_literal(stack_allocator, "]");
  }

  *this << typeIdent->name.string;
  setColor(defaultColor);
  return *this;
}

CodePrinter& CodePrinter::operator<<(VariableAccess *va) {
  VariableDeclaration *var = va->variable;
  for (int i = 0; i < va->accessCount; i++) {
    setColor(variableColor);
    *this << var->identifier;
    setColor(defaultColor);
    if (va->subscriptExpressions[i] != nullptr) {
      *this << va->subscriptExpressions[i];
    }

    if (i + 1 < va->accessCount) {
      var = GetVariableAtIndex(&var->typeInfo, va->indices[i + 1]);
      allocator_stack_push_string_literal(stack_allocator, ".");
    }
  }

  return *this;
}

