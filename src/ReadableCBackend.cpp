
void ReadableCBackend::CodeGenEntireAST(Compiler *c, std::ostream& stream) {
  stream << "#include <stdint.h>\n";
  stream << "#include <stddef.h>\n";
  stream << "#pragma clang diagnostic ignored \"-Wpointer-sign\"\n";
  stream << "#pragma clang diagnostic ignored \"-Wparentheses-equality\"\n";
  stream << "#pragma clang diagnostic ignored \"-Wincompatible-library-redeclaration\"\n\n";
  CodeGenBlock(c->globalBlock, stream, 0);
  stream.flush();
}

void ReadableCBackend::CodeGenStatement(Statement *statement, std::ostream& stream, int blockDepth) {
  CodeGenBlockDepthSpaces(blockDepth, stream);
  switch (statement->statementType) {
    case StatementType_Block: CodeGenBlock((Block *)statement, stream, blockDepth); break;
    case StatementType_TypeDeclaration: CodeGenTypeDeclaration((TypeDeclaration *)statement, stream, blockDepth); break;
    case StatementType_ProcedureDeclaration: CodeGenProcedureDeclaration((ProcedureDeclaration *)statement, stream, blockDepth); break;
    case StatementType_WhileStatement: CodeGenWhileStatement((WhileStatement *)statement, stream, blockDepth); break;
    case StatementType_IfStatement: CodeGenIfStatement((IfStatement *)statement, stream, blockDepth); break;

    case StatementType_VariableDeclaration: CodeGenVariableDeclaration((VariableDeclaration *)statement, stream); break;
    case StatementType_ConstantDeclaration: CodeGenConstantDeclaration((ConstantDeclaration *)statement, stream); break;
    case StatementType_VariableAssignment: CodeGenVariableAssignment((VariableAssignment *)statement, stream); break;
    case StatementType_CallStatement: CodeGenCallStatement((CallStatement *)statement, stream); break;
    case StatementType_ReturnStatement: CodeGenReturnStatement((ReturnStatement *)statement, stream); break;

    default: {
      assert(false);
    } break;
  };
}

void ReadableCBackend::CodeGenExpression(Expression *expression, std::ostream& stream) {
  switch (expression->expressionType) {
    case ExpressionType_IntegerLiteral: CodeGenIntegerLiteral((IntegerLiteral *)expression, stream); break;
    case ExpressionType_FloatLiteral: CodeGenFloatLiteral((FloatLiteral *)expression, stream); break;
    case ExpressionType_StringLiteral: CodeGenStringLiteral((StringLiteral *)expression, stream); break;

    case ExpressionType_VariableExpression: CodeGenVariableExpression((VariableExpression *)expression, stream); break;
    case ExpressionType_ConstantExpression: CodeGenConstantExpression((ConstantExpression *)expression, stream); break;
    case ExpressionType_CallExpression: CodeGenCallExpression((CallExpression *)expression, stream); break;

    case ExpressionType_UnaryOperation: CodeGenUnaryOperation((UnaryOperation *)expression, stream); break;
    case ExpressionType_BinaryOperation: CodeGenBinaryOperation((BinaryOperation *)expression, stream); break;
    case ExpressionType_CastExpression: CodeGenCastExpression((CastExpression *)expression, stream); break;

    case ExpressionType_SizeOfExpression: CodeGenSizeOfExpression((SizeOfExpression *)expression, stream); break;

    default: {
      assert(false);
      printf("UNIMPLEMENTED PRINT EXPRESSION");
    } break;
  };
}

void ReadableCBackend::CodeGenBlockDepthSpaces(int blockDepth, std::ostream& stream) {
  for (int i = 0; i < blockDepth; i++) {
    stream << "  ";
  }
}

void ReadableCBackend::CodeGenTypeNameAndPointers(TypeInfo *typeInfo, std::ostream& stream) {
  Identifier *typeIdent = typeInfo->type->identifier;

  //too lazy for compiler pointer here so excessive string compares ftw @Optimize
  if (MatchesCString(typeIdent->name, "U8")) {
    stream << "uint8_t";
  } else if (MatchesCString(typeIdent->name, "U16")) {
    stream << "uint16_t";
  } else if (MatchesCString(typeIdent->name, "U32")) {
    stream << "uint32_t";
  } else if (MatchesCString(typeIdent->name, "U64")) {
    stream << "uint64_t";
  } else if (MatchesCString(typeIdent->name, "S8")) {
    stream << "int8_t";
  } else if (MatchesCString(typeIdent->name, "S16")) {
    stream << "int16_t";
  } else if (MatchesCString(typeIdent->name, "S32")) {
    stream << "int32_t";
  } else if (MatchesCString(typeIdent->name, "S64")) {
    stream << "int64_t";
  } else if (MatchesCString(typeIdent->name, "F32")) {
    stream << "float";
  } else if (MatchesCString(typeIdent->name, "F64")) {
    stream << "double";
  } else {
    stream << typeIdent->name.string;
  }

  for (size_t i = 0; i < typeInfo->indirectionLevel; i++) stream << "*";
}

void ReadableCBackend::CodeGenTypeArraySize(TypeInfo *typeInfo, std::ostream& stream) {
  if (typeInfo->arraySize > 0)
    stream << "[" << typeInfo->arraySize << "]";
}

void ReadableCBackend::CodeGenParameterDeclaration(ParameterDeclaration *params, std::ostream& stream) {
  stream << "(";
  VariableDeclaration *current = params->firstParameter;
  while (current != nullptr) {
    CodeGenTypeNameAndPointers(&current->typeInfo, stream);
    stream << " " << current->identifier->name.string;
    current = (VariableDeclaration *)current->next;
    if (current != nullptr) stream << ", ";
  }
  stream << ")";
}

void ReadableCBackend::CodeGenParameterInvokation(ParameterInvokation *params, std::ostream& stream) {
  stream << "(";
  Expression *current = params->firstParameterExpression;
  while (current != nullptr) {
    CodeGenExpression(current, stream);
    current = current->next;
    if (current != nullptr) stream << ", ";
  }
  stream << ")";
}

void ReadableCBackend::CodeGenVariableAccess(VariableAccess *va, std::ostream& stream) {
  VariableDeclaration *currentVar = va->variable;
  for (size_t i = 0; i < va->accessCount; i++) {
    if (i != 0) {
      currentVar = GetVariableAtIndex(&currentVar->typeInfo, va->indices[i]);
    }

    stream << currentVar->identifier->name.string;
    if (va->subscriptExpressions[i] != 0) {
      stream << "[";
      CodeGenExpression(va->subscriptExpressions[i], stream);
      stream << "]";
      if (i + 1 < va->accessCount) {
        if (currentVar->typeInfo.indirectionLevel - 1 > 0) {
          stream << "->";
        } else {
          stream << ".";
        }
      }
    } else if (i + 1 < va->accessCount) {
      if (currentVar->typeInfo.indirectionLevel > 0) {
        stream << "->";
      } else {
        stream << ".";
      }
    }
  }
}
//===========================================================================================
//============================================================================================

void ReadableCBackend::CodeGenBlock(Block *block, std::ostream& stream, int blockDepth) {
  Statement *currentStatement = block->firstStatement;
  while (currentStatement != nullptr) {
    CodeGenStatement(currentStatement, stream, blockDepth);
    stream << "\n";
    currentStatement = currentStatement->next;
  }
}

void ReadableCBackend::CodeGenTypeDeclaration(TypeDeclaration *typeDecl, std::ostream& stream, int blockDepth) {
  stream << "typedef struct {\n";
  CodeGenBlock(typeDecl, stream, blockDepth + 1);
  Identifier *ident = typeDecl->identifier;
  stream << "} " << ident->name.string << ";\n";
}

void ReadableCBackend::CodeGenProcedureDeclaration(ProcedureDeclaration *procDecl, std::ostream& stream, int blockDepth) {
  ProcedureDeclaration *lastProcedure = currentProcedure;
  currentProcedure = procDecl;

  if (procDecl->outputParameters.parameterCount != 0) {
    CodeGenTypeNameAndPointers(&procDecl->outputParameters.firstParameter->typeInfo, stream);
  } else {
    stream << "void";
  }

  stream << " " << procDecl->identifier->name.string;
  CodeGenParameterDeclaration(&procDecl->inputParameters, stream);
  if (procDecl->isForeign == false) {
    stream << " {\n";
    if (procDecl->outputParameters.parameterCount != 0) {
      CodeGenBlockDepthSpaces(blockDepth + 1, stream);
      CodeGenVariableDeclaration(procDecl->outputParameters.firstParameter, stream);
      stream << "\n";
    }
    
    CodeGenBlock(procDecl, stream, blockDepth + 1);
    if (procDecl->outputParameters.parameterCount != 0) {
      stream << "  return " << procDecl->outputParameters.firstParameter->identifier->name.string << ";\n";
    }
    stream << "}\n";
  } else {
    stream << ";\n";
  }

  currentProcedure = lastProcedure;
}

void ReadableCBackend::CodeGenWhileStatement(WhileStatement *ws, std::ostream& stream, int blockDepth) {
  stream << "while (";
  CodeGenExpression(ws->condition, stream);
  stream << ") {\n";
  CodeGenBlock(ws, stream, blockDepth + 1);
  CodeGenBlockDepthSpaces(blockDepth, stream);
  stream << "}";
}

void ReadableCBackend::CodeGenIfStatement(IfStatement *is, std::ostream& stream, int blockDepth) {
  stream << "if (";
  CodeGenExpression(is->condition, stream);
  stream << ") {\n";
  CodeGenBlock(is, stream, blockDepth + 1);
  CodeGenBlockDepthSpaces(blockDepth, stream);
  stream << "} ";

  ElseStatement *currentElse = is->elseStatement;
  while (currentElse != nullptr) {
    stream << "else ";
    if (currentElse->condition != nullptr) {
      stream << "if (";
      CodeGenExpression(currentElse->condition, stream);
      stream << ")";
    }

    stream << "{\n";
    CodeGenBlock(currentElse, stream, blockDepth + 1);
    currentElse = currentElse->nextElse;
    CodeGenBlockDepthSpaces(blockDepth, stream);
    stream << "}\n";
  }
}

//====================================================

void ReadableCBackend::CodeGenVariableDeclaration(VariableDeclaration *varDecl, std::ostream& stream) {
  CodeGenTypeNameAndPointers(&varDecl->typeInfo, stream);
  stream << " " << varDecl->identifier->name.string;
  CodeGenTypeArraySize(&varDecl->typeInfo, stream);
  if (varDecl->initalExpression != nullptr) {
    stream << " = ";
    CodeGenExpression(varDecl->initalExpression, stream);
  }
  stream << ";";
}

void ReadableCBackend::CodeGenConstantDeclaration(ConstantDeclaration *c, std::ostream& stream) {
  CodeGenTypeNameAndPointers(&c->typeInfo, stream);
  stream << " " << c->identifier->name.string << " = ";
  CodeGenExpression(c->expression, stream);
  stream << ";";
}

void ReadableCBackend::CodeGenVariableAssignment(VariableAssignment *varAssignment, std::ostream& stream) {
  CodeGenVariableAccess(&varAssignment->variableAccess, stream);
  stream << " = ";
  CodeGenExpression(varAssignment->expression, stream);
  stream << ";";
}

void ReadableCBackend::CodeGenCallStatement(CallStatement *callStatement, std::ostream& stream) {
  stream << callStatement->procedure->identifier->name.string;
  CodeGenParameterInvokation(&callStatement->params, stream);
  stream << ";";
}

void ReadableCBackend::CodeGenReturnStatement(ReturnStatement *returnStatement, std::ostream& stream) {
  assert(currentProcedure != nullptr);
  if (currentProcedure->outputParameters.parameterCount > 0) {
    stream << "return " << currentProcedure->outputParameters.firstParameter->identifier->name.string << ";";
  } else {
    stream << "return;";
  }
}

//===============================================================

void ReadableCBackend::CodeGenIntegerLiteral(IntegerLiteral *intLiteral, std::ostream& stream) {
  stream << intLiteral->unsignedValue;
}

void ReadableCBackend::CodeGenFloatLiteral(FloatLiteral *floatLiteral, std::ostream& stream) {
  stream << floatLiteral->value;
}

void ReadableCBackend::CodeGenStringLiteral(StringLiteral *stringLiteral, std::ostream& stream) {
  stream << "\"";
  for (int i = 0; i < stringLiteral->value.length; i++) {
    if (stringLiteral->value.string[i] == '\n') {
      stream << "\\n";
    } else {
      stream << stringLiteral->value.string[i];
    }
  }
  stream << "\"";
}

void ReadableCBackend::CodeGenVariableExpression(VariableExpression *varExpr, std::ostream& stream) {
  CodeGenVariableAccess(&varExpr->variableAccess, stream);
}

void ReadableCBackend::CodeGenConstantExpression(ConstantExpression *ce, std::ostream& stream) {
  stream << ce->constant->identifier->name.string;
}

void ReadableCBackend::CodeGenCallExpression(CallExpression *callExpr, std::ostream& stream) {
  Identifier *procIdent = callExpr->procedure->identifier;
  stream << procIdent->name.string;
  CodeGenParameterInvokation(&callExpr->params, stream);
}

void ReadableCBackend::CodeGenBinaryOperation(BinaryOperation *binOp, std::ostream& stream) {
  stream << "(";
  CodeGenExpression(binOp->lhs, stream);
  //TODO this could differ from lang in future make sure c is seprate
  stream << " " << TokenString[binOp->binopToken] << " ";
  CodeGenExpression(binOp->rhs, stream);
  stream << ")";
}

void ReadableCBackend::CodeGenUnaryOperation(UnaryOperation *unaryOp, std::ostream& stream) {
  if (unaryOp->unaryToken != TokenType_ArrayOpen) {
    char c = 0;
    switch (unaryOp->unaryToken) {
      case TokenType_SymbolAddress: {
        c = '&';
      } break;
      case TokenType_SymbolSub: {
        c = '-';
      } break;

      case TokenType_BitwiseNot: {
        c = '~';
      } break;

      default: { assert(false); } break;
    }

    for (size_t i = 0; i < unaryOp->unaryCount; i++) {
      stream << c;
    }
  }

  CodeGenExpression(unaryOp->expression, stream);

  if (unaryOp->unaryToken == TokenType_ArrayOpen) {
    stream << "[";
    CodeGenExpression(unaryOp->subscriptExpression, stream);
    stream << "]";
  }
}

void ReadableCBackend::CodeGenCastExpression(CastExpression *castExpr, std::ostream& stream) {
  stream << "(";
  CodeGenTypeNameAndPointers(&castExpr->typeInfo, stream);
  stream << ")";
  CodeGenExpression(castExpr->expression, stream);
}

void ReadableCBackend::CodeGenSizeOfExpression(SizeOfExpression *expression, std::ostream& stream) {
  stream << "sizeof(";
  CodeGenTypeNameAndPointers(&expression->sizeOfTypeInfo, stream);
  stream << ")";
}
