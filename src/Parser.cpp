
void NextToken(Parser *p) {
  p->token = GetToken(&p->lexer);
}

Expression *CreateExpressionInternal(Parser *p, ExpressionType type, SourceLocation location, size_t size) {
  LogVerbose(p, "ExpressionID: %d, Creating Expression(%s)", (int)p->internalExpressionCounter, ExpressionName[type]);
  Expression *expr = (Expression *)Allocate(p->astAllocator, size, 8);
  expr->expressionType = type;
  expr->expressionID = p->internalExpressionCounter++;
  expr->location = location;
  return expr;
}

Statement *CreateStatementInternal(Parser *p, StatementType type, SourceLocation location, size_t size) {
  if (location.fileID != INVALID_FILE_ID) {
    SourceFile *sourceFile = &p->compiler->sourceFiles[location.fileID];
    LogVerbose(p, "StatementID: %d, [%.*s:%d:%d] Creating Statement(%s)", (int)p->internalStatementCounter, 
      (int)sourceFile->absolutePath.length, sourceFile->absolutePath.string, (int)location.lineNumber, (int)location.columnNumber, StatementName[type]);
  } else {
    LogVerbose(p, "StatementID: %d, [INTERNAL] Creating Statement(%s)", (int)p->internalStatementCounter, StatementName[type]);
  }
  
  Statement *s = (Statement *)Allocate(p->astAllocator, size, 8);
  s->statementType = type;
  s->statementID = p->internalStatementCounter++;
  s->location = location;
  return s;
}

Identifier *CreateIdentifier(Parser *p, Token identToken) {
  Identifier *ident = (Identifier *)Allocate(p->astAllocator, sizeof(Identifier));
  ident->name = AllocateString(p->stringAllocator, identToken.text, identToken.length);
  ident->location = identToken.location;
  if (p->currentBlock->lastIdentifier != nullptr) {
    p->currentBlock->lastIdentifier->next = ident;
  }

  if (p->currentBlock->firstIdentifier == nullptr) {
    p->currentBlock->firstIdentifier = ident;
  }

  p->currentBlock->lastIdentifier = ident;
  return ident;
}

TypeDeclaration *CreateBuiltinType(Parser *p, SourceLocation location, const char *name) {
  Token token = {};
  token.text = (char *)name;
  token.length = strlen(name);
  token.location = location;
  Identifier *ident = CreateIdentifier(p, token);
  TypeDeclaration *typeDecl = CreateStatement(TypeDeclaration, token.location, p);
  typeDecl->identifier = ident;
  ident->declaration = typeDecl;
  return typeDecl;
}
 
//When this procedure is called the parser must have already
//consumed the block open token
Block *ParseCurrentBlock(Parser *p) {
  while (p->token.type != TokenType_BraceClose && p->token.type != TokenType_EndOfBuffer) {
    Statement *statement = ParseStatement(p);
    if (statement == nullptr) return nullptr;
    if (p->currentBlock->firstStatement == nullptr) {
      p->currentBlock->firstStatement = statement;
    }

    if (p->currentBlock->lastStatement == nullptr) {
      p->currentBlock->lastStatement = statement;
    } else {
      p->currentBlock->lastStatement->next = statement;
      p->currentBlock->lastStatement = p->currentBlock->lastStatement->next;;
    }
    p->currentBlock->statementCount++;
  }

  if (p->token.type == TokenType_BraceClose)
    NextToken(p); //Eat TokenType_BraceClose
  return p->currentBlock;
}


bool ParseVariableAccess(Parser *p, VariableAccess *variableAccess, TypeInfo *outTypeInfo) {
  assert(variableAccess->variable != nullptr);
  assert(variableAccess->variable->typeInfo.type != nullptr);

  uint32_t accessCount = 1; //Zero contains the variable itself
  uint32_t accessIndices[256] = {};
  Expression *subscriptExpressions[256] = {};
  //Parser has already consumed variable name
  if (p->token.type == TokenType_ArrayOpen) {
    NextToken(p); //Eat ArrayOpen
    subscriptExpressions[0] = ParseExpression(p);
    if (p->token.type != TokenType_ArrayClose) {
      ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Expected subscript close ']'");
      return false;
    }
    NextToken(p); //Eat ArrayClose
  }

  TypeInfo *currentType = &variableAccess->variable->typeInfo;
  while (p->token.type == TokenType_SymbolDot && accessCount < 256) {
    if (currentType->type->statementType != StatementType_TypeDeclaration) {
      ReportErrorC(p->compiler, FrontendErrorType_Unspecified, p->token.location, "Cannot access field " << p->token.text);
      return false;
    }

    NextToken(p); //Eat the Dot token
    if (p->token.type != TokenType_Identifier) {
      ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Expected identifier to follow '.' operator");
      return false;
    }

    Identifier *memberIdent = FindIdentifierInType(currentType->type, p->token, &accessIndices[accessCount]);
    if (memberIdent == nullptr) {
      Identifier *typeIdent = currentType->type->identifier;
      ReportErrorC(p->compiler, FrontendErrorType_Unspecified, p->token.location, "Type " << typeIdent->name.string << "has no member named " << p->token.text);
    } else {
      VariableDeclaration *memberDecl = (VariableDeclaration *)memberIdent->declaration;
      currentType = &memberDecl->typeInfo;

      NextToken(p);
    }

    if (p->token.type == TokenType_ArrayOpen) {
      NextToken(p); //Eat ArrayOpen
      subscriptExpressions[accessCount] = ParseExpression(p);
      if (p->token.type != TokenType_ArrayClose) {
        ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Expected subscript close ']'");
        return false;
      }
      NextToken(p); //Eat ArrayClose
    }

    accessCount += 1;
  }

  //User must be emitting generated source code into a source file
  //by some procedural method, or they need to seek mental health care.  
  if (accessCount >= 256) {
    //NOTE Sarcasm intended
    ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Hello user!  It looks like you might be auto-generating code and "
      "feeding it to the compiler.  If this is not the case just know that it "
      "is okay; you don't have to be alone anymore'  There are profesional "
      "mental health services availiable and there is no shame in seeking them out "
      "It takes a great deal of courage just to admit you have a problem, and even more "
      "to reach out for help.  Be Brave, Be Strong, Have Courage.  See https://www.mentalhealth.gov/get-help/immediate-help/ "
      "for more information");
    return false;
  }

  variableAccess->indices = (uint32_t *)Allocate(p->astAllocator, sizeof(uint32_t) * accessCount, 4);
  variableAccess->subscriptExpressions = (Expression **)Allocate(p->astAllocator, sizeof(Expression *) * accessCount, 8);
  variableAccess->accessCount = accessCount;
  memcpy(variableAccess->indices, accessIndices, sizeof(uint32_t) * variableAccess->accessCount);
  memcpy(variableAccess->subscriptExpressions, subscriptExpressions, sizeof(Expression *) * accessCount);
  return true;
}

bool ParseParameterInvokation(Parser *p, ParameterInvokation *params) {
  assert(p->token.type == TokenType_ParenOpen);
  NextToken(p); //Eat TokenType_ParenOpen

  Expression *currentExpr = nullptr;
  while (p->token.type != TokenType_ParenClose && p->token.type != TokenType_EndOfBuffer) {
    Expression *expr = ParseExpression(p);
    if (expr == nullptr) return false;
    if (params->firstParameterExpression == nullptr) {
      params->firstParameterExpression  = expr;
      currentExpr = expr;
    } else {
      currentExpr->next = expr;
      currentExpr = currentExpr->next;
    }        
    params->parameterExpressionCount++;
    if (p->token.type != TokenType_SymbolComma && p->token.type != TokenType_ParenClose && p->token.type != TokenType_EndOfBuffer) {
      ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Expected comma in parameter invokation");
      return false;
    }

    if (p->token.type == TokenType_SymbolComma) {
      NextToken(p);
    }
  }

  if (p->token.type == TokenType_EndOfBuffer) {
    ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Reached End of buffer while parsing params invokation");
    return false;
  }


  assert(p->token.type == TokenType_ParenClose);
  NextToken(p); //Eat TokenType_ParenClose
  return true;
}

CastExpression *ParseCastExpression(Parser *p) {
  assert(p->token.type == TokenType_KeywordCast);
  NextToken(p); //Eat TokenType_KeywordCast
  if (p->token.type != TokenType_ParenOpen) {
    ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Must open partrns after Cast keyword");
    return nullptr;
  }

  NextToken(p); //Eat the paren Open
  CastExpression *castExpr = CreateExpression(CastExpression, p->token.location, p);
  ParseTypeInfo(p, &castExpr->typeInfo);
  if (p->token.type != TokenType_ParenClose) {
    ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "must close paren after ast thing");
    return nullptr;
  }
  NextToken(p); //Eat the ParenClose
  castExpr->expression = ParsePrimaryExpression(p);
  return castExpr;
}

//When this procedure is called, the TypeIdentifier ' 'Type' { ' has been consumed
//and the parser is currently on the open brace.
static InitalizerExpression *ParseInitializerExpression(Parser *p, Token identToken) {
  assert(p->token.type == TokenType_BraceOpen);

  Identifier *ident = FindIdentifier(p->currentBlock, p->token);
  if (ident == nullptr) {
    ReportErrorC(p->compiler, FrontendErrorType_Unspecified, p->token.location, "Could not find identifier %.*s" << p->token.text);
  } else if (ident->declaration->statementType != StatementType_TypeDeclaration) {
    ReportErrorC(p->compiler, FrontendErrorType_Unspecified, identToken.location, "This is not a type");
  } else if (IsSimpleType((TypeDeclaration *)ident->declaration, p->compiler)) {
    ReportErrorC(p->compiler, FrontendErrorType_Unspecified, identToken.location, "Type is not simple");
  }

  TypeDeclaration *type = (TypeDeclaration *)ident->declaration;

  while (p->token.type != TokenType_BraceClose && p->token.type != TokenType_EndOfBuffer) {
    if (p->token.type != TokenType_Identifier) {
      ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Expected identifier");
    } else {
      Identifier *identifier = FindIdentifier(type, p->token);
      if (identifier == nullptr) {
        //TODO search for the identifier and see if we can find it in the scope
        //and report an error about what we found
        ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Type does not have identifier");
      }

      NextToken(p); //Eat TokenType_Identifier


    }
  }

  if (p->token.type == TokenType_EndOfBuffer) return nullptr;
  NextToken(p); //Eat BraceClose
  return nullptr;
}

static CallExpression *ParseCallExpression(Parser *p, Token identToken) {
  assert(p->token.type == TokenType_ParenOpen);
  CallExpression *callExpr = CreateExpression(CallExpression, identToken.location, p);
  Identifier *ident = FindIdentifier(p->currentBlock, identToken);
  if (ident == nullptr) {
    //TODO defer checking if the ident is unresolved untill validation
    ReportErrorC(p->compiler, FrontendErrorType_Unspecified, identToken.location, "Could not find identifier " << identToken.text << " when parsing call expression\n");
  } else {
    if (ident->declaration->statementType == StatementType_ProcedureDeclaration) {
      ProcedureDeclaration *procDecl = (ProcedureDeclaration *)ident->declaration;
      callExpr->params.parameterList = &procDecl->inputParameters;
      callExpr->procedure = procDecl;
    } else if (ident->declaration->statementType == StatementType_TypeDeclaration) {
      ReportErrorC(p->compiler, FrontendErrorType_Unspecified, p->token.location, "cant call type");
    } else {
      ReportErrorC(p->compiler, FrontendErrorType_Unspecified, p->token.location, "Attempting to cast to or call from variable");
    }
  }
  
  //NOTE TypeInfo for the call is not set here!  We resolve it in validation
  if (ParseParameterInvokation(p, &callExpr->params) == false) {
    return nullptr;
  }

  return callExpr;
}

Expression *ParsePrimaryExpression(Parser *p) {

  switch (p->token.type) {

    // Unary operators
    case TokenType_SymbolAddress:
    case TokenType_SymbolValue:
    case TokenType_LogicalNot:
    case TokenType_BitwiseNot:
    case TokenType_SymbolSub: {
      UnaryOperation *unaryOp = CreateExpression(UnaryOperation, p->token.location, p);
      unaryOp->unaryToken = p->token.type;
      unaryOp->unaryCount = 0;
      while (p->token.type == unaryOp->unaryToken) {
        NextToken(p); //Eat unaryToken
        unaryOp->unaryCount += 1;
      }

      unaryOp->expression = ParsePrimaryExpression(p);
      if (unaryOp->expression == nullptr) return nullptr;
      return unaryOp;
    } break;

    case TokenType_Identifier: {
      Token identToken = p->token;
      NextToken(p); //Eat identifier
      
      //This is a call expression
      if (p->token.type == TokenType_ParenOpen) {
        return ParseCallExpression(p, identToken);
      }

      //This is an type initalizer expression
      else if (p->token.type == TokenType_BraceOpen) {
        //TODO We pass the identToken to the initalizer list which handles identifier resolution and
        //checking if its a valid type.  TODO currently we are attempting to resolve the identifier once
        //we know that the expression is an identifier expression.  It is better to do it in the later stage
        //like we are doing here so that we can report a much better error about what is happening.  So right now
        //there is a duplication of work happening
        InitalizerExpression *expression = ParseInitializerExpression(p, identToken);
        //The close brace of the initalizer list has been consumed in the ParseInitializerList procedure
        return expression;
      }
      
      //This must be a constant or variable expression
      else {
        Identifier *ident = FindIdentifier(p->currentBlock, identToken);
        if (ident == nullptr) {
          ReportErrorC(p->compiler, FrontendErrorType_Unspecified, p->token.location, "Could not find identifier " << 
            p->token.text << "when parsing variable or constant expression");
        }

        assert(ident->declaration != nullptr);
        if (ident->declaration->statementType == StatementType_ConstantDeclaration) {
          ConstantExpression *ce = CreateExpression(ConstantExpression, identToken.location, p);
          ce->constant = (ConstantDeclaration *)ident->declaration;
          ce->typeInfo = ce->constant->typeInfo;
          return ce;
        }

        VariableDeclaration *varDecl = (VariableDeclaration *)ident->declaration;
        if (varDecl->statementType != StatementType_VariableDeclaration) {
          ReportErrorC(p->compiler, FrontendErrorType_Unspecified, p->token.location, "Identifier " << ident->name.string << " does not represent a variable");
          return nullptr;
        }

        assert(varDecl->typeInfo.type != nullptr);
        VariableExpression *expr = CreateExpression(VariableExpression, p->token.location, p);
        expr->variableAccess.variable = varDecl;
        ParseVariableAccess(p,  &expr->variableAccess, &expr->typeInfo);
        return expr;
      }

      assert(false);
      return nullptr;
    } break;

    case TokenType_KeywordCast: {
      return ParseCastExpression(p);
    } break;

    case TokenType_KeywordSizeOf: {
      SizeOfExpression *e = CreateExpression(SizeOfExpression, p->token.location, p);
      e->typeInfo.type = p->compiler->typeDeclU64;
      NextToken(p); // Eat TokenType_KeywordSizeOf
      if (p->token.type != TokenType_ParenOpen) {
        ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Expected copenlose paren for sizeof expr");
        return nullptr;
      }

      NextToken(p); //Eat TokenType_ParenOpen
      if (ParseTypeInfo(p, &e->sizeOfTypeInfo) == false) {
        return nullptr;
      }

      if (p->token.type != TokenType_ParenClose) {
        ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Expected close paren for sizeof expr");
        return nullptr;
      }

      NextToken(p); //Eat TokenType_ParenClose
      return e;
    } break;

    case TokenType_Integer: {
      IntegerLiteral *expr = CreateExpression(IntegerLiteral, p->token.location, p);
      expr->unsignedValue = p->token.unsignedValue;
      expr->typeInfo.type = p->compiler->typeDeclS32;
      NextToken(p); //Eat the numeric literal
      return expr;
    } break;

    case TokenType_Float: {
      FloatLiteral *expr = CreateExpression(FloatLiteral, p->token.location, p);
      expr->value = p->token.floatValue;
      expr->typeInfo.type = p->compiler->typeDeclF32;
      NextToken(p); //Eat the numeric literal
      return expr;
    } break;

    case TokenType_String: {
      StringLiteral *expr = CreateExpression(StringLiteral, p->token.location, p);
      expr->value = AllocateString(p->stringAllocator, p->token.text, p->token.length);
      expr->typeInfo.type = p->compiler->typeDeclU8;
      expr->typeInfo.indirectionLevel = 1;
      NextToken(p); //Eat string literal
      return expr;
    } break;

    case TokenType_Char: {
      IntegerLiteral *expr = CreateExpression(IntegerLiteral, p->token.location, p);
      expr->unsignedValue = p->token.unsignedValue;
      expr->typeInfo.type = p->compiler->typeDeclU8;
      NextToken(p);
      return expr;
    } break;

    case TokenType_ParenOpen: {
      NextToken(p); //Eat TokenType_ParenOpen
      Expression *result = ParseExpression(p);
      if (p->token.type != TokenType_ParenClose) {
        ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Paren expression not terminated with closing paren");
        return nullptr;
      }

      NextToken(p); //Eat TokenType_ParenClose
      return result;
    } break;

    default: {
      ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Unexpected token");
      return nullptr;
    } break;
  }

  assert(false);
  return nullptr;
}

Expression *ParseRHSExpression(Parser *p, Expression *lhs, int precedence) {
  assert(lhs != nullptr);
  while (true) {
    int tokenPrecedence = TokenPrecedence[p->token.type];
    if (tokenPrecedence < 0) return lhs;

    TokenType binaryOperator = p->token.type;
    NextToken(p); //Eat BinaryOperator
    Expression *rhs = ParsePrimaryExpression(p);
    if (rhs == nullptr) return nullptr;

    int nextTokenPrecedence = TokenPrecedence[p->token.type];
    if (tokenPrecedence < nextTokenPrecedence) rhs = ParseRHSExpression(p, rhs, tokenPrecedence + 1);
    if (rhs == nullptr) return nullptr;

    BinaryOperation *binaryOperation = CreateExpression(BinaryOperation, lhs->location, p);
    binaryOperation->binopToken = binaryOperator;
    binaryOperation->lhs = lhs;
    binaryOperation->rhs = rhs;
    lhs = binaryOperation;
  }
}

Expression *ParseExpression(Parser *p) {
  Expression *lhsExpr = ParsePrimaryExpression(p);
  if (lhsExpr == nullptr) return nullptr;
  Expression *result = ParseRHSExpression(p, lhsExpr, 0);
  return result;
}

//Declaration of structure types
//TODO torin get rid of identifier here
TypeDeclaration *ParseTypeDeclaration(Parser *p, Identifier *ident) {
  assert(p->token.type == TokenType_KeywordType);
  NextToken(p); //Eat TokenType_KeywordType
  if (p->token.type != TokenType_BraceOpen) {
    ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Expected open brace after declaring Type " << ident << "\n");
    return nullptr;
  }
  
  TypeDeclaration *typeDecl = CreateStatement(TypeDeclaration, ident->location, p);
  typeDecl->identifier = ident;
  ident->declaration = typeDecl;
  typeDecl->parent = p->currentBlock;


  p->currentBlock = typeDecl;

  NextToken(p); //Eat the block open

  Statement *currentStatement = nullptr;
  while (p->token.type != TokenType_BraceClose) {
    Statement *statement = ParseStatement(p);
    if (statement == nullptr) return nullptr;
    if (statement->statementType != StatementType_VariableDeclaration) {
      ReportErrorC(p->compiler, FrontendErrorType_Unspecified, statement->location, "Type members must be variable declarations");
      return nullptr;
    }

    if (typeDecl->firstStatement == nullptr) {
      typeDecl->firstStatement = statement;
      currentStatement = statement;
    } else {
      currentStatement->next = statement;
      currentStatement = currentStatement->next;
    }

    typeDecl->statementCount += 1;
  }

  assert(p->token.type == TokenType_BraceClose);
  p->currentBlock = typeDecl->parent;
  NextToken(p);
  return typeDecl;
}

static size_t ParseSubscriptOperator(Parser *p) {
  assert(p->token.type == TokenType_ArrayOpen);
  NextToken(p); //Eat TokenType_ArrayOpen
  if (p->token.type == TokenType_ArrayClose) {
    ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Must specifiy array size");
    return 0; //todo should be expr
  }
  
  size_t result = 0;
  if (p->token.type == TokenType_Integer) {
    result = p->token.unsignedValue;
    NextToken(p);
  } else {
    ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Array size must be numeric constant");
  }

  if (p->token.type != TokenType_ArrayClose) {
    ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Must close array");
  } else {
    NextToken(p);
  }

  return result;
}

bool ParseTypeInfo(Parser *p, TypeInfo *typeInfo) {
  if (p->token.type == TokenType_SymbolAddress) {
    while (p->token.type == TokenType_SymbolAddress) {
      typeInfo->indirectionLevel += 1;
      NextToken(p);
    }
  }

  if (p->token.type == TokenType_ArrayOpen) {
    typeInfo->arraySize = ParseSubscriptOperator(p);
  }

  if (p->token.type != TokenType_Identifier) {
    ReportErrorC(p->compiler, FrontendErrorType_Unspecified, p->token.location, "Unreconized type " << p->token.text);
    NextToken(p);
    return false;
  }

  Identifier *typeIdent = FindIdentifier(p->currentBlock, p->token);
  if (typeIdent == nullptr) {
    ReportErrorC(p->compiler, FrontendErrorType_Unspecified, p->token.location, "Could not find type " << p->token.text);
    return false;
  }

  //TODO show location of the declaration
  if (typeIdent->declaration->statementType != StatementType_TypeDeclaration) {
    ReportErrorC(p->compiler, FrontendErrorType_Unspecified, p->token.location, typeIdent->name.string << " does not name a type");
    return false;
  }

  typeInfo->type = (TypeDeclaration *)typeIdent->declaration;
  NextToken(p); //Eat type name
  return true;
}

bool ParseParameterDeclaration(Parser *p, ParameterDeclaration *params) {
  assert(p->token.type == TokenType_ParenOpen);
  NextToken(p); //Eat TokenType_ParenOpen
  VariableDeclaration *currentDecl = nullptr;
  while (p->token.type != TokenType_ParenClose && p->token.type != TokenType_EndOfBuffer) {
    Statement *s = ParseStatement(p);
    if (s->statementType != StatementType_VariableDeclaration) {
      ReportErrorC(p->compiler, FrontendErrorType_Unspecified, p->token.location, "statement in parameter declaration is not a variable declaration");
      return false;
    }

    if (params->firstParameter == nullptr) {
      params->firstParameter = (VariableDeclaration *)s;
      currentDecl = (VariableDeclaration *)s;
    } else {
      currentDecl->next = (VariableDeclaration *)s;
      currentDecl = (VariableDeclaration *)s;
    }

    params->parameterCount++;
    if (p->token.type != TokenType_SymbolComma && p->token.type != TokenType_ParenClose && p->token.type != TokenType_EndOfBuffer) {
      ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Expected comma in parameter declaration");
      return false;
    }

    if (p->token.type == TokenType_SymbolComma) {
      NextToken(p);
    }
  }

  if (p->token.type == TokenType_EndOfBuffer) {
    ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Reached end of buffer before end of parameter declaration");
    return false;
  } 

  assert(p->token.type == TokenType_ParenClose);
  NextToken(p); //Eat TokenType_ParenClose
  return true;
}

Statement *ParseProcedureDeclaration(Parser *p, Identifier *ident) {
  assert(p->token.type == TokenType_ParenOpen);
  ProcedureDeclaration *procDecl = CreateStatement(ProcedureDeclaration, ident->location, p);
  procDecl->identifier = ident;
  ident->declaration = procDecl;
  procDecl->parent = p->currentBlock;
  p->currentBlock = procDecl;

  //Parse required input parameters
  if (ParseParameterDeclaration(p, &procDecl->inputParameters) == false) {
    return nullptr;
  }

  //Parse output parameters if any are present
  if (p->token.type == TokenType_ParenOpen) {
    if (ParseParameterDeclaration(p, &procDecl->outputParameters) == false) {
      return nullptr;
    }
  }

  if (p->token.type == TokenType_KeywordForeign) {
    procDecl->isForeign = true;
    NextToken(p);
    if (p->token.type == TokenType_BraceOpen) {
      ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Foregin functions cannot have a procedure body!");
    }
  }

  if (p->token.type != TokenType_BraceOpen && procDecl->isForeign == false) {
    ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Procedure " << ident->name.string << " must have a body");
  } else if (procDecl->isForeign == false) {
    NextToken(p); //Eat TokenType_BraceOpen
    if (ParseCurrentBlock(p) == nullptr) return nullptr;    
  }

  p->currentBlock = procDecl->parent;
  return procDecl;
}


//The currentToken is past the colon so its either [], @, or typeName
VariableDeclaration *ParseVariableDeclaration(Parser *p, Identifier *ident) {
  TypeInfo typeInfo = {};
  if (ParseTypeInfo(p, &typeInfo) == false) return nullptr;
  Expression *initalExpression = nullptr;
  if (p->token.type == TokenType_SymbolEquals) {
    NextToken(p); //Eat TokenType_SymbolEquals
    initalExpression = ParseExpression(p);
  }

  VariableDeclaration *varDecl = CreateStatement(VariableDeclaration, ident->location, p);
  varDecl->identifier = ident;
  ident->declaration = varDecl;
  varDecl->typeInfo = typeInfo;
  varDecl->initalExpression = initalExpression;
  return varDecl;
}

VariableAssignment *ParseVariableAssignment(Parser *p, Token identToken) {
  VariableAssignment *varAssignment = CreateStatement(VariableAssignment, identToken.location, p);
  Identifier *ident = FindIdentifier(p->currentBlock, identToken);
  if (ident == nullptr) {
    ReportErrorC(p->compiler, FrontendErrorType_Unspecified, identToken.location, "Variable " << identToken.text << " not found in scope");
      return nullptr;
  } else {
    assert(ident->declaration != nullptr);
    varAssignment->variableAccess.variable = (VariableDeclaration *)ident->declaration;
    if (varAssignment->variableAccess.variable->statementType != StatementType_VariableDeclaration) {
      ReportErrorC(p->compiler, FrontendErrorType_Unspecified, identToken.location, identToken.text << " does not name a variable");
    }

    ParseVariableAccess(p, &varAssignment->variableAccess, &varAssignment->typeInfo);

    if (p->token.type != TokenType_SymbolEquals) {
      ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Expected '='");
      return nullptr;
    }

    NextToken(p); //Eat Equals
    varAssignment->expression = ParseExpression(p);
  }

  return varAssignment;
}

ConstantDeclaration *ParseConstantDeclaration(Parser *p, Identifier *ident) {
  ConstantDeclaration *result = CreateStatement(ConstantDeclaration, ident->location, p);
  result->identifier = ident;
  ident->declaration = result;
  result->expression = ParseExpression(p);
  if (result->expression == nullptr) return nullptr;
  result->typeInfo = result->expression->typeInfo;
  return result;
}

//The ident token has been parsed and we just gt a ParenOpen indicating
//that this is a call statement
CallStatement *ParseCallStatement(Parser *p, Token identToken) {
  assert(p->token.type == TokenType_ParenOpen);
  CallStatement *callStatement = CreateStatement(CallStatement, identToken.location, p);
  Identifier *ident = FindIdentifier(p->currentBlock, identToken);
  if (ident == nullptr) {
    ReportErrorC(p->compiler, FrontendErrorType_Unspecified, identToken.location, "Identifier " << identToken.text << "not found when trying to call procedure");
  } else {
    ProcedureDeclaration *procDecl = (ProcedureDeclaration *)ident->declaration;
    if (procDecl->statementType != StatementType_ProcedureDeclaration) {
      ReportErrorC(p->compiler, FrontendErrorType_Unspecified, identToken.location, "Identifier does not name a procedure");
    } else {
      callStatement->procedure = procDecl;
      callStatement->params.parameterList = &procDecl->inputParameters;
    }
  }

  if (ParseParameterInvokation(p, &callStatement->params) == false) {
    return nullptr;
  }
  
  return callStatement;
}

Statement *ParseIdentiferStatement(Parser *p) {
  assert(p->token.type == TokenType_Identifier);
  Token identToken = p->token;
  NextToken(p); //Eat TokenType_Identifier


  switch (p->token.type) {
    case TokenType_SymbolColon: {
      NextToken(p); //Eat single colon
      Identifier *ident = FindIdentifier(p->currentBlock, identToken);
      if (ident != nullptr) {
        //TODO WAY BETTER ERROR FOR THIS
        ReportErrorC(p->compiler, FrontendErrorType_Unspecified, p->token.location, "Cannot declare new identifier " << identToken.text);
        return nullptr;
      }

      ident = CreateIdentifier(p, identToken);
      //This is a variable declaration
      return ParseVariableDeclaration(p, ident);
    } break;

    //This is a static identifier declaration.  The identifier can
    //either be a type, procedure, or static variable
    case TokenType_DoubleColon: {
      NextToken(p); //Eat double colon
      Identifier *ident = FindIdentifier(p->currentBlock, identToken);
      if (ident != nullptr) {
        ReportErrorC(p->compiler, FrontendErrorType_Unspecified, p->token.location, "Cannot declare new identifier " << identToken.text);
        return nullptr;
      }

      ident = CreateIdentifier(p, identToken);
      if (p->token.type == TokenType_KeywordType) {
        return ParseTypeDeclaration(p, ident);
      } else if (p->token.type == TokenType_ParenOpen) {
        return ParseProcedureDeclaration(p, ident);
      } else {
        return ParseConstantDeclaration(p, ident);
      }
    } break;


    case TokenType_ParenOpen: {
      return ParseCallStatement(p, identToken);
    } break;

    case TokenType_Identifier: {
      ReportErrorC(p->compiler, FrontendErrorType_Unspecified, p->token.location, "Unexptected identifier(C style declaration?)");
      return nullptr;
    } break;

    case TokenType_ArrayOpen:
    case TokenType_SymbolEquals:
    case TokenType_SymbolDot: {
      return ParseVariableAssignment(p, identToken);
    } break;

    default: {
      
      ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Unexptected token after identifier");
      return nullptr;
    };
  }

  assert(false);
  return nullptr;
}


//TODO better brace message
WhileStatement *ParseWhileStatement(Parser *p) {
  assert(p->token.type == TokenType_KeywordWhile);
  NextToken(p); //Eat TokenType_KeywordWhile
  WhileStatement *whileStatement = CreateStatement(WhileStatement, p->token.location, p);
  whileStatement->parent = p->currentBlock;
  if (p->token.type != TokenType_ParenOpen) {
    ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Expected paren open '(' after while keyword\n");
    return nullptr;
  }

  //Parse the while condition
  NextToken(p); //Eat TokenType_ParenOpen
  whileStatement->condition = ParseExpression(p);
  if (whileStatement->condition == nullptr) return nullptr;
  if (p->token.type != TokenType_ParenClose) {
    ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Expected paren close after while condition\n");
    return nullptr;
  }

  //The while condition has been sucuessfuly parsed and now we will handle the block
  NextToken(p); //Eat TokenType_ParenClose
  if (p->token.type != TokenType_BraceOpen) {
    ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Expected block after while");
    return nullptr;
  }
  NextToken(p); //Eat TokenType_BraceOpen
  p->currentBlock = whileStatement;
  if (ParseCurrentBlock(p) == nullptr) return nullptr;
  p->currentBlock = whileStatement->parent;
  return whileStatement;
}

//TODO clean up if implementation, possibly combine into a "conditional branch" struct
IfStatement *ParseIfStatement(Parser *p) {
  assert(p->token.type == TokenType_KeywordIf);
  NextToken(p); //Eat TokenType_KeywordIf
  IfStatement *ifStatement = CreateStatement(IfStatement, p->token.location, p);
  ifStatement->parent = p->currentBlock;
  if (p->token.type != TokenType_ParenOpen) {
    ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "expected open paren after if keyword\n");
    return nullptr;
  }

  NextToken(p); //Eat TokenType_ParenOpen
  ifStatement->condition = ParseExpression(p);
  if (ifStatement->condition == nullptr) return nullptr;
  if (p->token.type != TokenType_ParenClose) {
    ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Missing close paren after if statement\n");
    return nullptr;
  }

  //The condition has been sucuessfuly parsed and now the block will be parsed
  NextToken(p); //Eat TokenType_ParenClose
  if (p->token.type != TokenType_BraceOpen) {
    ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Expected block after if statement");
    return nullptr;
  }

  NextToken(p); //Eat TokenType_BraceOpen
  p->currentBlock = ifStatement;
  if (ParseCurrentBlock(p) == nullptr) return nullptr;
  p->currentBlock = ifStatement->parent;

  ElseStatement *currentElse = nullptr;
  while (p->token.type == TokenType_KeywordElse) {
    ElseStatement *elseStatement = CreateStatement(ElseStatement, p->token.location, p);
    elseStatement->parent = p->currentBlock;
    if (currentElse == nullptr) {
      currentElse = elseStatement;
      ifStatement->elseStatement = elseStatement;
    } else {
      currentElse->nextElse = elseStatement;
    }

    NextToken(p); //Eat TokenType_KeywordElse
    if (p->token.type == TokenType_KeywordIf) {
      NextToken(p); //Eat TokenType_KeywordIf
      elseStatement->condition = ParseExpression(p);
      if (elseStatement->condition == nullptr) return nullptr;
    }

    if (p->token.type != TokenType_BraceOpen) {
      ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Expected block after if");
      return nullptr;
    }

    NextToken(p); //Eat TokenType_BraceOpen
    p->currentBlock = elseStatement;
    if (ParseCurrentBlock(p) == nullptr) return nullptr;
    p->currentBlock = elseStatement->parent; 
    currentElse = elseStatement;
  }

  return ifStatement;
}

Statement *ParseStatement(Parser *p) {
  switch (p->token.type) {
    case TokenType_Identifier: {
      return ParseIdentiferStatement(p);
    } break;

    case TokenType_KeywordImport: {
      NextToken(p); //Eat TokenType_KeywordImport
      if (p->token.type != TokenType_String) {
        ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Expected string literal after IMPORT keyword");
        return nullptr;
      }

      uint32_t fileID = AddFileToSourceFileList(p->compiler, p->fileID, p->token.text, p->token.length);
      NextToken(p); //Eat TokenType_KeywordString
      if (fileID != INVALID_FILE_ID) {
        if (ParseEntireFile(p->compiler, fileID) == false) {
            return nullptr;
        }
      }

      return ParseStatement(p);
    } break;


    case TokenType_BraceOpen: {
      assert(false);
    } break;

    case TokenType_KeywordReturn: {
      ReturnStatement *returnStatement = CreateStatement(ReturnStatement, p->token.location, p);
      NextToken(p);
      return returnStatement;
    } break;

    case TokenType_KeywordWhile: {
      return ParseWhileStatement(p);
    } break;

    case TokenType_KeywordIf: {
      return ParseIfStatement(p);
    } break;

    default: {
      ReportErrorC(p->compiler, FrontendErrorType_Syntax, p->token.location, "Unexpected token " << p->token.text << " Epxected a statement");
      NextToken(p);
      return nullptr;
    } break;
  }

  assert(false);
  return nullptr;
}

bool ParseEntireFile(Compiler *compiler, uint32_t fileID) {
  SourceFile *sourceFile = &compiler->sourceFiles[fileID];
  FILE *file = fopen(sourceFile->absolutePath.string, "r");
  if (file == nullptr) return false;
  fseek(file, 0, SEEK_END);
  size_t fileSize = ftell(file);
  fseek(file, 0, SEEK_SET);
  char *fileBuffer = (char *)malloc(fileSize + 1);
  fread(fileBuffer, fileSize, 1, file);
  fileBuffer[fileSize] = 0;
  fclose(file);

  //TODO(Torin) Initalize parser somewhere else
  Parser parser = {};
  parser.compiler = compiler;
  parser.astAllocator = &compiler->astAllocator;
  parser.stringAllocator = &compiler->stringAllocator;
  parser.logLevel = LogLevel_Info;
  parser.currentBlock = compiler->globalBlock;
  parser.fileID = fileID;
  InitalizeLexer(&parser.lexer, fileID, fileBuffer);
  NextToken(&parser); //Get the first token in the buffer
  LogInfo(&parser, "Parsing file: %s", sourceFile->absolutePath.string);
  ParseCurrentBlock(&parser);
  free(fileBuffer);

  if (compiler->errors.size() > 0) return false;
  return true;
}