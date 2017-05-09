
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
      (int)sourceFile->path.length, sourceFile->path.string, (int)location.lineNumber, (int)location.columnNumber, StatementName[type]);
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
  Statement *currentStatement = nullptr;
  while (p->token.type != TokenType_BlockClose && p->token.type != TokenType_EndOfBuffer) {
    Statement *statement = ParseStatement(p);
    if (statement == nullptr) return nullptr;
    if (p->currentBlock->firstStatement == nullptr) {
        p->currentBlock->firstStatement = statement;
        currentStatement = statement;
    } else {
      currentStatement->next = statement;
      currentStatement = currentStatement->next;;
    }
    p->currentBlock->statementCount++;
  }

  if (p->token.type == TokenType_BlockClose)
    NextToken(p); //Eat TokenType_BlockClose
  return p->currentBlock;
}

//=======================================================================
//When this procedure is called the parser should store the
//first TokenType_SymbolDot token that appeared when parsing
//a type member access in either a assignment statement or
//when parsing a primary expression.  memberAccess is an out
//variable that will store the indices needed to walk to the
//correct member.

bool ParseTypeMemberAccess(Parser *p, VariableDeclaration *varDecl, TypeMemberAccess *memberAccess, TypeInfo *outTypeInfo) {
  assert(p->token.type == TokenType_SymbolDot);
  assert(varDecl != nullptr);
  assert(varDecl->typeInfo.type != nullptr);

  uint32_t memberIndices[256] = {};
  size_t memberAccessDepth = 0;
  TypeInfo *currentType = &varDecl->typeInfo;
  while (p->token.type == TokenType_SymbolDot && memberAccessDepth < 256) {
    if (currentType->type->statementType != StatementType_TypeDeclaration) {
      ReportError(p, p->token.location, "Cannot access field '%.*s'",
      (int)p->token.length, p->token.text);
      return false;
    }

    NextToken(p); //Eat the Dot token
    if (p->token.type != TokenType_Identifier) {
      ReportError(p, p->token.location, "Expected identifier to follow '.' operator");
      return false;
    }

    Identifier *memberIdent = FindIdentifierInType(currentType->type, p->token, memberIndices + memberAccessDepth);
    if (memberIdent == nullptr) {
      Identifier *typeIdent = currentType->type->identifier;
      ReportError(p, p->token.location, "Type '%.*s' has no member named '%.*s'",
        (int)typeIdent->name.length, typeIdent->name.string,
        (int)p->token.length, p->token.text);
    } else {
      VariableDeclaration *memberDecl = (VariableDeclaration *)memberIdent->declaration;
      currentType = &memberDecl->typeInfo;
      memberAccessDepth += 1;
      NextToken(p);
    }
  }

  //User must be emitting generated source code into a source file
  //by some procedural method, or they need to seek mental health care.  
  if (memberAccessDepth >= 256) {
    //NOTE Sarcasm intended
    ReportError(p, "Hello user!  It looks like you are auto-generating code and "
      "feeding it to the compiler.  If this is not the case just know that it "
      "is okay; you don't have to be alone anymore'  There are profesional "
      "mental health services availiable and there is no shame in seeking them out "
      "It takes a great deal of courage just to admit you have a problem, and even more "
      "to reach out for help.  Be Brave, Be Strong, Have Courage.  See https://www.mentalhealth.gov/get-help/immediate-help/ "
      "for more information");
    return false;
  }

  if (outTypeInfo != nullptr) *outTypeInfo = *currentType;
  memberAccess->indices = (uint32_t *)Allocate(p->astAllocator, sizeof(uint32_t)*memberAccessDepth, 4);
  memberAccess->indexCount = memberAccessDepth;
  memcpy(memberAccess->indices, memberIndices, sizeof(uint32_t)*memberAccess->indexCount);
  return true;
}

//Returns head of the argument linked list
Expression *ParseCallArguments(Parser *p, ProcedureDeclaration *procDecl, int *argCount) {
  assert(argCount != nullptr);
  assert(p->token.type == TokenType_ParenOpen);
  assert(procDecl->statementType == StatementType_ProcedureDeclaration);
  NextToken(p); //Eat the TokenType_ParenOpen              
  Expression *firstArgument = nullptr;
  Expression *currentArgument = nullptr;
  int currentArgCount = 0;
  while (p->token.type != TokenType_ParenClose && p->token.type != TokenType_EndOfBuffer) {
    Expression *expr = ParseExpression(p);
    if (expr == nullptr) return nullptr;
    if (firstArgument == nullptr) {
      firstArgument = expr;
      currentArgument = expr;
    } else {
      currentArgument->next = expr;
      currentArgument = currentArgument->next;
    }        
    currentArgCount++;
  }

  if (p->token.type == TokenType_EndOfBuffer) {
    ReportError(p, p->token.location, "Unexpected EndOfBuffer when parsing call arguments");
    return nullptr;
  }

  *argCount = currentArgCount;
  return firstArgument;
}


CastExpression *ParseCastExpression(Parser *p) {
  assert(p->token.type == TokenType_KeywordCast);
  NextToken(p); //Eat TokenType_KeywordCast
  if (p->token.type != TokenType_ParenOpen) {
    ReportError(p, "Must open partrns after Cast keyword");
    return nullptr;
  }

  NextToken(p); //Eat the paren Open
  CastExpression *castExpr = CreateExpression(CastExpression, p->token.location, p);
  ParseTypeInfo(p, &castExpr->typeInfo);
  if (p->token.type != TokenType_ParenClose) {
    ReportError(p, "must close paren after ast thing");
    return nullptr;
  }
  NextToken(p); //Eat the ParenClose
  castExpr->expression = ParsePrimaryExpression(p);
  return castExpr;
}


Expression *ParsePrimaryExpression(Parser *p) {

  switch (p->token.type) {

    // Unary operators
    case TokenType_SymbolAddress:
    case TokenType_SymbolValue:
    case TokenType_LogicalNot:
    case TokenType_BitwiseNot:
    case TokenType_ArrayOpen: 
    case TokenType_SymbolSub: {
      UnaryOperation *unaryOp = CreateExpression(UnaryOperation, p->token.location, p);
      unaryOp->unaryToken = p->token.type;
      unaryOp->unaryCount = 0;
      while (p->token.type == unaryOp->unaryToken) {
        NextToken(p); //Eat unaryToken
        unaryOp->unaryCount += 1;
      }

      if (unaryOp->unaryToken == TokenType_ArrayOpen) {
        unaryOp->subscriptExpression = ParseExpression(p);
        if (unaryOp->subscriptExpression == nullptr) {
          ReportError(p, unaryOp->location, "Invalid subscript expression");
          return nullptr;
        }

        assert(p->token.type == TokenType_ArrayClose);
        NextToken(p); //Eat TokenType_ArrayClose
      }

      unaryOp->expression = ParsePrimaryExpression(p);
      if (unaryOp->expression == nullptr) return nullptr;
      unaryOp->typeInfo = unaryOp->expression->typeInfo;
      if (unaryOp->unaryToken == TokenType_SymbolAddress)
        unaryOp->typeInfo.indirectionLevel += unaryOp->unaryCount;
      return unaryOp;
    } break;

    case TokenType_Identifier: {
      Identifier *ident = FindIdentifier(p->currentBlock, p->token);
      if (ident == nullptr) {
        ReportError(p, p->token.location, "Could not find identifier %.*s",
          p->token.length, p->token.text);
        return nullptr;
      }

      NextToken(p); //Eat identifier

      //This is either a call or cast expression
      if (p->token.type == TokenType_ParenOpen) {
        //This is a call expression
        if (ident->declaration->statementType == StatementType_ProcedureDeclaration) {
          ProcedureDeclaration *procDecl = (ProcedureDeclaration *)ident->declaration;
          int argumentCount = 0;
          Expression *firstArgument = ParseCallArguments(p, procDecl, &argumentCount);
          CallExpression *callExpr = CreateExpression(CallExpression, p->token.location, p);
          callExpr->firstArgument = firstArgument;
          callExpr->argumentCount = argumentCount;
          callExpr->procDecl = procDecl;
          assert(p->token.type == TokenType_ParenClose);
          NextToken(p); //Eat TokenType_ParenClose
          return callExpr;
        }

        //This is a cast expression
        else if (ident->declaration->statementType == StatementType_TypeDeclaration) {
          ReportError(p, "cant call type");
          return nullptr;
        }

        //Must be a variable identifier which is invalid
        else {
          ReportError(p, "Attempting to cast to or call from variable");
          return nullptr;
        }
      }
      
      else {
        assert(ident->declaration != nullptr);
        VariableDeclaration *varDecl = (VariableDeclaration *)ident->declaration;
        if (varDecl->statementType != StatementType_VariableDeclaration) {
          ReportError(p, "Identifier %.*s does not represent a variable",
            (int)ident->name.length, ident->name.string);
          return nullptr;
        }

        assert(varDecl->typeInfo.type != nullptr);

        if (p->token.type == TokenType_SymbolDot) {
          MemberAccessExpression *expr = CreateExpression(MemberAccessExpression, p->token.location, p);
          expr->varDecl = varDecl;
          ParseTypeMemberAccess(p, varDecl, &expr->memberAccess, &expr->typeInfo);
          return expr;
        } else {
          VariableExpression *expr = CreateExpression(VariableExpression, p->token.location, p);
          expr->varDecl = varDecl;
          expr->typeInfo = varDecl->typeInfo;
          return expr;
        }
      }

      assert(false);
      return nullptr;
    } break;

    case TokenType_KeywordCast: {
      return ParseCastExpression(p);
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

    case TokenType_ParenOpen: {
      NextToken(p); //Eat TokenType_ParenOpen
      Expression *result = ParseExpression(p);
      if (p->token.type != TokenType_ParenClose) {
        ReportError(p, p->token.location, "Paren expression not terminated with closing paren");
        return nullptr;
      }

      NextToken(p); //Eat TokenType_ParenClose
      return result;
    } break;



    default: assert(false);
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

TypeDeclaration *ParseTypeDeclaration(Parser *p, Identifier *ident) {
  assert(p->token.type == TokenType_KeywordType);
  NextToken(p);
  if (p->token.type != TokenType_BlockOpen) {
    ReportError(p, "Expected new block after type declaration");
    return nullptr;
  }
  
  TypeDeclaration *typeDecl = CreateStatement(TypeDeclaration, ident->location, p);
  typeDecl->identifier = ident;
  ident->declaration = typeDecl;
  typeDecl->parent = p->currentBlock;


  p->currentBlock = typeDecl;

  NextToken(p); //Eat the block open

  Statement *currentStatement = nullptr;
  while (p->token.type != TokenType_BlockClose) {
    Statement *statement = ParseStatement(p);
    if (statement == nullptr) return nullptr;
    if (statement->statementType != StatementType_VariableDeclaration) {
      ReportError(p, "Type members must be variable declarations");
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

  assert(p->token.type == TokenType_BlockClose);
  p->currentBlock = typeDecl->parent;
  NextToken(p);
  return typeDecl;
}

static size_t ParseSubscriptOperator(Parser *p) {
  assert(p->token.type == TokenType_ArrayOpen);
  NextToken(p); //Eat TokenType_ArrayOpen
  if (p->token.type == TokenType_ArrayClose) {
    ReportError(p, "Must specifiy array size");
    return 0; //todo should be expr
  } 
  
  size_t result = 0;
  if (p->token.type == TokenType_Integer) {
    result = p->token.unsignedValue;
    NextToken(p);
  } else {
    ReportError(p, "Array size must be numeric constant");
  }

  if (p->token.type != TokenType_ArrayClose) {
    ReportError(p, "Must close array");
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
    ReportError(p, p->token.location, "Unreconized type '%.*s'",
      (int)p->token.length, p->token.text);
    NextToken(p);
    return false;
  }

  Identifier *typeIdent = FindIdentifier(p->currentBlock, p->token);
  if (typeIdent == nullptr) {
    ReportError(p, p->token.location, "Could not find type %.*s", (int)p->token.length, p->token.text);
    return false;
  }

  if (typeIdent->declaration->statementType != StatementType_TypeDeclaration) {
    ReportError(p, p->token.location, "%.*s does not name a type", 
      (int)typeIdent->name.length, typeIdent->name.string);
    return false;
  }

  typeInfo->type = (TypeDeclaration *)typeIdent->declaration;
  NextToken(p); //Eat type name
  return true;
}

Statement *ParseProcedureDeclaration(Parser *p, Identifier *ident) {
  assert(p->token.type == TokenType_ParenOpen);
  NextToken(p);

  ProcedureDeclaration *procDecl = CreateStatement(ProcedureDeclaration, ident->location, p);
  procDecl->identifier = ident;
  ident->declaration = procDecl;
  procDecl->parent = p->currentBlock;
  p->currentBlock = procDecl;

  VariableDeclaration *currentDecl = nullptr;
  while (p->token.type != TokenType_ParenClose) {
    Statement *s = ParseStatement(p);
    if (s->statementType != StatementType_VariableDeclaration) {
      ReportError(p, "statement in procedure arguments is not a variable decl");
    }

    if (procDecl->firstArgument == nullptr) {
      procDecl->firstArgument = (VariableDeclaration *)s;
      currentDecl = (VariableDeclaration *)s;
    } else {
      currentDecl->next = (VariableDeclaration *)s;
      currentDecl = (VariableDeclaration *)s;
    }

    procDecl->argumentCount++;
  }


  assert(p->token.type == TokenType_ParenClose);
  NextToken(p); //Eat TokenType_ParenClose
  if (p->token.type == TokenType_BitwiseRightShift) {
    NextToken(p); //Eat TokenType_BitwiseRightShift
    if (ParseTypeInfo(p, &procDecl->returnTypeInfo) == false) {
      ReportError(p, p->token.location, "Could not resolve return type of procedure '%.*s'",
        (int)procDecl->identifier->name.length, procDecl->identifier->name.string);
      return nullptr;
    }
  }

  if (p->token.type == TokenType_KeywordForeign) {
    procDecl->isForeign = true;
    NextToken(p);
    if (p->token.type == TokenType_BlockOpen) {
      ReportError(p, "Foregin functions cannot have a procedure body!");
    }
  }

  if (p->token.type != TokenType_BlockOpen && procDecl->isForeign == false) {
    ReportError(p, p->token.location, "Procedure '%.*s' must have a body",
      (int)ident->name.length, ident->name.string);
  } else if (procDecl->isForeign == false) {
    NextToken(p); //Eat blockopen
    ParseCurrentBlock(p);    
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

Statement *ParseIdentifierDecl(Parser *p, Token identToken) {
  Identifier *ident = FindIdentifier(p->currentBlock, identToken);
  if (ident != nullptr) {
    ReportError(p, "Cannot declare new identifier %.*s", identToken.length, identToken.text);
    return nullptr;
  }

  ident = CreateIdentifier(p, identToken);

  //This is a static identifier declaration.  The identifier can
  //either be a type, procedure, or static variable
  if (p->token.type == TokenType_SymbolColon) {
    NextToken(p);
    if (p->token.type == TokenType_KeywordType) {
      return ParseTypeDeclaration(p, ident);
    } else if (p->token.type == TokenType_ParenOpen) {
      return ParseProcedureDeclaration(p, ident);
    }
  }

  //This is a variable declaration
  return ParseVariableDeclaration(p, ident);



  assert(false);
  return nullptr;
}

VariableAssignment *ParseVariableAssignment(Parser *p, Token identToken) {
  VariableAssignment *varAssignment = CreateStatement(VariableAssignment, identToken.location, p);
  Identifier *ident = FindIdentifier(p->currentBlock, identToken);
  if (ident == nullptr) {
    ReportError(p, identToken.location, "Variable '%.*s' not found in scope",
      (int)identToken.length, identToken.text);
  } else {
    assert(ident->declaration != nullptr);
    varAssignment->varDecl = (VariableDeclaration *)ident->declaration;
    if (varAssignment->varDecl->statementType != StatementType_VariableDeclaration) {
      ReportError(p, identToken.location, "%.*s does not name a variable",
        (int)identToken.length, identToken.text);
    }

    if (p->token.type == TokenType_SymbolDot) {
      bool sucuess = ParseTypeMemberAccess(p, varAssignment->varDecl, &varAssignment->memberAccess, &varAssignment->typeInfo);
      if (sucuess == false) return nullptr;
    } else {
      varAssignment->typeInfo = varAssignment->varDecl->typeInfo;
    }

    if (p->token.type != TokenType_SymbolEquals) {
      ReportError(p, p->token.location, "Expected '='");
      return nullptr;
    }

    NextToken(p);
    varAssignment->expression = ParseExpression(p);
  }

  return varAssignment;
}


Statement *ParseIdentiferStatement(Parser *p) {
  assert(p->token.type == TokenType_Identifier);
  Token identToken = p->token;
  NextToken(p);
  switch (p->token.type) {
    case TokenType_SymbolColon: {
      NextToken(p); //Eat single colon
      return ParseIdentifierDecl(p, identToken);
    } break;

    case TokenType_SymbolEquals:
    case TokenType_SymbolDot: {
      return ParseVariableAssignment(p, identToken);
    } break;

    case TokenType_ParenOpen: {
      Identifier *ident = FindIdentifier(p->currentBlock, identToken);
      if (ident == nullptr) {
        ReportError(p, identToken.location, "Identifier not found");
        return nullptr;
      }

      ProcedureDeclaration *procDecl = (ProcedureDeclaration *)ident->declaration;
      if (procDecl->statementType != StatementType_ProcedureDeclaration) {
        ReportError(p, identToken.location, "Identifier does not name a procedure");
        return nullptr;
      }

      int argumentCount = 0;
      Expression *firstArgument = ParseCallArguments(p, procDecl, &argumentCount);
      CallStatement *callStatement = CreateStatement(CallStatement, identToken.location, p);
      callStatement->procedure = procDecl;
      callStatement->firstArgument = firstArgument;
      callStatement->argumentCount = argumentCount;
      assert(p->token.type == TokenType_ParenClose);
      NextToken(p); //Eat TokenType_ParenClose
      return callStatement;
    } break;

    default: assert(false);
  }

  assert(false);
  return nullptr;
}

WhileStatement *ParseWhileStatement(Parser *p) {
  assert(p->token.type == TokenType_KeywordWhile);
  NextToken(p); //Eat TokenType_KeywordWhile
  WhileStatement *whileStatement = CreateStatement(WhileStatement, p->token.location, p);
  whileStatement->parent = p->currentBlock;
  whileStatement->condition = ParseExpression(p);
  if (whileStatement->condition == nullptr) return nullptr;
  if (p->token.type != TokenType_BlockOpen) {
    ReportError(p, p->token.location, "Expected block after while");
    return nullptr;
  }

  NextToken(p); //Eat TokenType_BlockOpen
  p->currentBlock = whileStatement;
  if (ParseCurrentBlock(p) == nullptr) return nullptr;
  p->currentBlock = whileStatement->parent;
  return whileStatement;
}

IfStatement *ParseIfStatement(Parser *p) {
  assert(p->token.type == TokenType_KeywordIf);
  NextToken(p); //Eat TokenType_KeywordIf
  IfStatement *ifStatement = CreateStatement(IfStatement, p->token.location, p);
  ifStatement->parent = p->currentBlock;
  ifStatement->condition = ParseExpression(p);
  if (ifStatement->condition == nullptr) return nullptr;
  if (p->token.type != TokenType_BlockOpen) {
    ReportError(p, p->token.location, "Expected block after while");
    return nullptr;
  }

  NextToken(p); //Eat TokenType_BlockOpen
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

    if (p->token.type != TokenType_BlockOpen) {
      ReportError(p, p->token.location, "Expected block after if");
      return nullptr;
    }

    NextToken(p); //Eat TokenType_BlockOpen
    p->currentBlock = elseStatement;
    if (ParseCurrentBlock(p) == nullptr) return nullptr;
    p->currentBlock = elseStatement->parent; 
    currentElse = elseStatement;
  }

  return ifStatement;
}

Statement *ParseStatement(Parser *p) {
  switch(p->token.type) {
    case TokenType_Identifier: {
      return ParseIdentiferStatement(p);
    } break;


    case TokenType_BlockOpen: {
      assert(false);
    } break;

    case TokenType_KeywordReturn: {
      ReturnStatement *returnStatement = CreateStatement(ReturnStatement, p->token.location, p);
      NextToken(p);
      returnStatement->returnValue = ParseExpression(p);
      return returnStatement;
    } break;

    case TokenType_KeywordWhile: {
      return ParseWhileStatement(p);
    } break;

    case TokenType_KeywordIf: {
      return ParseIfStatement(p);
    } break;

    default: {
      ReportError(p, p->token.location, "Unexpected token '%.*s' Epxected a statement",
        p->token.length, p->token.text);
      NextToken(p);
      return nullptr;
    } break;
  }

  assert(false);
  return nullptr;
}

void DebugPrintAllTokens(Parser *p) {
  while (p->token.type != TokenType_EndOfBuffer) {
    printf("%u (%s) %.*s\n", p->token.tokenID, TokenName[p->token.type], (int)p->token.length, p->token.text);
    NextToken(p);
  }
}

bool ParseEntireFile(Compiler *compiler, uint32_t fileID) {
  SourceFile *sourceFile = &compiler->sourceFiles[fileID];
  FILE *file = fopen(sourceFile->path.string, "r");
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
  parser.logLevel = LogLevel_Verbose;
  SourceLocation internalDummyLocation = {};
  internalDummyLocation.fileID = INVALID_FILE_ID;

  Block *globalBlock = CreateStatement(Block, internalDummyLocation, &parser);
  parser.currentBlock = globalBlock;
  compiler->typeDeclU8 = CreateBuiltinType(&parser, internalDummyLocation, "U8");
  compiler->typeDeclU16 = CreateBuiltinType(&parser, internalDummyLocation, "U16");
  compiler->typeDeclU32 = CreateBuiltinType(&parser, internalDummyLocation, "U32");
  compiler->typeDeclU64 = CreateBuiltinType(&parser, internalDummyLocation, "U64");
  compiler->typeDeclS8 = CreateBuiltinType(&parser, internalDummyLocation, "S8");
  compiler->typeDeclS16 = CreateBuiltinType(&parser, internalDummyLocation, "S16");
  compiler->typeDeclS32 = CreateBuiltinType(&parser, internalDummyLocation, "S32");
  compiler->typeDeclS64 = CreateBuiltinType(&parser, internalDummyLocation, "S64");
  compiler->typeDeclF32 = CreateBuiltinType(&parser, internalDummyLocation, "F32");
  compiler->typeDeclF64 = CreateBuiltinType(&parser, internalDummyLocation, "F64");
  InitalizeLexer(&parser.lexer, fileID, fileBuffer);
  NextToken(&parser); //Get the first token in the buffer
  LogInfo(&parser, "Parsing file: %s", sourceFile->path.string);
  ParseCurrentBlock(&parser);
  free(fileBuffer);

  if (compiler->errorCount > 0) return false;
  ValidateBlock(compiler, globalBlock);
  if (compiler->errorCount > 0) return false;
  PrintBlock(globalBlock);
  CodegenGlobalBlock(compiler, globalBlock);
  return true;
}

bool ParseAllFiles(Compiler *compiler) {
  while (compiler->filesToParse.size() > 0) {
    uint32_t fileID = compiler->filesToParse.back();
    compiler->filesToParse.pop_back();
    if (ParseEntireFile(compiler, fileID) == false) {
      return false;
    }
  }
  return true;
}