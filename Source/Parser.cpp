
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
bool ParseTypeMemberAccess(Parser *p, VariableDeclaration *varDecl, TypeMemberAccess *memberAccess) {
  assert(p->token.type == TokenType_SymbolDot);
  assert(varDecl != nullptr);
  assert(varDecl->typeDeclaration != nullptr);

  uint32_t memberIndices[256] = {};
  size_t memberAccessDepth = 0;
  TypeDeclaration *currentType = (TypeDeclaration *)varDecl->typeDeclaration;

  while (p->token.type == TokenType_SymbolDot && memberAccessDepth < 256) {
    if (currentType->statementType != StatementType_TypeDeclaration) {
      ReportError(p, p->token.location, "Cannot access field '%.*s'",
      (int)p->token.length, p->token.text);
      return false;
    }

    NextToken(p); //Eat the Dot token
    if (p->token.type != TokenType_Identifier) {
      ReportError(p, p->token.location, "Expected identifier to follow '.' operator");
      return false;
    }

    Identifier *memberIdent = FindIdentifierInType(currentType, p->token, memberIndices + memberAccessDepth);
    if (memberIdent == nullptr) {
      Identifier *typeIdent = currentType->identifier;
      ReportError(p, p->token.location, "Type '%.*s' has no member named '%.*s'",
        (int)typeIdent->name.length, typeIdent->name.string,
        (int)p->token.length, p->token.text);
    } else {
      currentType = (TypeDeclaration *)memberIdent->declaration;
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

  memberAccess->indices = (uint32_t *)Allocate(p->astAllocator, sizeof(uint32_t)*memberAccessDepth, 4);
  memberAccess->indexCount = memberAccessDepth;
  memcpy(memberAccess->indices, memberIndices, sizeof(uint32_t)*memberAccess->indexCount);
  return true;
}

TokenType ParseSymbolicUnaryOperator(Parser *p, int *outCount) {
  assert(IsSymbolicUnaryToken(p->token.type));
  int unaryCount = 0;
  TokenType unaryToken = p->token.type;
  while (p->token.type == unaryToken) {
    NextToken(p); //Eat unaryToken
    unaryCount += 1;
  }

  *outCount = unaryCount;
  return unaryToken;
};

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



Expression *ParsePrimaryExpression(Parser *p, TokenType unaryToken = TokenType_Invalid, int unaryCount = 0) {

  switch (p->token.type) {

    //Symbolic Unary operators
    case TokenType_SymbolAddress:
    case TokenType_SymbolValue:
    case TokenType_SymbolNot:
    case TokenType_SymbolInvert: {
      assert(unaryToken == TokenType_Invalid);
      assert(unaryCount == 0);
      unaryToken = ParseSymbolicUnaryOperator(p, &unaryCount);
      while (IsSymbolicUnaryToken(p->token.type)) {
        int illegalCount = 0;
        SourceLocation location = p->token.location;
        TokenType illegalToken = ParseSymbolicUnaryOperator(p, &illegalCount);
        ReportError(p, location, "Only one unary operator token is permitted "
          "in an expression.  Surround with () if intended");
      }
      return ParsePrimaryExpression(p, unaryToken, unaryCount);
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
          CastExpression *expr = CreateExpression(CastExpression, p->token.location, p);
          expr->unaryToken = unaryToken;
          expr->unaryCount = unaryCount;
          NextToken(p); //Eat the paren Open
          expr->expression = ParseExpression(p);
          expr->typeDeclaration = (TypeDeclaration *)ident->declaration;
          assert(p->token.type == TokenType_ParenClose);
          NextToken(p); //Eat the ParenClose
          return expr;
        }

        //Must be a variable identifier which is invalid
        else {
          ReportError(p, "Attempting to cast to or call from variable");
          return nullptr;
        }
      } 
      
      //Accessing array identifier
      else if (p->token.type == TokenType_ArrayOpen) {
        assert(false);
      }

      else {
        assert(ident->declaration != nullptr);
        VariableDeclaration *varDecl = (VariableDeclaration *)ident->declaration;
        if (varDecl->statementType != StatementType_VariableDeclaration) {
          ReportError(p, "Identifier %.*s does not represent a variable",
            (int)ident->name.length, ident->name.string);
          return nullptr;
        }

        assert(varDecl->typeDeclaration != nullptr);
        if (p->token.type == TokenType_SymbolDot) {
          MemberAccessExpression *expr = CreateExpression(MemberAccessExpression, p->token.location, p);
          ParseTypeMemberAccess(p, varDecl, &expr->memberAccess);
          expr->varDecl = varDecl;
          expr->unaryToken = unaryToken;
          expr->unaryCount = unaryCount;
          return expr;
        } else {
          VariableExpression *expr = CreateExpression(VariableExpression, p->token.location, p);
          expr->unaryToken = unaryToken;
          expr->unaryCount = unaryCount;
          expr->varDecl = varDecl;
          return expr;
        }
      }

      assert(false);
      return nullptr;
    } break;

    case TokenType_Number: {
      IntegerLiteral *expr = CreateExpression(IntegerLiteral, p->token.location, p);
      expr->value = StringToInt(p->token.text, p->token.length);
      NextToken(p); //Eat the numeric literal
      return expr;
    } break;

    case TokenType_String: {
      StringLiteral *expr = CreateExpression(StringLiteral, p->token.location, p);
      expr->value = AllocateString(p->stringAllocator, p->token.text, p->token.length);
      NextToken(p); //Eat string literal
      return expr;
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

    BinaryOperation *binaryOperation = CreateExpression(BinaryOperation, p->token.location, p);
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

TypeDeclaration *ParseTypeAssignment(Parser *p, int *indirectionLevel, int *arraySize) {
  if (p->token.type == TokenType_SymbolAddress) {
    while (p->token.type == TokenType_SymbolAddress) {
      *indirectionLevel = *indirectionLevel + 1;
      NextToken(p);
    }
  }

  if (p->token.type != TokenType_Identifier) {
    ReportError(p, "Variable declaration must specifiy type");
  }

  Identifier *typeIdent = FindIdentifier(p->currentBlock, p->token);
  if (typeIdent == nullptr) {
    ReportError(p, p->token.location, "Could not find type %.*s", (int)p->token.length, p->token.text);
    return nullptr;
  }

  if (typeIdent->declaration->statementType != StatementType_TypeDeclaration) {
    ReportError(p, p->token.location, "%.*s does not name a type", 
      (int)typeIdent->name.length, typeIdent->name.string);
    return nullptr;
  }

  NextToken(p); //Eat type name

  if (p->token.type == TokenType_ArrayOpen) {
    NextToken(p);
    if (p->token.type == TokenType_ArrayClose) {
      ReportError(p, "Must specifiy array size");
    } else if (p->token.type == TokenType_Number) {
      *arraySize = StringToInt(p->token.text, p->token.length);
      NextToken(p);
    } else {
      ReportError(p, "Array size must be numeric constant");
    }

    if (p->token.type != TokenType_ArrayClose) {
      ReportError(p, "Must close array");
    } else {
      NextToken(p);
    }
  }

  if (*indirectionLevel != 0 && *arraySize != 0) {
    ReportError(p, "Cannot store pointer to static array");
  }
  return (TypeDeclaration *)typeIdent->declaration;
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
  if (p->token.type == TokenType_SymbolGreaterThan) {
    NextToken(p); //Eat TokenType_SymbolGreaterThan
    if (p->token.type == TokenType_SymbolGreaterThan) {
      NextToken(p); //Eat TokenType_SymbolGreaterThan
      procDecl->returnType = ParseTypeAssignment(p, &procDecl->returnIndirectionLevel, &procDecl->returnArraySize);
      if (procDecl->returnType == nullptr) {
        ReportError(p, p->token.location, "Could not resolve return type of procedure '%.*s'",
          (int)procDecl->identifier->name.length, procDecl->identifier->name.string);
      }
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
  int indirectionLevel = 0;
  int arraySize = 0;
  TypeDeclaration *typeDecl = ParseTypeAssignment(p, &indirectionLevel, &arraySize);

  Expression *initalExpression = nullptr;
  if (p->token.type == TokenType_SymbolEquals) {
    NextToken(p); //Eat TokenType_SymbolEquals
    initalExpression = ParseExpression(p);
  }

  VariableDeclaration *varDecl = CreateStatement(VariableDeclaration, ident->location, p);
  varDecl->identifier = ident;
  ident->declaration = varDecl;
  varDecl->typeDeclaration = typeDecl;
  varDecl->indirectionLevel = indirectionLevel;
  varDecl->arraySize = arraySize;
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
      bool sucuess = ParseTypeMemberAccess(p, varAssignment->varDecl, &varAssignment->memberAccess);
      if (sucuess == false) return nullptr;
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

    default: {
      ReportError(p, p->token.location, "Unexpected token '%.*s' Epxected a statement",
        p->token.length, p->token.text);
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
  ParseCurrentBlock(&parser);
  free(fileBuffer);

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