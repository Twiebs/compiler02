
Identifier *FindIdentifierInType(TypeDeclaration *typeDecl, Token token, uint32_t *index) {
  *index = 0;
  Identifier *currentIdent = typeDecl->firstIdentifier;
  while (currentIdent != nullptr) {
    if (Equals(currentIdent->name, token.text, token.length)) {
      return currentIdent;
    }
    currentIdent = currentIdent->next;
    *index = *index + 1;
  }
  return nullptr;
}

Identifier *FindIdentifier(Block *block, Token token) {
  Block *currentBlock = block;
  while (currentBlock != nullptr) {
    Identifier *currentIdent = currentBlock->firstIdentifier;
    while (currentIdent != nullptr) {
      if (Equals(currentIdent->name, token.text, token.length)) {
        return currentIdent;
      }
      currentIdent = currentIdent->next;
    }
    currentBlock = currentBlock->parent;
  }
  return nullptr;
}

bool IsUnsignedIntegerType(TypeDeclaration *type, Compiler *compiler) {
  bool result = (type == compiler->typeDeclU8)  ||
    (type == compiler->typeDeclU16) ||
    (type == compiler->typeDeclU32) ||
    (type == compiler->typeDeclU64);
  return result;
}
bool IsSignedIntegerType(TypeDeclaration *type, Compiler *compiler) {
  bool result = (type == compiler->typeDeclS8)  ||
    (type == compiler->typeDeclS16) ||
    (type == compiler->typeDeclS32) ||
    (type == compiler->typeDeclS64);
  return result;
}

bool IsIntegerType(TypeDeclaration *type, Compiler *compiler) {
  bool result = IsSignedIntegerType(type, compiler) || IsUnsignedIntegerType(type, compiler);
  return result;
}

bool IsFloatType(TypeDeclaration *type, Compiler *compiler) {
  bool result = (type == compiler->typeDeclF32)  ||
    (type == compiler->typeDeclF64);
  return result;
}

bool IsBitwiseBinOp(TokenType type) {
  switch(type) {
    case TokenType_BitwiseAnd:
    case TokenType_BitwiseOr:
    case TokenType_BitwiseXor:
    case TokenType_BitwiseLeftShift:
    case TokenType_BitwiseRightShift:
      return true;
    default: return false;
  }
  return false;
}


TypeInfo *GetSubTypeAtIndex(TypeInfo *type, size_t index) {
  VariableDeclaration *current = (VariableDeclaration *)type->type->firstStatement;
  size_t i = 0;
  while (current != nullptr) {
    assert(current->statementType == StatementType_VariableDeclaration);
    if (i == index) {
      return &current->typeInfo;
    }
    current = (VariableDeclaration *)current->next;
    i += 1;
  }
  return nullptr;
}

bool Equals(TypeInfo *a, TypeInfo *b) {
  if (a->type != b->type) return false;
  bool result = a->indirectionLevel == b->indirectionLevel &&
    a->arraySize == b->arraySize;
  return result;
}