
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