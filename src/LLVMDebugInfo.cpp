

void InitalizeCompileUnitAndFiles(LLVMCodegenerator *llvmBE) {
  Compiler *c = llvmBE->compiler;
  llvmBE->diFiles.reserve(c->sourceFiles.size());
  for (size_t i = 0; i < c->sourceFiles.size(); i++) {
    SourceFile *file = &c->sourceFiles[i];
    llvmBE->diFiles.push_back(llvmBE->dibuilder->createFile(file->absolutePath.string, file->absolutePath.string));
  }

  uint32_t lang = llvm::dwarf::DW_LANG_C;
  const char *producer = "skylang";
  bool isOptimized = false;    
  llvmBE->dibuilder->createCompileUnit(lang, llvmBE->diFiles[0], producer, isOptimized, "", 0);
}

llvm::DIType *LLVMCodegenerator::GetDIType(TypeInfo *typeInfo) {
  char *name = typeInfo->type->identifier->name.string;
  llvm::DIType *result = diTypeMap[name];
  for (auto i = 0; i < typeInfo->indirectionLevel; i++) {
    result = dibuilder->createPointerType(result, 64);
  }

  return result;
}

void LLVMCodegenerator::CreateDebugInfoForProcedure(ProcedureDeclaration *procedure) {
  llvm::DIFile *file = diFiles[procedure->location.fileID];
  llvm::DIType *parameterTypes[procedure->params.parameterCount];
  //llvm::ArrayRef<llvm::DIType *> arrayRef(parameterTypes, procedure->params.parameterCount);
  //DITypeArrayRef diArrayRef = dibuilder->getOrCreateTypeArray(arrayRef);
  //Return type is at zero index
  llvm::DISubroutineType *type = 0; //dibuilder->createSubroutineType();

  llvm::DISubprogram *subprogram = dibuilder->createFunction(
    currentDebugScope, procedure->identifier->name.string, "", file, procedure->location.lineNumber,
    type, false /* internal linkage */, true /* definition */, procedure->location.lineNumber);
}