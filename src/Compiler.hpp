
struct SourceFile {
  StringReference path;
};

struct Compiler {
  std::vector<SourceFile> sourceFiles;
  PersistantBlockAllocator stringAllocator;
  PersistantBlockAllocator astAllocator;

  Block *globalBlock;
  TypeDeclaration *typeDeclU8;
  TypeDeclaration *typeDeclU16;
  TypeDeclaration *typeDeclU32;
  TypeDeclaration *typeDeclU64;
  TypeDeclaration *typeDeclS8;
  TypeDeclaration *typeDeclS16;
  TypeDeclaration *typeDeclS32;
  TypeDeclaration *typeDeclS64;
  TypeDeclaration *typeDeclF32;
  TypeDeclaration *typeDeclF64;

  uint64_t errorCount;
};

void InitalizeCompiler(Compiler *compiler);
uint32_t AddFileToSourceFileList(Compiler *compiler, const char *filepath, size_t length);