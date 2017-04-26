
#include <string>
#include <vector>

struct SourceFile {
  StringReference path;
};

struct Compiler {
  std::vector<SourceFile> sourceFiles;
  std::vector<uint32_t> filesToParse;
  PersistantBlockAllocator stringAllocator;
  PersistantBlockAllocator astAllocator;

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
};

void InitalizeCompiler(Compiler *compiler);