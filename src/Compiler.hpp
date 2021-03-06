
struct SourceFile {
  StringReference absolutePath;
};

struct BuildSettings {
  std::string projectName;
  std::string outputDirectory;
  int optimizationLevel;
  bool emitLLVMIR;
  bool emitReadableCCode;
};

struct Compiler {
  std::string currentWorkingDirectory;
  BuildSettings settings;

  std::vector<SourceFile> sourceFiles;
  PersistantBlockAllocator stringAllocator;
  PersistantBlockAllocator astAllocator;
  Allocator_Stack stack_allocator;

  //TODO move this to be per file
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

  std::vector<FrontendErrorMessage> errors;
  CodePrinter printer;
};

void InitalizeCompiler(Compiler *compiler);
uint32_t AddFileToSourceFileList(Compiler *compiler, uint16_t realitiveID, const char *filepath, size_t length);