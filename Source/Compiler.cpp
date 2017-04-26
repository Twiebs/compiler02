#if 0
echo ================================================================================================================
echo ================================================================================================================
echo ================================================================================================================
clang++ -std=c++11 -g -O0 -c Source/Compiler.cpp -o sky.o
clang++ LLVM/LLVMCore.a sky.o -o a.out
exit
#endif

#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <malloc.h>

static const uint16_t INVALID_FILE_ID = 0xFFFF;

//TODO(Torin) Find a home for this
struct SourceLocation {
  uint32_t lineNumber;
  uint16_t columnNumber;
  uint16_t fileID;
} __attribute((packed));

#include "llvm-c/Core.h"
#include "llvm-c/Analysis.h"

#include "Utility.hpp"
#include "Lexer.hpp"
#include "AST.hpp"
#include "Compiler.hpp"
#include "Diagnostics.hpp"
#include "Parser.hpp"
#include "LLVMCodegen.hpp"

#include "Utility.cpp"
#include "AST.cpp"
#include "Lexer.cpp"
#include "Parser.cpp"
#include "Diagnostics.cpp"
#include "LLVMCodegen.cpp"

void InitalizeCompiler(Compiler *compiler) {
  InitalizeAllocator(&compiler->stringAllocator, 4096);
  InitalizeAllocator(&compiler->astAllocator, 4096);
}

void AddFileToParseQueue(Compiler *compiler, const char *filepath, size_t length) {
  StringReference path = AllocateString(&compiler->stringAllocator, filepath, length);
  uint32_t fileID = compiler->sourceFiles.size();
  compiler->sourceFiles.push_back(SourceFile { path });
  compiler->filesToParse.push_back(fileID);
}

bool HandleCommandLineArguments(Compiler *compiler, int argc, const char **argv) {
  if (argc < 2) {
    ReportError(compiler, "Did not provide command line argument specificiying filename!");
    return false;
  } else if (argc > 2) {
    ReportError(compiler, "Too many command line arguments!  Expected one filename!");
    return false;
  }

  AddFileToParseQueue(compiler, argv[1], strlen(argv[1]));
  return true;
}

int main(int argc, const char **argv) {
  Compiler compiler = {};
  InitalizeCompiler(&compiler);
  if (HandleCommandLineArguments(&compiler, argc, argv) == false) {
    return -1;
  }

  ParseAllFiles(&compiler);
  return 0;
}