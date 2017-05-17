#if 0
echo ================================================================================================================
echo ================================================================================================================
echo ================================================================================================================
clang++ -std=c++11 -g -O0 -c src/Compiler.cpp -o sky.o
clang++ -lLLVM-4.0 sky.o -o a.out
exit
#endif

#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <malloc.h>

#include <string>
#include <vector>
#include <fstream>
#include <iostream>

static const uint16_t INVALID_FILE_ID = 0xFFFF;

//TODO(Torin) Find a home for this
struct SourceLocation {
  uint32_t lineNumber;
  uint16_t columnNumber;
  uint16_t fileID;
} __attribute((packed));

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_os_ostream.h"

#include "Utility.hpp"
#include "Lexer.hpp"
#include "AST.hpp"
#include "Diagnostics.hpp"
#include "Compiler.hpp"
#include "Parser.hpp"
#include "Validation.hpp"
#include "LLVMCodegen.hpp"

#include "Utility.cpp"
#include "AST.cpp"
#include "Lexer.cpp"
#include "Parser.cpp"
#include "Validation.cpp"
#include "Diagnostics.cpp"
#include "LLVMCodegen.cpp"

void InitalizeCompiler(Compiler *compiler) {
  InitalizeAllocator(&compiler->stringAllocator, 32768);
  InitalizeAllocator(&compiler->astAllocator, 32768);
  compiler->printer.stream = &std::cout;
  compiler->printer.typeColor = TerminalColor::LightGreen;
  compiler->printer.expressionColor = TerminalColor::Blue;

  //This is a crazy hack to generate statements in a global
  //block because these procedures currently take a parser pointer
  Parser parser = {};
  parser.compiler = compiler;
  parser.astAllocator = &compiler->astAllocator;
  parser.stringAllocator = &compiler->stringAllocator;
  parser.logLevel = LogLevel_Verbose;

  SourceLocation internalDummyLocation = {};
  internalDummyLocation.fileID = INVALID_FILE_ID;
  compiler->globalBlock = CreateStatement(Block, internalDummyLocation, &parser);
  parser.currentBlock = compiler->globalBlock;
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
}

uint32_t AddFileToSourceFileList(Compiler *compiler, const char *filepath, size_t length) {
  StringReference path = AllocateString(&compiler->stringAllocator, filepath, length);
  uint32_t fileID = compiler->sourceFiles.size();
  compiler->sourceFiles.push_back(SourceFile { path });
  return fileID;
}

bool HandleCommandLineArguments(Compiler *compiler, int argc, const char **argv) {
  if (argc < 2) {
    ReportError(compiler, "Did not provide command line argument specificiying filename!");
    return false;
  } else if (argc > 2) {
    ReportError(compiler, "Too many command line arguments!  Expected one filename!");
    return false;
  }

  AddFileToSourceFileList(compiler, argv[1], strlen(argv[1]));

  return true;
}

bool RunFrontendAndBackend(Compiler *compiler) {
  ParseEntireFile(compiler, 0);
  if (ValidateBlock(compiler, compiler->globalBlock) == false) {
    return false;
  }

  if (compiler->errorCount > 0) return false;
  PrintBlock(compiler->globalBlock);
  CodegenGlobalBlock(compiler, compiler->globalBlock);
  return true;
}

int main(int argc, const char **argv) {
  Compiler compiler = {};
  InitalizeCompiler(&compiler);
  if (HandleCommandLineArguments(&compiler, argc, argv) == false) {
    return -1;
  }

  RunFrontendAndBackend(&compiler);


  return 0;
}