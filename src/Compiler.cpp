#if 0
echo ================================================================================================================
echo ================================================================================================================
echo ================================================================================================================
clang++ -std=c++11 -g -O0 -c src/Compiler.cpp -o compiler.o
clang++ -lLLVM-4.0 compiler.o -o compiler
exit
#endif

//TODO Error when not finding import file

#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <malloc.h>

#include <string>
#include <vector>
#include <fstream>
#include <iostream>
#include <sstream>

#include <unistd.h>


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
#include "ReadableCBackend.hpp"

#include "Utility.cpp"
#include "AST.cpp"
#include "Lexer.cpp"
#include "Parser.cpp"
#include "Validation.cpp"
#include "Diagnostics.cpp"
#include "LLVMCodegen.cpp"
#include "ReadableCBackend.cpp"

void InitalizeCompiler(Compiler *compiler) {
  InitalizeAllocator(&compiler->stringAllocator, 32768);
  InitalizeAllocator(&compiler->astAllocator, 32768);
  compiler->printer.stream = &std::cout;
  compiler->printer.typeColor = TerminalColor::LightGreen;
  compiler->printer.variableColor = TerminalColor::Blue;
  compiler->printer.expressionColor = TerminalColor::Blue;
  compiler->printer.defaultColor = TerminalColor::Default;
  compiler->printer.keywordColor = TerminalColor::Magenta;

  //TODO Make a safer version of this
  char cwd[1024] = {};
  if (getcwd(cwd, sizeof(cwd)) == 0) {
    printf("failed to getcwd\n");
    return;
  }

  compiler->currentWorkingDirectory = std::string(cwd);
  compiler->currentWorkingDirectory.append("/");

  //TODO make sure otput directory exists
  BuildSettings& settings = compiler->settings;
  settings.projectName = "project";
  settings.outputDirectory = "output/";
  settings.emitLLVMIR = true;
  settings.emitReadableCCode = true;
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
  compiler->typeDeclU8 = CreateBuiltinType(&parser,  internalDummyLocation,  "Any");
  compiler->typeDeclU8 = CreateBuiltinType(&parser,  internalDummyLocation,  "U8");
  compiler->typeDeclU16 = CreateBuiltinType(&parser, internalDummyLocation, "U16");
  compiler->typeDeclU32 = CreateBuiltinType(&parser, internalDummyLocation, "U32");
  compiler->typeDeclU64 = CreateBuiltinType(&parser, internalDummyLocation, "U64");
  compiler->typeDeclS8 = CreateBuiltinType(&parser,  internalDummyLocation,  "S8");
  compiler->typeDeclS16 = CreateBuiltinType(&parser, internalDummyLocation, "S16");
  compiler->typeDeclS32 = CreateBuiltinType(&parser, internalDummyLocation, "S32");
  compiler->typeDeclS64 = CreateBuiltinType(&parser, internalDummyLocation, "S64");
  compiler->typeDeclF32 = CreateBuiltinType(&parser, internalDummyLocation, "F32");
  compiler->typeDeclF64 = CreateBuiltinType(&parser, internalDummyLocation, "F64");
}



uint32_t AddFileToSourceFileList(Compiler *compiler, uint16_t relativeFileID, const char *filepath, size_t length) {
  if (compiler->sourceFiles.size() == INVALID_FILE_ID) {
    ReportError(compiler, "Reached max include count");
    return INVALID_FILE_ID;
  }

  if (length == 0) return INVALID_FILE_ID;
  StringReference absolutePath = {};

  //This is the input file and must be realitive to the CWD or is an absolute path
  if (compiler->sourceFiles.size() == 0) {
    assert(relativeFileID == INVALID_FILE_ID);
    char *concat = (char *)alloca(compiler->currentWorkingDirectory.length() + length + 2);
    memcpy(concat, compiler->currentWorkingDirectory.data(), compiler->currentWorkingDirectory.length());
    memcpy(concat + compiler->currentWorkingDirectory.length(), filepath, length);
    concat[compiler->currentWorkingDirectory.length() + length] = 0;
    char realPathBuffer[PATH_MAX] = {};
    char *result = realpath(concat, realPathBuffer);
    if (result != realPathBuffer) {
      ReportError(compiler, "Error resolving absolute path");
      return INVALID_FILE_ID;
    }

    absolutePath = AllocateString(&compiler->stringAllocator, realPathBuffer, strlen(realPathBuffer));
  } else {
    if (filepath[0] == '/') {
      absolutePath = AllocateString(&compiler->stringAllocator, filepath, length);
    } else {
      SourceFile *relativeFile = &compiler->sourceFiles[relativeFileID];
      StringReference& relativeFilePath = relativeFile->absolutePath;
      size_t newLength = 0;
      if (string_index_of_char_from_back('/', relativeFilePath.string, relativeFilePath.length, &newLength) == false) {
        assert(false);
      }
      newLength += 1;

      char *concat = (char *)alloca(newLength + length + 1);
      memcpy(concat, relativeFilePath.string, newLength);
      memcpy(concat + newLength, filepath, length);
      concat[newLength + length] = 0;
      char realPathBuffer[PATH_MAX] = {};
      char *result = realpath(concat, realPathBuffer);
      if (result != realPathBuffer) {
        ReportError(compiler, "Error resolving absolute path");
        return INVALID_FILE_ID;
      }

      absolutePath = AllocateString(&compiler->stringAllocator, realPathBuffer, strlen(realPathBuffer));
    }
  }

  for (size_t i = 0; i < compiler->sourceFiles.size(); i++) {
    SourceFile *file = &compiler->sourceFiles[i];
    if (Equals(file->absolutePath, absolutePath)) {
      return INVALID_FILE_ID;
    }
  }

  uint32_t fileID = compiler->sourceFiles.size();
  compiler->sourceFiles.push_back(SourceFile { absolutePath });
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

  AddFileToSourceFileList(compiler, INVALID_FILE_ID, argv[1], strlen(argv[1]));

  return true;
}

bool BuildProject(Compiler *compiler) {
  ParseEntireFile(compiler, 0);
  if (ValidateBlock(compiler, compiler->globalBlock) == false) {
    return false;
  }


  if (compiler->errors.size() > 0) return false;
  CodegenGlobalBlock(compiler, compiler->globalBlock);

  if (compiler->settings.emitReadableCCode) {
    printf("Emitting Readable C Code\n");
    TemporaryString s("%s%s_cbackend.c", compiler->settings.outputDirectory.c_str(), compiler->settings.projectName.c_str());
    auto stream = std::ofstream(s.getString());
    ReadableCBackend readableCBackend = {};
    readableCBackend.CodeGenEntireAST(compiler, stream);

    //Emit the LLVM IR for the C Backend
    s.set("clang -O0 -S -emit-llvm %s%s_cbackend.c -o %s%s_cbackend.ll", 
      compiler->settings.outputDirectory.c_str(), compiler->settings.projectName.c_str(), 
      compiler->settings.outputDirectory.c_str(), compiler->settings.projectName.c_str());
    system(s.getString());

    s.set("clang -O0 -g -c %s%s_cbackend.c -o %s%s_cbackend.o", 
      compiler->settings.outputDirectory.c_str(), compiler->settings.projectName.c_str(), 
      compiler->settings.outputDirectory.c_str(), compiler->settings.projectName.c_str());
    system(s.getString());

    s.set("clang -lSDL2 -lm %s%s_cbackend.o -o %s%s_cbackend",
      compiler->settings.outputDirectory.c_str(), compiler->settings.projectName.c_str(), 
      compiler->settings.outputDirectory.c_str(), compiler->settings.projectName.c_str());
    system(s.getString());
    
    //system("objdump -M intel -d test_software > test_software.dump");
  }



  return true;
}

int main(int argc, const char **argv) {
  Compiler compiler = {};
  InitalizeCompiler(&compiler);
  if (HandleCommandLineArguments(&compiler, argc, argv) == false) {
    return -1;
  }

  BuildProject(&compiler);


  return 0;
}