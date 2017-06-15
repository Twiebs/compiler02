set(CMAKE_CXX_COMPILER clang++)
add_compile_options(-std=c++11)

set(SOURCE_FILES 
      src/AST.cpp
  		src/Compiler.cpp 
      src/Diagnostics.cpp
      src/Lexer.cpp
      src/LLVMCodegen.cpp
      src/LLVMDebugInfo.cpp
      src/Parser.cpp
      src/ReadableCBackend.cpp
			src/Utility.cpp
			src/Validation.cpp
)

add_executable(sky ${SOURCE_FILES})
target_link_libraries(sky LLVM)