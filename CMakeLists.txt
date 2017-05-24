set(CMAKE_CXX_COMPILER clang++)
add_compile_options(-std=c++11)

set(SOURCE_FILES 
  		src/Compiler.cpp 
			src/Utility.cpp
  		src/Lexer.cpp
  		src/Parser.cpp
			src/Validation.cpp
			src/Diagnostics.cpp
			src/LLVMCodegen.cpp
      src/AST.cpp
)

add_executable(sky ${SOURCE_FILES})
target_link_libraries(sky LLVM)