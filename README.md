This is a simple toy compiler created for experimental purposes.  
It implements a straight foward "c-like" toy language with a minimalistic syntax and feature set.
The compiler is designed to be as simple as possible.

# Toy Language Specification

variable_declaration: Int32 = 0

Type_Declaration :: TYPE { field0: Uint8 }

#Procedures
Return values are named and implicitly return when the procedure exits.  The `RETURN` keyword can
be used to exit the procedure imeddiatly.  It takes no arguments.
procedure_declaration :: (argument_a: Int32)(return_value_a: Float32) { ... }

There is an example program in the exaples directory of a basic software rasterizer
to demonstrate the usage of the language in a more than basic context.
The code can be found [here](https://github.com/Twiebs/compiler02/examples/software_rasterizer.src)
