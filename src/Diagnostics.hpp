
struct Parser;
struct Compiler;

enum LogLevel {
  LogLevel_None,
  LogLevel_Verbose,
  LogLevel_Info,
};

enum class TerminalColor {
  None,
  Default,
  Red,
  Cyan,
  Blue,
  Magenta,
  LightGreen,
};

static const char *TerminalColorStrings[] = {
  "", //None
  "\033[0m", //Default
  "\033[31;1m", //Red
  "\033[36;1m", //Cyan
  "\033[34;1m", //Blue
  "\033[35;1m", //Magenta
  "\033[92;1m", //LightGreen
};

static const char *TERMINAL_COLOR_RED = "\033[31;1m";
static const char *TERMINAL_COLOR_CYAN = "\033[36;1m";
static const char *TERMINAL_COLOR_BLUE = "\033[34;1m";
static const char *TERMINAL_COLOR_LIGHTGREEN = "\033[92;1m";
static const char *TERMINAL_COLOR_DEFAULT = "\033[0m";

//Designing the error and warning API is very nontrivial.  I want
//the option to multithread the frontend without making huge architectural
//changes to the way parsing and vailidation works, so the API
//needs to be written with mutex aquire/release semantics in mind.
//The implementation should also be abstract enough to foward the
//messages to different output sources such as stdout/stderr, a
//file, a user provided buffer, etc.  This is important because any tools
//using the compiler may wish to intercept error messages with arbitrary
//levels of control in straight foward way.  All of this needs to be acomplished
//while still keeping the API simple and transparent from the parsers
//perspective

//User level logging routines that notify user of errors and warnings
//found while parsing or analyizing the code
void ReportError(Compiler *c, const char *fmt, ...);
void ReportError(Parser *p, const char *fmt, ...);
void ReportWarning(Parser *p, const char *fmt, ...);
void ReportError(Compiler *c, const SourceLocation& location, const char *fmt, ...);
void ReportError(Parser *p, const SourceLocation& location, const char *fmt, ...);
void ReportWarning(Parser *p, const SourceLocation& location, const char *fmt, ...);

//There does not seem to be a nice way to make this api without resorting
//to dirty macro hacks.  These routines exist so that the frontend can
//more eaisly be transitioned into a multithreaded pass if desired.
void ErrorReportBegin(Compiler *c, SourceLocation& location);
void ErrorReportStreamEnd(Compiler *c);
#define ReportErrorC(c,l, msg) ErrorReportBegin(c, l); c->printer << msg

//Internal logging routines for developer use and only
void LogInfo(Parser *p, const char *fmt, ...);
void LogVerbose(Parser *p, const char *fmt, ...);

//This class wraps a abstract std::ostream and provides
//pretty printing of the AST as source code.  This is used
//in error reporting.
class CodePrinter {

  void setColor(TerminalColor color);

public:
  std::ostream *stream;

  TerminalColor typeColor;
  TerminalColor variableColor;
  TerminalColor expressionColor;
  TerminalColor defaultColor;
  TerminalColor keywordColor;

  //Basic types stream fowarder
  CodePrinter& operator<<(const char *string);
  
  //Expression printers
  CodePrinter& operator<<(Expression *expression);
  CodePrinter& operator<<(VariableExpression *expression);
  CodePrinter& operator<<(CastExpression *expression);
  CodePrinter& operator<<(CallExpression *expression);
  CodePrinter& operator<<(SizeOfExpression *expression);
  CodePrinter& operator<<(UnaryOperation *expression);
  CodePrinter& operator<<(BinaryOperation *binOp);

  //Single statement code printer procedures
  CodePrinter& operator<<(VariableAssignment *varAssign);
  CodePrinter& operator<<(VariableDeclaration *varDecl);
  
  //Utility Non AST Print Routines
  CodePrinter& operator<<(Identifier *ident);
  CodePrinter& operator<<(TypeInfo *typeInfo);
  CodePrinter& operator<<(ParameterInvokation *params);
  CodePrinter& operator<<(ParameterDeclaration *params);
  CodePrinter& operator<<(VariableAccess *variableAccess);
};

//This class wraps a abstract std::ostream and provides
//pretty printing of the AST in a xml like tree representation.
class ASTPrinter {
public:
  ASTPrinter(std::ostream& s);

private:
  std::ostream& stream;
  TerminalColor statementColor;
  TerminalColor expressionColor;
  void setCurrentColor(TerminalColor color);
};

//Debug procedures to pretty print AST represntation of
//parsed data structures to verifiy parsing is correct
void PrintStatement(Statement *statement, int blockDepth);
void PrintBlock(Block *block, int blockDepth = 0);

void PrintTypeDeclaration(TypeDeclaration *typeDecl, int blockDepth = 0);
void PrintProcedureDeclaration(ProcedureDeclaration *procDecl, int blockDepth = 0);
void PrintVariableDeclaration(VariableDeclaration *varDecl);
void PrintConstantDeclaration(ConstantDeclaration *c, std::ostream& s);

void PrintCallStatement(CallStatement *callStatement);
void PrintReturnStatement(ReturnStatement *returnStatement, int blockDepth = 0);
void PrintWhileStatement(WhileStatement *ws, int blockDepth = 0);
void PrintIfStatement(IfStatement *is, int blockDepth = 0);

void PrintExpression(Expression *expression);
void PrintIntegerLiteral(IntegerLiteral *intLiteral);
void PrintFloatLiteral(FloatLiteral *floatLiteral);
void PrintStringLiteral(StringLiteral *stringLiteral);

void PrintConstantExpression(ConstantExpression *ce, std::ostream& s);

void PrintCallExpression(CallExpression *callExpr);
void PrintBinaryOperation(BinaryOperation *binOp);
void PrintUnaryOperation(UnaryOperation *unaryOp);
void PrintCastExpression(CastExpression *castExpr);

void PrintParameterDeclaration(ParameterDeclaration *params);
void PrintParameterInvokation(ParameterInvokation *params);
void PrintTypeInfo(TypeInfo *typeInfo, std::ostream& stream);