
struct Compiler;

enum StatementType {
  StatementType_Invalid,
  StatementType_Block,

  StatementType_VariableDeclaration,
  StatementType_TypeDeclaration,
  StatementType_ProcedureDeclaration,
  StatementType_ConstantDeclaration,

  StatementType_VariableAssignment,
  StatementType_CallStatement,
  StatementType_ReturnStatement,

  StatementType_WhileStatement,
  StatementType_IfStatement,
  StatementType_ElseStatement,
};

const char *StatementName[] = {
  "StatementType_Invalid",
  "StatementType_Block",

  "StatementType_VariableDeclaration",
  "StatementType_TypeDeclaration",
  "StatementType_ProcedureDeclaration",
  "StatementType_ConstantDeclaration",

  "StatementType_VariableAssignment",
  "StatementType_CallStatement",
  "StatementType_ReturnStatement",

  "StatementType_WhileStatement",
  "StatementType_IfStatement",
  "StatementType_ElseStatement",
};

enum ExpressionType {
  ExpressionType_Invalid,

  ExpressionType_IntegerLiteral,
  ExpressionType_FloatLiteral,
  ExpressionType_StringLiteral,

  ExpressionType_VariableExpression,
  ExpressionType_ConstantExpression,
  ExpressionType_CallExpression,
  ExpressionType_UnaryOperation,
  ExpressionType_BinaryOperation,

  ExpressionType_CastExpression,

  ExpressionType_SizeOfExpression,
};

const char *ExpressionName[] = {
  "ExpressionType_Invalid",

  "ExpressionType_IntegerLiteral",
  "ExpressionType_FloatLiteral",
  "ExpressionType_StringLiteral",

  "ExpressionType_VariableExpression",
  "ExpressionType_ConstantExpression",
  "ExpressionType_CallExpression",
  "ExpressionType_UnaryOperation",
  "ExpressionType_BinaryOperation",
  "ExpressionType_CastExpression",

  "ExpressionType_SizeOfExpression"
};

struct Statement {
  StatementType statementType;
  uint32_t statementID;
  SourceLocation location;
  Statement *next;
};

struct Identifier {
  StringReference name;
  SourceLocation location;
  Statement *declaration;
  Identifier *next;
};

struct Block : Statement {
  Identifier *firstIdentifier;
  Identifier *lastIdentifier;
  Statement *firstStatement;
  Statement *lastStatement;
  uint32_t statementCount;
  Block *parent;
};

struct TypeDeclaration : Block {
  Identifier *identifier;
  llvm::Type *llvmType;
};


struct TypeInfo {
  TypeDeclaration *type;
  bool isArray;
  int indirectionLevel;
  int arraySize;
};

//This struct is used for VariableAssigments and variable
//expressions.  It shares the code for acessing struct members
//And offseting by the subscript operator

struct Expression {
  ExpressionType expressionType;
  SourceLocation location;
  uint32_t expressionID;

  TypeInfo typeInfo;
  Expression *next;
};

//=====================================================


struct WhileStatement : Block {
  Expression *condition;
};

struct ElseStatement;


//TODO Consolidate if and else statement @Refactor
struct IfStatement : Block {
  Expression *condition;
  ElseStatement *elseStatement;
};

struct ElseStatement : Block {
  Expression *condition;
  ElseStatement *nextElse;
};

struct VariableDeclaration : Statement {
  Identifier *identifier;
  TypeInfo typeInfo;
  Expression *initalExpression;
  llvm::Value *llvmAlloca;
};

struct AccessInfo {
  int index;
  SourceLocation location;
  Expression *subscriptExpression;
};

struct VariableAccess {
  VariableDeclaration *variable;
  int accessCount;
  uint32_t *indices;
  Expression **subscriptExpressions;
};


struct ConstantDeclaration : Statement {
  Identifier *identifier;
  TypeInfo typeInfo;
  Expression *expression;
  void *backendPointer;
};

struct ParameterDeclaration {
  VariableDeclaration *firstParameter;
  int parameterCount;
};

struct ParameterInvokation {
  ParameterDeclaration *parameterList;
  Expression *firstParameterExpression;
  int parameterExpressionCount;
};

struct ProcedureDeclaration : Block {
  Identifier *identifier;
  ParameterDeclaration params;
  TypeInfo returnTypeInfo;
  bool isForeign;

  llvm::Function *llvmFunction;
};

struct VariableAssignment : Statement {
  VariableAccess variableAccess;
  TypeInfo typeInfo; //TypeOf final value
  Expression *expression;
};

struct CallStatement : Statement {
  ProcedureDeclaration *procedure;
  ParameterInvokation params;
};

struct ReturnStatement : Statement {
  Expression *returnValue;
};

//Expressions

struct BinaryOperation : Expression {
  TokenType binopToken;
  Expression *lhs;
  Expression *rhs;
};

struct UnaryOperation : Expression {
  TokenType unaryToken;
  uint32_t unaryCount;
  Expression *subscriptExpression;
  Expression *expression;
};

struct VariableExpression : Expression {
  VariableAccess variableAccess;
};

struct ConstantExpression : Expression {
  ConstantDeclaration *constant;
};

struct SizeOfExpression : Expression {
  TypeInfo sizeOfTypeInfo;
};

struct IntegerLiteral : Expression {
   uint64_t unsignedValue;
};

struct FloatLiteral : Expression {
  double value;
};

struct StringLiteral : Expression {
  StringReference value;
};

struct CastExpression : Expression {
  Expression *expression;
};

struct CallExpression : Expression {
  ProcedureDeclaration *procedure;
  ParameterInvokation params;
};

bool IsUnsignedIntegerType(TypeDeclaration *type, Compiler *compiler);
bool IsSignedIntegerType(TypeDeclaration *type, Compiler *compiler);
bool IsIntegerType(TypeDeclaration *type, Compiler *compiler);
bool IsFloatType(TypeDeclaration *type, Compiler *compiler);

bool IsLiteralExpression(Expression *e);

TypeInfo *GetSubTypeAtIndex(TypeInfo *type, size_t index);
VariableDeclaration *GetVariableAtIndex(TypeInfo *type, size_t index);
bool IsBitwiseBinOp(TokenType type);

bool Equals(TypeInfo *a, TypeInfo *b);