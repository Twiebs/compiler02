
enum StatementType {
  StatementType_Invalid,
  StatementType_Block,

  StatementType_VariableDeclaration,
  StatementType_TypeDeclaration,
  StatementType_ProcedureDeclaration,

  StatementType_VariableAssignment,
  StatementType_CallStatement,
  StatementType_ReturnStatement,
};

const char *StatementName[] = {
  "StatementType_Invalid",
  "StatementType_Block",

  "StatementType_VariableDeclaration",
  "StatementType_TypeDeclaration",
  "StatementType_ProcedureDeclaration",

  "StatementType_VariableAssignment",
  "StatementType_CallStatement",
  "StatementType_ReturnStatement",
};

enum ExpressionType {
  ExpressionType_Invalid,

  ExpressionType_IntegerLiteral,
  ExpressionType_FloatLiteral,
  ExpressionType_StringLiteral,

  ExpressionType_VariableExpression,
  ExpressionType_CallExpression,
  ExpressionType_BinaryOperation,
  ExpressionType_MemberAccessExpression,
  ExpressionType_CastExpression,
};

const char *ExpressionName[] = {
  "ExpressionType_Invalid",

  "ExpressionType_IntegerLiteral",
  "ExpressionType_FloatLiteral",
  "ExpressionType_StringLiteral",

  "ExpressionType_VariableExpression",
  "ExpressionType_CallExpression",
  "ExpressionType_BinaryOperation",
  "ExpressionType_MemberAccessExpression",
  "ExpressionType_CastExpression",
};

struct Statement {
  StatementType statementType;
  uint32_t statementID;
  SourceLocation location;
  Statement *next;
};

struct Expression {
  ExpressionType expressionType;
  uint32_t expressionID;
  SourceLocation location;
  TokenType unaryToken;
  int unaryCount;
  Expression *next;
};

struct Identifier {
  StringReference name;
  SourceLocation location;
  Statement *declaration;
  Identifier *next;
};

struct TypeMemberAccess {
  uint32_t indexCount;
  uint32_t *indices;
};

//Statements
struct Block : Statement {
  Identifier *firstIdentifier;
  Identifier *lastIdentifier;
  Statement *firstStatement;
  uint32_t statementCount;
  Block *parent;
};

struct TypeDeclaration : Block {
  Identifier *identifier;
  LLVMTypeRef llvmType;
};

struct VariableDeclaration : Statement {
  TypeDeclaration *typeDeclaration;
  Identifier *identifier;
  Expression *initalExpression;
  bool isArray;
  int indirectionLevel;
  int arraySize;
  LLVMValueRef llvmAlloca;
};

struct ProcedureDeclaration : Block {
  Identifier *identifier;
  VariableDeclaration *firstArgument;
  TypeDeclaration *returnType;
  int returnIndirectionLevel;
  int returnArraySize;
  bool isForeign;
  uint32_t argumentCount;
  LLVMValueRef llvmFunction;
};

struct VariableAssignment : Statement {
  VariableDeclaration *varDecl;
  int variableDepth;
  Expression *expression;
  TypeMemberAccess memberAccess;
};

struct CallStatement : Statement {
  ProcedureDeclaration *procedure;
  Expression *firstArgument;
  int argumentCount;
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

struct MemberAccessExpression : Expression {
  VariableDeclaration *varDecl;
  TypeMemberAccess memberAccess;
};

struct VariableExpression : Expression {
  VariableDeclaration *varDecl;
};

struct IntegerLiteral : Expression {
  int64_t value;
};

struct FloatLiteral : Expression {
  double value;
};

struct StringLiteral : Expression {
  StringReference value;
};

struct CastExpression : Expression {
  TypeDeclaration *typeDeclaration;
  Expression *expression;
};

//TODO Consider consolodating call statement and expression
//into a additional data structure called CallInfo and make
//it a member of both structs
struct CallExpression : Expression {
  ProcedureDeclaration *procDecl;
  Expression *firstArgument;
  int argumentCount;
};