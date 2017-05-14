
struct Compiler;

enum StatementType {
  StatementType_Invalid,
  StatementType_Block,

  StatementType_VariableDeclaration,
  StatementType_TypeDeclaration,
  StatementType_ProcedureDeclaration,

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
  ExpressionType_CallExpression,
  ExpressionType_UnaryOperation,
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
  "ExpressionType_UnaryOperation",
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

struct Expression {
  ExpressionType expressionType;
  SourceLocation location;
  uint32_t expressionID;

  TypeInfo typeInfo;
  Expression *next;
};

//=====================================================

struct TypeMemberAccess {
  uint32_t indexCount;
  uint32_t *indices;
};

struct WhileStatement : Block {
  Expression *condition;
};

struct ElseStatement;

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
  VariableDeclaration *varDecl;
  Expression *expression;
  Expression *subscriptExpression;
  TypeMemberAccess memberAccess;
  TypeInfo typeInfo;
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

struct MemberAccessExpression : Expression {
  VariableDeclaration *varDecl;
  TypeMemberAccess memberAccess;
};

struct VariableExpression : Expression {
  VariableDeclaration *varDecl;
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
TypeInfo *GetSubTypeAtIndex(TypeInfo *type, size_t index) ;
bool IsBitwiseBinOp(TokenType type);

bool Equals(TypeInfo *a, TypeInfo *b);