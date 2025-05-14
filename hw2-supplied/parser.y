%{
#include "nodes.hpp"
#include "output.hpp"

extern int yylineno;
extern int yylex();
void yyerror(const char*);

// the AST root
std::shared_ptr<ast::Node> program;

using namespace ast;
%}

/* Tokens (lexical patterns defined in scanner.lex) */
%token VOID INT BYTE BOOL AND OR NOT TRUE FALSE RETURN
%token IF ELSE WHILE BREAK CONTINUE
%token SC       /* ; */ 
%token COMMA    /* , */
%token LPAREN   /* ( */
%token RPAREN   /* ) */
%token LBRACE   /* { */
%token RBRACE   /* } */
%token LBRACK   /* [ */
%token RBRACK   /* ] */
%token ASSIGN   /* = */
%token RELOP    /* == != < > <= >= */
%token BINOP    /* + - * / */
%token ID       /* [a-zA-Z][a-zA-Z0-9]* */
%token NUM      /* 0 | [1-9][0-9]* */
%token NUM_B    /* 0b | [1-9][0-9]*b */
%token STRING   /* "…"(no newlines or unescaped quotes) */

/* Precedence & associativity to disambiguate expressions
   and to solve dangling-else (lower-than-else trick). */
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE
%left OR
%left AND
%nonassoc RELOP
%left BINOP
%right NOT

%%

Program
  : Funcs              { program = $1; }
  ;

/*=== top-level function list ===*/
Funcs
  : /* empty */        { $$ = std::make_shared<ast::Funcs>(); }
  | FuncDecl Funcs     {
        auto fd = std::dynamic_pointer_cast<ast::FuncDecl>($1);
        auto fs = std::dynamic_pointer_cast<ast::Funcs>   ($2);
        fs->push_front(fd);
        $$ = fs;
    }
  ;

/*=== one function declaration ===*/
FuncDecl
  : RetType ID LPAREN Formals RPAREN LBRACE Statements RBRACE
    {
      auto rt   = std::dynamic_pointer_cast<Type>($1);
      auto id   = std::dynamic_pointer_cast<ID>  ($2);
      auto fmls = std::dynamic_pointer_cast<Formals>   ($4);
      auto stmts= std::dynamic_pointer_cast<Statements>($7);
      $$ = std::make_shared<FuncDecl>(id, rt, fmls, stmts);
    }
  ;

/*=== return type ===*/
RetType
  : Type              { $$ = $1; }
  | VOID              { $$ = std::make_shared<PrimitiveType>(BuiltInType::VOID); }
  ;

/*=== formal parameters ===*/
Formals
  : /* empty */       { $$ = std::make_shared<Formals>(); }
  | FormalsList       { $$ = $1; }
  ;

FormalsList
  : FormalDecl
    { $$ = std::make_shared<Formals>($1); }
  | FormalDecl COMMA FormalsList
    {
      auto fl = $3;
      fl->push_front($1);
      $$ = fl;
    }
  ;

FormalDecl
  : Type ID
    {
      auto t  = std::dynamic_pointer_cast<Type>($1);
      auto id = std::dynamic_pointer_cast<ID>  ($2);
      $$ = std::make_shared<Formal>(id, t);
    }
  ;

/*=== statement sequences ===*/
Statements
  : Statement
    { $$ = std::make_shared<Statements>($1); }
  | Statements Statement
    {
      auto ss = $1;
      ss->push_back($2);
      $$ = ss;
    }
  ;

/*=== individual statements ===*/
Statement
  : LBRACE Statements RBRACE
    { $$ = $2; }

  | Type ID SC
    {
      auto t  = std::dynamic_pointer_cast<Type>($1);
      auto id = std::dynamic_pointer_cast<ID>  ($2);
      $$ = std::make_shared<VarDecl>(id, t, nullptr);
    }
  | Type ID ASSIGN Exp SC
    {
      auto t  = std::dynamic_pointer_cast<Type>($1);
      auto id = std::dynamic_pointer_cast<ID>  ($2);
      auto e  = std::dynamic_pointer_cast<Exp> ($4);
      $$ = std::make_shared<VarDecl>(id, t, e);
    }
  | ID ASSIGN Exp SC
    {
      auto id = std::dynamic_pointer_cast<ID> ($1);
      auto e  = std::dynamic_pointer_cast<Exp>($3);
      $$ = std::make_shared<Assign>(id, e);
    }
  | ID LBRACK Exp RBRACK ASSIGN Exp SC
    {
      auto id  = std::dynamic_pointer_cast<ID> ($1);
      auto idx = std::dynamic_pointer_cast<Exp>($3);
      auto e   = std::dynamic_pointer_cast<Exp>($6);
      $$ = std::make_shared<ArrayAssign>(id, e, idx);
    }
  | Type ID LBRACK Exp RBRACK SC
    {
      // array declaration: Type id[Exp];
      auto base = std::dynamic_pointer_cast<PrimitiveType>($1);
      auto id   = std::dynamic_pointer_cast<ID>           ($2);
      auto len  = std::dynamic_pointer_cast<Exp>          ($4);
      auto arrT = std::make_shared<ArrayType>(base->type, len);
      $$ = std::make_shared<VarDecl>(id, arrT, nullptr);
    }
  | Call SC           { $$ = std::dynamic_pointer_cast<Statement>($1); }
  | RETURN SC         { $$ = std::make_shared<Return>(nullptr); }
  | RETURN Exp SC     {
      auto e = std::dynamic_pointer_cast<Exp>($2);
      $$ = std::make_shared<Return>(e);
    }

  /* dangling-else resolved by %prec LOWER_THAN_ELSE */
  | IF LPAREN Exp RPAREN Statement %prec LOWER_THAN_ELSE
    {
      auto c = std::dynamic_pointer_cast<Exp>      ($3);
      auto s = std::dynamic_pointer_cast<Statement>($5);
      $$ = std::make_shared<If>(c, s, nullptr);
    }
  | IF LPAREN Exp RPAREN Statement ELSE Statement
    {
      auto c = std::dynamic_pointer_cast<Exp>      ($3);
      auto t = std::dynamic_pointer_cast<Statement>($5);
      auto o = std::dynamic_pointer_cast<Statement>($7);
      $$ = std::make_shared<If>(c, t, o);
    }
  | WHILE LPAREN Exp RPAREN Statement
    {
      auto c = std::dynamic_pointer_cast<Exp>      ($3);
      auto b = std::dynamic_pointer_cast<Statement>($6);
      $$ = std::make_shared<While>(c, b);
    }
  | BREAK SC        { $$ = std::make_shared<Break>(); }
  | CONTINUE SC     { $$ = std::make_shared<Continue>(); }
  ;

/*=== function calls as expressions/statements ===*/
Call
  : ID LPAREN ExpList RPAREN
    {
      auto id  = std::dynamic_pointer_cast<ID>($1);
      auto el  = std::dynamic_pointer_cast<ExpList>($3);
      $$ = std::make_shared<Call>(id, el);
    }
  | ID LPAREN RPAREN
    {
      auto id = std::dynamic_pointer_cast<ID>($1);
      $$ = std::make_shared<Call>(id);
    }
  ;

/*=== comma-separated argument lists ===*/
ExpList
  : Exp
    { $$ = std::make_shared<ExpList>(std::dynamic_pointer_cast<Exp>($1)); }
  | Exp COMMA ExpList
    {
      auto e  = std::dynamic_pointer_cast<Exp>($1);
      auto el = $3;
      el->push_front(e);
      $$ = el;
    }
  ;

/*=== built-in types ===*/
Type
  : INT  { $$ = std::make_shared<PrimitiveType>(BuiltInType::INT);  }
  | BYTE { $$ = std::make_shared<PrimitiveType>(BuiltInType::BYTE); }
  | BOOL { $$ = std::make_shared<PrimitiveType>(BuiltInType::BOOL); }
  ;

/*=== expressions ===*/
Exp
  : LPAREN Exp RPAREN
    { $$ = $2; }

  | LPAREN Type RPAREN Exp
    {
      // cast:  (Type) Exp
      // we know $2 is PrimitiveType
      auto pt = std::dynamic_pointer_cast<PrimitiveType>($2);
      auto ex= std::dynamic_pointer_cast<Exp>($4);
      $$ = std::make_shared<Cast>(ex, pt);
    }

  | NOT Exp
    {
      auto e = std::dynamic_pointer_cast<Exp>($2);
      $$ = std::make_shared<Not>(e);
    }

  | Exp AND Exp
    {
      auto l = std::dynamic_pointer_cast<Exp>($1);
      auto r = std::dynamic_pointer_cast<Exp>($3);
      $$ = std::make_shared<And>(l, r);
    }

  | Exp OR Exp
    {
      auto l = std::dynamic_pointer_cast<Exp>($1);
      auto r = std::dynamic_pointer_cast<Exp>($3);
      $$ = std::make_shared<Or>(l, r);
    }

  | Exp RELOP Exp
    {
      // relational operator
      char const* op = yytext;
      RelOpType ro;
      if      (!strcmp(op, "==" )) ro = RelOpType::EQ;
      else if (!strcmp(op, "!=" )) ro = RelOpType::NE;
      else if (!strcmp(op, "<"  )) ro = RelOpType::LT;
      else if (!strcmp(op, "<=" )) ro = RelOpType::LE;
      else if (!strcmp(op, ">"  )) ro = RelOpType::GT;
      else /*>=*/                  ro = RelOpType::GE;
      auto l = std::dynamic_pointer_cast<Exp>($1);
      auto r = std::dynamic_pointer_cast<Exp>($3);
      $$ = std::make_shared<RelOp>(l, r, ro);
    }

  | Exp BINOP Exp
    {
      // binary arithmetic
      char c = yytext[0];
      BinOpType bo = (c=='+'?BinOpType::ADD:
                      c=='-'?BinOpType::SUB:
                      c=='*'?BinOpType::MUL:
                             BinOpType::DIV);
      auto l = std::dynamic_pointer_cast<Exp>($1);
      auto r = std::dynamic_pointer_cast<Exp>($3);
      $$ = std::make_shared<BinOp>(l, r, bo);
    }

  | ID         { $$ = $1; }
  | NUM        { $$ = $1; }
  | NUM_B      { $$ = $1; }
  | STRING     { $$ = $1; }
  | TRUE       { $$ = std::make_shared<Bool>(true); }
  | FALSE      { $$ = std::make_shared<Bool>(false); }
  | Call       { $$ = std::dynamic_pointer_cast<Exp>($1); }
  ;

%%

/* on any syntax error, report and exit immediately */
void yyerror(const char* s) {
    errorSyn(yylineno);
}
