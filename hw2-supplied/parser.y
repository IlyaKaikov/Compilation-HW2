%define api.value.type {std::shared_ptr<ast::Node>}
%define api.location.type {YYLTYPE}
%define parse.error custom
%locations
%debug

%{
#include <memory>
#include <utility>
#include "nodes.hpp"
#include "output.hpp"

using namespace std;
using namespace ast;

extern int yylineno;
extern int yylex();

std::shared_ptr<ast::Node> program;
%}

%token VOID INT BYTE BOOL IF ELSE WHILE RETURN BREAK CONTINUE
%token TRUE FALSE NOT AND OR
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token SC COMMA ASSIGN
%token <std::shared_ptr<ast::Node>> ID NUM NUM_B STRING BINOP RELOP

%type <std::shared_ptr<ast::Node>> Program Funcs FuncDecl RetType Type Formals FormalsList FormalDecl Statements Statement Exp ExpList Call

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE
%left OR
%left AND
%left RELOP
%left BINOP
%right NOT

%%
Program : Funcs               { program = $1; $$ = $1; }
        ;

Funcs   :                     { $$ = make_shared<Funcs>(); }
        | FuncDecl Funcs      { auto f = dynamic_pointer_cast<FuncDecl>($1); auto fs = dynamic_pointer_cast<Funcs>($2); fs->push_front(f); $$ = fs; }
        ;

FuncDecl: RetType ID LPAREN Formals RPAREN LBRACE Statements RBRACE
          { $$ = make_shared<FuncDecl>(dynamic_pointer_cast<ID>($2), dynamic_pointer_cast<Type>($1), dynamic_pointer_cast<Formals>($4), dynamic_pointer_cast<Statements>($7)); }
        ;

RetType : Type                { $$ = $1; }
        | VOID                { $$ = make_shared<PrimitiveType>(BuiltInType::VOID); }
        ;

Formals :                     { $$ = make_shared<Formals>(); }
        | FormalsList         { $$ = $1; }
        ;

FormalsList : FormalDecl                          { $$ = make_shared<Formals>(dynamic_pointer_cast<FormalDecl>($1)); }
            | FormalDecl COMMA FormalsList        { auto fs = dynamic_pointer_cast<Formals>($3); fs->push_front(dynamic_pointer_cast<FormalDecl>($1)); $$ = fs; }
            ;

FormalDecl : Type ID   { $$ = make_shared<Formal>(dynamic_pointer_cast<ID>($2), dynamic_pointer_cast<Type>($1)); }
           ;

Statements : Statement                     { $$ = make_shared<Statements>(dynamic_pointer_cast<Statement>($1)); }
           | Statements Statement          { auto st = dynamic_pointer_cast<Statements>($1); st->push_back(dynamic_pointer_cast<Statement>($2)); $$ = st; }
           ;

Statement : LBRACE Statements RBRACE       { $$ = $2; }
          | Type ID SC                     { $$ = make_shared<VarDecl>(dynamic_pointer_cast<ID>($2), dynamic_pointer_cast<Type>($1), nullptr); }
          | Type ID ASSIGN Exp SC          { $$ = make_shared<VarDecl>(dynamic_pointer_cast<ID>($2), dynamic_pointer_cast<Type>($1), dynamic_pointer_cast<Exp>($4)); }
          | ID ASSIGN Exp SC               { $$ = make_shared<Assign>(dynamic_pointer_cast<ID>($1), dynamic_pointer_cast<Exp>($3)); }
          | ID LBRACK Exp RBRACK ASSIGN Exp SC { $$ = make_shared<ArrayAssign>(dynamic_pointer_cast<ID>($1), dynamic_pointer_cast<Exp>($6), dynamic_pointer_cast<Exp>($3)); }
          | Type ID LBRACK Exp RBRACK SC   { $$ = make_shared<VarDecl>(dynamic_pointer_cast<ID>($2), make_shared<ArrayType>(dynamic_pointer_cast<PrimitiveType>($1)->type, dynamic_pointer_cast<Exp>($4)), nullptr); }
          | Call SC                        { $$ = $1; }
          | RETURN SC                      { $$ = make_shared<Return>(nullptr); }
          | RETURN Exp SC                  { $$ = make_shared<Return>(dynamic_pointer_cast<Exp>($2)); }
          | IF LPAREN Exp RPAREN Statement %prec LOWER_THAN_ELSE { $$ = make_shared<If>(dynamic_pointer_cast<Exp>($3), dynamic_pointer_cast<Statement>($5), nullptr); }
          | IF LPAREN Exp RPAREN Statement ELSE Statement        { $$ = make_shared<If>(dynamic_pointer_cast<Exp>($3), dynamic_pointer_cast<Statement>($5), dynamic_pointer_cast<Statement>($7)); }
          | WHILE LPAREN Exp RPAREN Statement   { $$ = make_shared<While>(dynamic_pointer_cast<Exp>($3), dynamic_pointer_cast<Statement>($5)); }
          | BREAK SC                     { $$ = make_shared<Break>(); }
          | CONTINUE SC                  { $$ = make_shared<Continue>(); }
          ;

Call      : ID LPAREN RPAREN                 { $$ = make_shared<Call>(dynamic_pointer_cast<ID>($1)); }
          | ID LPAREN ExpList RPAREN         { $$ = make_shared<Call>(dynamic_pointer_cast<ID>($1), dynamic_pointer_cast<ExpList>($3)); }
          ;

ExpList   : Exp                              { $$ = make_shared<ExpList>(dynamic_pointer_cast<Exp>($1)); }
          | Exp COMMA ExpList                { auto lst = dynamic_pointer_cast<ExpList>($3); lst->push_front(dynamic_pointer_cast<Exp>($1)); $$ = lst; }
          ;

Type      : INT   { $$ = make_shared<PrimitiveType>(BuiltInType::INT); }
          | BYTE  { $$ = make_shared<PrimitiveType>(BuiltInType::BYTE); }
          | BOOL  { $$ = make_shared<PrimitiveType>(BuiltInType::BOOL); }
          ;

Exp       : LPAREN Exp RPAREN                { $$ = $2; }
          | ID LBRACK Exp RBRACK             { $$ = make_shared<ArrayDereference>(dynamic_pointer_cast<ID>($1), dynamic_pointer_cast<Exp>($3)); }
          | Exp BINOP Exp                    { auto opStr = dynamic_pointer_cast<ID>($2)->value; BinOpType op = (opStr == "+") ? BinOpType::ADD : (opStr == "-") ? BinOpType::SUB : (opStr == "*") ? BinOpType::MUL : BinOpType::DIV; $$ = make_shared<BinOp>(dynamic_pointer_cast<Exp>($1), dynamic_pointer_cast<Exp>($3), op); }
          | Exp AND Exp                      { $$ = make_shared<And>(dynamic_pointer_cast<Exp>($1), dynamic_pointer_cast<Exp>($3)); }
          | Exp OR Exp                       { $$ = make_shared<Or>(dynamic_pointer_cast<Exp>($1), dynamic_pointer_cast<Exp>($3)); }
          | Exp RELOP Exp                    { auto opStr = dynamic_pointer_cast<ID>($2)->value; RelOpType op = (opStr == "==") ? RelOpType::EQ : (opStr == "!=") ? RelOpType::NE : (opStr == "<") ? RelOpType::LT : (opStr == "<=") ? RelOpType::LE : (opStr == ">") ? RelOpType::GT : RelOpType::GE; $$ = make_shared<RelOp>(dynamic_pointer_cast<Exp>($1), dynamic_pointer_cast<Exp>($3), op); }
          | ID                                { $$ = $1; }
          | Call                              { $$ = $1; }
          | NUM                               { $$ = $1; }
          | NUM_B                             { $$ = $1; }
          | STRING                            { $$ = $1; }
          | TRUE                              { $$ = $1; }
          | FALSE                             { $$ = $1; }
          | NOT Exp                           { $$ = make_shared<Not>(dynamic_pointer_cast<Exp>($2)); }
          | LPAREN Type RPAREN Exp            { $$ = make_shared<Cast>(dynamic_pointer_cast<Exp>($4), dynamic_pointer_cast<PrimitiveType>($2)); }
          ;
%%

int yyerror(const char *) { output::errorSyn(yylineno); return 0; }
