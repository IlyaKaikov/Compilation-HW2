%{

#include "nodes.hpp"
#include "output.hpp"

// bison declarations
extern int yylineno;
extern int yylex();

void yyerror(const char*);

// root of the AST, set by the parser and used by other parts of the compiler
std::shared_ptr<ast::Node> program;

using namespace std;
%}

%define api.value.type {std::shared_ptr<ast::Node>}

%union {
  char*    sval;
  bool     bval;
  std::shared_ptr<ast::Node> node;
}

%token <sval> ID STRING
%token <sval> BINOP
%token <sval> RELOP
%token <node> NUM
%token <node> NUM_B

%token VOID INT BYTE BOOL
%token AND OR NOT TRUE FALSE RETURN
%token IF ELSE WHILE BREAK CONTINUE

%token SC COMMA LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK ASSIGN

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE
%right    NOT
%left     OR
%left     AND
%nonassoc RELOP
%left     BINOP

%%

/* 1. Program → Funcs */
Program:
    Funcs            { program = $1; }
  ;

/* 2–3. Funcs → ε | FuncDecl Funcs */
Funcs:
    /* empty */      { $$ = std::make_shared<ast::Funcs>(); }
  | FuncDecl Funcs  {
        auto tail = std::dynamic_pointer_cast<ast::Funcs>($2);
        tail->push_front(std::dynamic_pointer_cast<ast::FuncDecl>($1));
        $$ = tail;
    }
  ;

/* 4. FuncDecl → RetType ID LPAREN Formals RPAREN LBRACE Statements RBRACE */
FuncDecl:
    RetType ID LPAREN Formals RPAREN LBRACE Statements RBRACE {
        $$ = std::make_shared<ast::FuncDecl>(
            std::dynamic_pointer_cast<ast::ID>($2),
            std::dynamic_pointer_cast<ast::Type>($1),
            std::dynamic_pointer_cast<ast::Formals>($4),
            std::dynamic_pointer_cast<ast::Statements>($7)
        );
    }
  ;

/* 5–6. RetType → Type | VOID */
RetType:
    Type             { $$ = $1; }
  | VOID             { $$ = std::make_shared<ast::PrimitiveType>(ast::BuiltInType::VOID); }
  ;

/* 32–34. Type → INT | BYTE | BOOL */
Type:
    INT              { $$ = std::make_shared<ast::PrimitiveType>(ast::BuiltInType::INT); }
  | BYTE             { $$ = std::make_shared<ast::PrimitiveType>(ast::BuiltInType::BYTE); }
  | BOOL             { $$ = std::make_shared<ast::PrimitiveType>(ast::BuiltInType::BOOL); }
  ;

/* 7–11. Formals → ε | FormalsList */
Formals:
    /* empty */      { $$ = std::make_shared<ast::Formals>(); }
  | FormalsList     { $$ = $1; }
  ;


FormalsList:
    /* 9–10 */ FormalDecl                         {
        $$ = std::make_shared<ast::Formals>(
            std::dynamic_pointer_cast<ast::Formal>($1)
        );
    }
  | FormalDecl COMMA FormalsList                {
        auto tail = $3;
        tail->push_front(std::dynamic_pointer_cast<ast::Formal>($1));
        $$ = tail;
    }
  ;

/* 11. FormalDecl → Type ID */
FormalDecl:
    Type ID          {
        $$ = std::make_shared<ast::Formal>(
            std::dynamic_pointer_cast<ast::ID>($2),
            std::dynamic_pointer_cast<ast::Type>($1)
        );
    }
  ;

/* 12–13. Statements → Statement | Statements Statement */
Statements:
    Statement                              {
        $$ = std::make_shared<ast::Statements>(
            std::dynamic_pointer_cast<ast::Statement>($1)
        );
    }
  | Statements Statement                   {
        auto seq = $1;
        seq->push_back(std::dynamic_pointer_cast<ast::Statement>($2));
        $$ = seq;
    }
  ;

/* 14–27. Statement → … */
Statement:
    LBRACE Statements RBRACE               {
        $$ = std::make_shared<ast::Statements>(
            std::dynamic_pointer_cast<ast::Statement>($2)
        );
    }
  | Type ID SC                              {
        $$ = std::make_shared<ast::VarDecl>(
            std::dynamic_pointer_cast<ast::ID>($2),
            std::dynamic_pointer_cast<ast::Type>($1),
            nullptr
        );
    }
  | Type ID ASSIGN Exp SC                   {
        $$ = std::make_shared<ast::VarDecl>(
            std::dynamic_pointer_cast<ast::ID>($2),
            std::dynamic_pointer_cast<ast::Type>($1),
            std::dynamic_pointer_cast<ast::Exp>($4)
        );
    }
  | ID ASSIGN Exp SC                        {
        $$ = std::make_shared<ast::Assign>(
            std::dynamic_pointer_cast<ast::ID>($1),
            std::dynamic_pointer_cast<ast::Exp>($3)
        );
    }
  | ID LBRACK Exp RBRACK ASSIGN Exp SC      {
        $$ = std::make_shared<ast::ArrayAssign>(
            std::dynamic_pointer_cast<ast::ID>($1),
            std::dynamic_pointer_cast<ast::Exp>($3),
            std::dynamic_pointer_cast<ast::Exp>($6)
        );
    }
  | Type ID LBRACK Exp RBRACK SC            {
        /* array declaration */
        auto prim = std::dynamic_pointer_cast<ast::PrimitiveType>($1);
        $$ = std::make_shared<ast::VarDecl>(
            std::dynamic_pointer_cast<ast::ID>($2),
            std::make_shared<ast::ArrayType>(prim->type, std::dynamic_pointer_cast<ast::Exp>($4)),
            nullptr
        );
    }
  | Call SC                                 { $$ = std::dynamic_pointer_cast<ast::Statement>($1); }
  | RETURN SC                               { $$ = std::make_shared<ast::Return>(nullptr); }
  | RETURN Exp SC                           { $$ = std::make_shared<ast::Return>(std::dynamic_pointer_cast<ast::Exp>($2)); }
  | IF LPAREN Exp RPAREN Statement %prec LOWER_THAN_ELSE {
        $$ = std::make_shared<ast::If>(
            std::dynamic_pointer_cast<ast::Exp>($3),
            std::dynamic_pointer_cast<ast::Statement>($5),
            nullptr
        );
    }
  | IF LPAREN Exp RPAREN Statement ELSE Statement {
        $$ = std::make_shared<ast::If>(
            std::dynamic_pointer_cast<ast::Exp>($3),
            std::dynamic_pointer_cast<ast::Statement>($5),
            std::dynamic_pointer_cast<ast::Statement>($7)
        );
    }
  | WHILE LPAREN Exp RPAREN Statement       {
        $$ = std::make_shared<ast::While>(
            std::dynamic_pointer_cast<ast::Exp>($3),
            std::dynamic_pointer_cast<ast::Statement>($5)
        );
    }
  | BREAK SC                                { $$ = std::make_shared<ast::Break>(); }
  | CONTINUE SC                             { $$ = std::make_shared<ast::Continue>(); }
  ;

/* 28–29. Call → ID LPAREN (ExpList | ε) RPAREN */
Call:
    ID LPAREN ExpList RPAREN                {
        $$ = std::make_shared<ast::Call>(
            std::dynamic_pointer_cast<ast::ID>($1),
            std::dynamic_pointer_cast<ast::ExpList>($3)
        );
    }
  | ID LPAREN RPAREN                        {
        $$ = std::make_shared<ast::Call>(
            std::dynamic_pointer_cast<ast::ID>($1)
        );
    }
  ;

/* 30–31. ExpList → Exp | Exp COMMA ExpList */
ExpList:
    Exp                                     {
        $$ = std::make_shared<ast::ExpList>(std::dynamic_pointer_cast<ast::Exp>($1));
    }
  | Exp COMMA ExpList                       {
        auto tail = $3;
        tail->push_front(std::dynamic_pointer_cast<ast::Exp>($1));
        $$ = tail;
    }
  ;

/* 35–49. Exp → … */
Exp:
    LPAREN Exp RPAREN                       { $$ = $2; }
  | ID LBRACK Exp RBRACK                    {
        $$ = std::make_shared<ast::ArrayDereference>(
            std::dynamic_pointer_cast<ast::ID>($1),
            std::dynamic_pointer_cast<ast::Exp>($3)
        );
    }
  | Exp BINOP Exp                           {
        ast::BinOpType op;
        if      (strcmp($2, "+")==0) op = ast::BinOpType::ADD;
        else if (strcmp($2, "-")==0) op = ast::BinOpType::SUB;
        else if (strcmp($2, "*")==0) op = ast::BinOpType::MUL;
        else                         op = ast::BinOpType::DIV;
        $$ = std::make_shared<ast::BinOp>($1, $3, op);
    }
  | Exp RELOP Exp                           {
        ast::RelOpType ro;
        if      (strcmp($2, "==")==0) ro = ast::RelOpType::EQ;
        else if (strcmp($2, "!=")==0) ro = ast::RelOpType::NE;
        else if (strcmp($2, "<=")==0) ro = ast::RelOpType::LE;
        else if (strcmp($2, ">=")==0) ro = ast::RelOpType::GE;
        else if (strcmp($2, "<")==0)  ro = ast::RelOpType::LT;
        else                          ro = ast::RelOpType::GT;
        $$ = std::make_shared<ast::RelOp>($1, $3, ro);
    }
  | Exp AND Exp                            { $$ = std::make_shared<ast::And>($1,$3); }
  | Exp OR Exp                             { $$ = std::make_shared<ast::Or>($1,$3); }
  | NOT Exp                                { $$ = std::make_shared<ast::Not>($2); }
  | LPAREN Type RPAREN Exp                 {
        $$ = std::make_shared<ast::Cast>(
            std::dynamic_pointer_cast<ast::Exp>($4),
            std::dynamic_pointer_cast<ast::PrimitiveType>($2)
        );
    }
  | ID                                     { $$ = $1; }
  | Call                                   { $$ = $1; }
  | NUM                                    { $$ = $1; }
  | NUM_B                                  { $$ = $1; }
  | STRING                                 { $$ = $1; }
  | TRUE                                   { $$ = std::make_shared<ast::Bool>(true); }
  | FALSE                                  { $$ = std::make_shared<ast::Bool>(false); }
  ;

%%

void yyerror(const char *s) {
    errorSyn(yylineno);
}
