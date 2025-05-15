%{
    #include <stdio.h>
    #include "nodes.hpp"
    #include "output.hpp"
    #include "visitor.hpp"

    extern int yylineno;
    extern int yylex();
    void yyerror(const char*);

    std::shared_ptr<ast::Node> program;

    using namespace std;
%}

%token VOID INT BYTE BOOL TRUE FALSE IF RETURN WHILE BREAK CONTINUE SC COMMA ID NUM NUM_B STRING

%right ASSIGN
%left OR
%left AND
%left RELOP
%left ADD_SUB
%left MULT_DIV
%right NOT
%left LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%right ELSE

%%

// While reducing the start variable, set the root of the AST
Program:  Funcs { program = $1; }
;

// TODO: Define grammar here

%%

// TODO: Place any additional code here