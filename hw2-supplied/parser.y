%{
    #include "nodes.hpp"
    #include "output.hpp"

    extern int yylineno;
    extern int yylex();
    void yyerror(const char*);

    std::shared_ptr<ast::Node> program;

    using namespace std;
    using namespace ast;
%}

%token VOID INT BYTE BOOL CONST TRUE FALSE IF RETURN WHILE BREAK CONTINUE SC COMMA ID NUM NUM_B STRING


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

Program: Funcs                                                      {}

Funcs: /*epsilon*/                                                  {}
       | FuncDecl Funcs                                             {}

FuncDecl: RetType ID LPAREN Formals RPAREN LBRACE Statements RBRACE {}

RetType: Type                                                       {}
         | VOID                                                     {}

Formals: /*epsilon*/                                                {}
         | FormalsList                                              {}

FormalsList: FormalDecl                                             {}
             | FormalDecl COMMA FormalsList                         {}

FormalDecl:  Type ID                                                {}

Statements: Statement                                               {}
            | Statements Statement                                  {}

Statement: LBRACE Statements RBRACE                                 {}
           | Type ID SC                                             {}
           | Type ID ASSIGN Exp SC                                  {}
           | ID ASSIGN Exp SC                                       {}
           | ID LBRACK Exp RBRACK ASSIGN Exp SC                     {}
           | Type ID LBRACK Exp RBRACK SC                           {}
           | Call SC                                                {}
           | RETURN SC                                              {}
           | RETURN Exp SC                                          {}
           | IF LPAREN Exp RPAREN Statement                         {}
           | IF LPAREN Exp RPAREN Statement ELSE Statement          {}
           | WHILE LPAREN Exp RPAREN Statement                      {}
           | BREAK SC                                               {}
           | CONTINUE SC                                            {}

Call: ID LPAREN ExpList RPAREN                                      {}
      | ID LPAREN RPAREN                                            {}

ExpList: Exp                                                        {}
         | Exp COMMA ExpList                                        {}

Type: INT                                                           {}
      | BYTE                                                        {}
      | BOOL                                                        {}

Exp: LPAREN Exp RPAREN                                              {}
     | ID LBRACK Exp RBRACK                                         {}
     | Exp ADD_SUB Exp                                              {}
     | Exp MULT_DIV Exp                                             {}
     | ID                                                           {}
     | Call                                                         {}
     | NUM                                                          {}
     | NUM_B                                                        {}
     | STRING                                                       {}
     | TRUE                                                         {}
     | FALSE                                                        {}
     | NOT Exp                                                      {}
     | Exp AND Exp                                                  {}
     | Exp OR Exp                                                   {}
     | Exp RELOP Exp                                                {}
     | LPAREN Type RPAREN Exp                                       {}

%%

void yyerror(const char*) {
    output::errorSyn(yylineno);
    exit(0);
}
