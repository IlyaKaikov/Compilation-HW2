%option c++
%option noyywrap
%option yylineno

%{
#include <memory>
#include "nodes.hpp"
#include "output.hpp"
using namespace std;
using namespace ast;
%}

WHITES      [ \t\r]+
DIGIT       [0-9]
BIN_DIGIT   [01]
ID_START    [a-zA-Z]
ID_CHAR     [a-zA-Z0-9]
ESC         \\[rnt\"\\]

%%
{WHITES}                     ;
"//"[^\n\r]*                 ;
\n                           ;

"void"                       { return VOID; }
"int"                        { return INT; }
"byte"                       { return BYTE; }
"bool"                       { return BOOL; }

"and"                        { return AND; }
"or"                         { return OR; }
"not"                        { return NOT; }

"true"                       { yylval = make_shared<ast::Bool>(true);  return TRUE; }
"false"                      { yylval = make_shared<ast::Bool>(false); return FALSE; }

"return"                     { return RETURN; }
"if"                         { return IF; }
"else"                       { return ELSE; }
"while"                      { return WHILE; }
"break"                      { return BREAK; }
"continue"                   { return CONTINUE; }

";"                          { return SC; }
","                          { return COMMA; }
"("                          { return LPAREN; }
")"                          { return RPAREN; }
"{"                          { return LBRACE; }
"}"                          { return RBRACE; }
"["                          { return LBRACK; }
"]"                          { return RBRACK; }
"="                          { return ASSIGN; }

"=="|"!="|"<="|">="|"<"|">"  { yylval = make_shared<ast::ID>(yytext); return RELOP; }
"+"|"-"|"*"|"/"              { yylval = make_shared<ast::ID>(yytext); return BINOP; }

\"([^\\\n\r\"]|{ESC})+\"     { yylval = make_shared<ast::String>(yytext); return STRING; }

0b{BIN_DIGIT}+               { yylval = make_shared<ast::NumB>(yytext); return NUM_B; }
{DIGIT}+b                    { yylval = make_shared<ast::NumB>(yytext); return NUM_B; }

0|([1-9]{DIGIT}*)            { yylval = make_shared<ast::Num>(yytext);  return NUM; }

{ID_START}{ID_CHAR}*         { yylval = make_shared<ast::ID>(yytext);  return ID; }

.                            { output::errorLex(yylineno); }

%%
int yywrap()                 { return 1; }
