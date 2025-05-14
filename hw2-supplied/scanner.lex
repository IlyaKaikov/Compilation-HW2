%{
#include "parser.tab.h"   /* Bison’s token definitions */
#include "nodes.hpp"       /* ast::ID, ast::Num, etc. */
#include "output.hpp"      /* errorLex() */
extern int yylineno;
%}

%option noyywrap
%option yylineno

%%
[ \t\r\n]+                    /* skip whitespace */
"//".*                        /* skip single-line comments */

void                          return VOID;
int                           return INT;
byte                          return BYTE;
bool                          return BOOL;
and                           return AND;
or                            return OR;
not                           return NOT;
true                          return TRUE;
false                         return FALSE;
return                        return RETURN;
if                            return IF;
else                          return ELSE;
while                         return WHILE;
break                         return BREAK;
continue                      return CONTINUE;

;                             return SC;
,                             return COMMA;
\(                            return LPAREN;
\)                            return RPAREN;
\{                            return LBRACE;
\}                            return RBRACE;
\[                            return LBRACK;
\]                            return RBRACK;

==|!=|<=|>=                   return RELOP;
<|>                           return RELOP;
=                             return ASSIGN;

+|-|*|/                       return BINOP;

[a-zA-Z][a-zA-Z0-9]*          {
                                  yylval = std::make_shared<ast::ID>(yytext);
                                  return ID;
                              }
0|[1-9][0-9]*                 {
                                  yylval = std::make_shared<ast::Num>(yytext);
                                  return NUM;
                              }
/(0b|[1-9][0-9]*b)/           {
                                  yylval = std::make_shared<ast::NumB>(yytext);
                                  return NUM_B;
                              }
\"([^\\\n\r\"]|\\[rnt\"\\])*\"  {
                                  yylval = std::make_shared<ast::String>(yytext);
                                  return STRING;
                              }

.                             { errorLex(yylineno); }
%%
