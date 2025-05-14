%{
#include "parser.tab.h"
#include "nodes.hpp"
#include "output.hpp"
extern int yylineno;
%}

%option noyywrap
%option yylineno

whitespace      ([\r\n\t ])
digit           ([0-9])
letter          ([a-zA-Z])
digitletter     ([0-9a-zA-Z])
string          ([ !#-\[	\]-~])

%%
void                                                                            return VOID;
int                                                                             return INT;
byte                                                                            return BYTE;
bool                                                                            return BOOL;
and                                                                             return AND;
or                                                                              return OR;
not                                                                             return NOT;
true                                                                            return TRUE;
false                                                                           return FALSE;
return                                                                          return RETURN;
if                                                                              return IF;
else                                                                            return ELSE;
while                                                                           return WHILE;
break                                                                           return BREAK;
continue                                                                        return CONTINUE;
;                                                                               return SC;
,                                                                               return COMMA;
\(                                                                              return LPAREN;
\)                                                                              return RPAREN;
\{                                                                              return LBRACE;
\}                                                                              return RBRACE;
\[                                                                              return LBRACK;
\]                                                                              return RBRACK;
[=!<>]=                                                                         return RELOP;
<|>                                                                             return RELOP;
=                                                                               return ASSIGN;
[-+*/]                                                                          return BINOP;

{letter}{digitletter}*                                                          {
                                                                                yylval = std::make_shared<ast::ID>(yytext);
                                                                                return ID;
                                                                                }
0|([1-9]+{digit}*)                                                              {
                                                                                yylval = std::make_shared<ast::Num>(yytext);
                                                                                return NUM;
                                                                                }
(0|([1-9]+{digit}*))b                                                           {
                                                                                yylval = std::make_shared<ast::NumB>(yytext);
                                                                                return NUM_B;
                                                                                }
\"{string}*\"                                                                   {
                                                                                yylval = std::make_shared<ast::String>(yytext);
                                                                                return STRING;
                                                                                }
{whitespace}                                                                    ;
\/\/[^\r\n]*                                                                    ;
.                                                                               { errorLex(yylineno); }
%%
