%{
\#include "parser.tab.hpp"
\#include "nodes.hpp"
\#include <memory>
extern int yylineno;
// On fatal scanner error, report with the prescribed function
\#define YY\_FATAL\_ERROR(msg) errorLex(yylineno)
%}

%option noyywrap

/\* Definitions for reusable patterns */
DIGIT           \[0-9]
ID              \[a-zA-Z]\[a-zA-Z0-9]*
NUM             0|\[1-9]\[0-9]\*
NUM\_B           0b\[01]+|\[1-9]\[0-9]*b
STRING          "(\[^"\n\\]|\\\[rnt"\\])*"  /\* C-style escaped string \*/

%%

/\* Keywords \*/
"void"          { return VOID; }
"int"           { return INT; }
"byte"          { return BYTE; }
"bool"          { return BOOL; }
"string"        { return STRING; }
"and"           { return AND; }
"or"            { return OR; }
"not"           { return NOT; }
"true"          { yylval = std::make\_shared[ast::Bool](ast::Bool)(true); return TRUE; }
"false"         { yylval = std::make\_shared[ast::Bool](ast::Bool)(false); return FALSE; }
"return"        { return RETURN; }
"if"            { return IF; }
"else"          { return ELSE; }
"while"         { return WHILE; }
"break"         { return BREAK; }
"continue"      { return CONTINUE; }

/\* Identifiers \*/
{ID}             { yylval = std::make\_shared[ast::Ident](ast::Ident)(yytext); return ID; }

/\* Numeric literals \*/
{NUM\_B}          { yylval = std::make\_shared[ast::Num](ast::Num)(yytext); return NUM\_B; }
{NUM}            { yylval = std::make\_shared[ast::Num](ast::Num)(yytext); return NUM; }

/\* String literals \*/
{STRING}         { yylval = std::make\_shared[ast::String](ast::String)(yytext); return STRING; }

/\* Punctuation and operators */
";"             { return SC; }
","             { return COMMA; }
"("             { return LPAREN; }
")"             { return RPAREN; }
"{"             { return LBRACE; }
"}"             { return RBRACE; }
"\["            { return LBRACK; }
"]"             { return RBRACK; }
"="             { return ASSIGN; }
"=="|"!="|"<="|">="|"<"|">"  { return RELOP; }
"+"|"-"|"*"|"/"               { return BINOP; }

/\* Skip whitespace and comments */
\[ \t\r]+       { /* ignore */ }
"\n"           { yylineno++; }
"//".*         { /\* ignore line comment \*/ }

/\* Any other character is illegal \*/
.                { errorLex(yylineno); }

%%
