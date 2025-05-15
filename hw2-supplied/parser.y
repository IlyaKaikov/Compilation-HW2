%{
#include <memory>
#include <cstring>
#include "nodes.hpp"
#include "output.hpp"

extern int yylineno;
extern int yylex();
extern char *yytext;
void yyerror(const char*);

std::shared_ptr<ast::Node> program;

using namespace ast;

extern char last_op[];

static BinOpType binop_from_lexeme(const char* l){
    switch(l[0]){
        case '+': return BinOpType::ADD;
        case '-': return BinOpType::SUB;
        case '*': return BinOpType::MUL;
        default: return BinOpType::DIV;
    }
}
static RelOpType relop_from_lexeme(const char* l){
    if(l[0]=='='&&l[1]=='=') return RelOpType::EQ;
    if(l[0]=='!'&&l[1]=='=') return RelOpType::NE;
    if(l[0]=='<'&&l[1]=='=') return RelOpType::LE;
    if(l[0]=='>'&&l[1]=='=') return RelOpType::GE;
    if(l[0]=='<') return RelOpType::LT;
    return RelOpType::GT;
}
%}

%token T_VOID T_INT T_BYTE T_BOOL AND OR NOT TRUE FALSE RETURN IF ELSE WHILE BREAK CONTINUE
%token SC COMMA LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK ASSIGN
%token RELOP BINOP
%token T_ID NUM NUM_B T_STRING

%type Program Funcs FuncDecl RetType Formals FormalsList FormalDecl Statements Statement Call Exp ExpList Type

%start Program

%left OR
%left AND
%nonassoc RELOP
%left BINOP
%right NOT
%right CAST
%left LBRACK
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%%
Program
    : Funcs { program = $1; }
    ;

Funcs
    : /* empty */ { $$ = std::make_shared<Funcs>(); }
    | FuncDecl Funcs { auto list = std::dynamic_pointer_cast<Funcs>($2); list->push_front(std::dynamic_pointer_cast<FuncDecl>($1)); $$ = list; }
    ;

FuncDecl
    : RetType T_ID LPAREN Formals RPAREN LBRACE Statements RBRACE
        {
            $$ = std::make_shared<FuncDecl>(
                std::dynamic_pointer_cast<ID>($2),
                std::dynamic_pointer_cast<Type>($1),
                std::dynamic_pointer_cast<Formals>($4),
                std::dynamic_pointer_cast<Statements>($7)
            );
        }
    ;

RetType
    : Type { $$ = $1; }
    | T_VOID { $$ = std::make_shared<PrimitiveType>(BuiltInType::VOID); }
    ;

Formals
    : /* empty */ { $$ = std::make_shared<Formals>(); }
    | FormalsList { $$ = $1; }
    ;

FormalsList
    : FormalDecl { $$ = std::make_shared<Formals>(std::dynamic_pointer_cast<Formal>($1)); }
    | FormalDecl COMMA FormalsList
        {
            auto list = std::dynamic_pointer_cast<Formals>($3);
            list->push_front(std::dynamic_pointer_cast<Formal>($1));
            $$ = list;
        }
    ;

FormalDecl
    : Type T_ID
        {
            $$ = std::make_shared<Formal>(
                std::dynamic_pointer_cast<ID>($2),
                std::dynamic_pointer_cast<Type>($1)
            );
        }
    ;

Statements
    : Statement { $$ = std::make_shared<Statements>(std::dynamic_pointer_cast<Statement>($1)); }
    | Statements Statement
        {
            auto list = std::dynamic_pointer_cast<Statements>($1);
            list->push_back(std::dynamic_pointer_cast<Statement>($2));
            $$ = list;
        }
    ;

Statement
    : LBRACE Statements RBRACE { $$ = $2; }
    | Type T_ID SC
        {
            $$ = std::make_shared<VarDecl>(
                std::dynamic_pointer_cast<ID>($2),
                std::dynamic_pointer_cast<Type>($1),
                nullptr
            );
        }
    | Type T_ID ASSIGN Exp SC
        {
            $$ = std::make_shared<VarDecl>(
                std::dynamic_pointer_cast<ID>($2),
                std::dynamic_pointer_cast<Type>($1),
                std::dynamic_pointer_cast<Exp>($4)
            );
        }
    | T_ID ASSIGN Exp SC
        {
            $$ = std::make_shared<Assign>(
                std::dynamic_pointer_cast<ID>($1),
                std::dynamic_pointer_cast<Exp>($3)
            );
        }
    | T_ID LBRACK Exp RBRACK ASSIGN Exp SC
        {
            $$ = std::make_shared<ArrayAssign>(
                std::dynamic_pointer_cast<ID>($1),
                std::dynamic_pointer_cast<Exp>($6),
                std::dynamic_pointer_cast<Exp>($3)
            );
        }
    | Type T_ID LBRACK Exp RBRACK SC
        {
            $$ = std::make_shared<VarDecl>(
                std::dynamic_pointer_cast<ID>($2),
                std::make_shared<ArrayType>(
                    std::dynamic_pointer_cast<PrimitiveType>($1)->type,
                    std::dynamic_pointer_cast<Exp>($4)
                ),
                nullptr
            );
        }
    | Call SC { $$ = $1; }
    | RETURN SC { $$ = std::make_shared<Return>(nullptr); }
    | RETURN Exp SC { $$ = std::make_shared<Return>(std::dynamic_pointer_cast<Exp>($2)); }
    | IF LPAREN Exp RPAREN Statement %prec LOWER_THAN_ELSE
        {
            $$ = std::make_shared<If>(
                std::dynamic_pointer_cast<Exp>($3),
                std::dynamic_pointer_cast<Statement>($5),
                nullptr
            );
        }
    | IF LPAREN Exp RPAREN Statement ELSE Statement
        {
            $$ = std::make_shared<If>(
                std::dynamic_pointer_cast<Exp>($3),
                std::dynamic_pointer_cast<Statement>($5),
                std::dynamic_pointer_cast<Statement>($7)
            );
        }
    | WHILE LPAREN Exp RPAREN Statement
        {
            $$ = std::make_shared<While>(
                std::dynamic_pointer_cast<Exp>($3),
                std::dynamic_pointer_cast<Statement>($5)
            );
        }
    | BREAK SC { $$ = std::make_shared<Break>(); }
    | CONTINUE SC { $$ = std::make_shared<Continue>(); }
    ;

Call
    : T_ID LPAREN ExpList RPAREN
        { $$ = std::make_shared<Call>(std::dynamic_pointer_cast<ID>($1), std::dynamic_pointer_cast<ExpList>($3)); }
    | T_ID LPAREN RPAREN
        { $$ = std::make_shared<Call>(std::dynamic_pointer_cast<ID>($1)); }
    ;

ExpList
    : Exp { $$ = std::make_shared<ExpList>(std::dynamic_pointer_cast<Exp>($1)); }
    | Exp COMMA ExpList
        {
            auto list = std::dynamic_pointer_cast<ExpList>($3);
            list->push_front(std::dynamic_pointer_cast<Exp>($1));
            $$ = list;
        }
    ;

Type
    : T_INT { $$ = std::make_shared<PrimitiveType>(BuiltInType::INT); }
    | T_BYTE { $$ = std::make_shared<PrimitiveType>(BuiltInType::BYTE); }
    | T_BOOL { $$ = std::make_shared<PrimitiveType>(BuiltInType::BOOL); }
    ;

Exp
    : LPAREN Exp RPAREN { $$ = $2; }
    | T_ID LBRACK Exp RBRACK
        { $$ = std::make_shared<ArrayDereference>(std::dynamic_pointer_cast<ID>($1), std::dynamic_pointer_cast<Exp>($3)); }
    | Exp BINOP Exp
        {
            $$ = std::make_shared<BinOp>(
                std::dynamic_pointer_cast<Exp>($1),
                std::dynamic_pointer_cast<Exp>($3),
                binop_from_lexeme(last_op)
            );
        }
    | T_ID { $$ = $1; }
    | Call { $$ = $1; }
    | NUM { $$ = $1; }
    | NUM_B { $$ = $1; }
    | T_STRING { $$ = $1; }
    | TRUE { $$ = std::make_shared<Bool>(true); }
    | FALSE { $$ = std::make_shared<Bool>(false); }
    | NOT Exp { $$ = std::make_shared<Not>(std::dynamic_pointer_cast<Exp>($2)); }
    | Exp AND Exp
        {
            $$ = std::make_shared<And>(std::dynamic_pointer_cast<Exp>($1), std::dynamic_pointer_cast<Exp>($3));
        }
    | Exp OR Exp
        {
            $$ = std::make_shared<Or>(std::dynamic_pointer_cast<Exp>($1), std::dynamic_pointer_cast<Exp>($3));
        }
    | Exp RELOP Exp
        {
            $$ = std::make_shared<RelOp>(
                std::dynamic_pointer_cast<Exp>($1),
                std::dynamic_pointer_cast<Exp>($3),
                relop_from_lexeme(last_op)
            );
        }
    | LPAREN Type RPAREN Exp %prec CAST
        {
            $$ = std::make_shared<Cast>(
                std::dynamic_pointer_cast<Exp>($4),
                std::dynamic_pointer_cast<PrimitiveType>($2)
            );
        }
    ;
%%

void yyerror(const char*) {
    output::errorSyn(yylineno);
}
