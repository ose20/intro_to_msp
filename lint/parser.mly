%{
open Ast
%}

%token <int> NUM
%token <string> ID
%token NUM IFZ THEN ELSE PLUS MINUS TIMES DIV LP RP
%token DEF ASSIGN FUN ARROW ID SEMI EOF


//    生成規則 X -> a b c ... の優先順位は，
//    -> の右辺の最も右側にある終端記号の優先順位になる．
//    従って，exp -> ifz exp then exp else exp
//    の優先順位は else の優先順位になる．


%nonassoc ELSE
%left PLUS MINUS
%left TIMES DIV
%left prec_app

%type <Ast.prog> prog
%start prog

%%

prog:
    | defs exp EOF
        { Program($1, $2) }

defs:
    | def defs
        { $1 :: $2 }
    | 
        { [] }

def:
    | DEF ID ASSIGN FUN ID ARROW exp SEMI
        { Declaration($2, $5, $7) }

exp:
    | simple_exp
        { $1 }
    | exp PLUS exp
        { Add($1, $3) }
    | exp MINUS exp
        { Sub($1, $3) }
    | exp TIMES exp
        { Mul($1, $3) }
    | exp DIV exp
        { Div($1, $3) }
    | IFZ exp THEN exp ELSE exp
        { Ifz($2, $4, $6) }
    | ID simple_exp
        %prec prec_app
        { App($1, $2) }

simple_exp:
    | NUM
        { Int $1 }
    | ID
        { Var $1 }
    | LP exp RP
        { $2 }

%%