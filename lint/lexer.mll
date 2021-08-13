{
open Parser
exception Lexing of string
}

let digit = ['0'-'9']
let id = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let space = [' ' '\t' '\n' '\r']

rule lexer = parse
    | "ifz"
        { IFZ }
    | "then"
        { THEN }
    | "else"
        { ELSE }
    | "+"
        { PLUS }
    | "-"
        { MINUS }
    | "*"
        { TIMES }
    | "/"
        { DIV }
    | "("
        { LP }
    | ")"
        { RP }
    | '-'? digit+ as num
        { NUM (int_of_string num) }
    | "def"
        { DEF }
    | "="
        { ASSIGN }
    | "fun"
        { FUN }
    | "->"
        { ARROW }
    | id as text
        { ID text }
    | ";"
        { SEMI }
    | space
        { lexer lexbuf }
    | eof
        { EOF }
    | _
        { raise (Lexing (Lexing.lexeme lexbuf)) }