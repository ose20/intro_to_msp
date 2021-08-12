{
open Parser
exception Lexing of string
}

let digit = ['0'-'9']
let id = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let space = [' ' '\t' '\n' '\r']

