open Ast
open Interpreter

exception Syserr of string

let asert_with_exn boolean exn =
    if boolean then raise exn        
let interpreter () =
    asert_with_exn (Array.length Sys.argv != 2) (Syserr "just one input file is needed");
    let cin = open_in Sys.argv.(1) in
    let lexbuf = Lexing.from_channel cin in
    let res = peval1 (Parser.prog Lexer.lexer lexbuf) env0 fenv0 in
    Printf.printf "%d\n" res

let _ = interpreter ()