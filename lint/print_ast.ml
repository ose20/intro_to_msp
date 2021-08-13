open Ast
open Printf

exception Syserr of string

let _ = Printf.printf "hello world\n"
let add_semi str =
    if str = "" then str else str ^ "; "
let rec string_of_prog prog =
    match prog with
    | Program(d_l, e) ->
        sprintf "Program([%s], %s)" 
        (List.fold_left (fun s d -> add_semi s ^ string_of_def d) "" d_l)
        (string_of_exp e)
and string_of_def def =
    match def with
    | Declaration(f, x, e) ->
        sprintf "Declaration(%s, %s, %s)" f x (string_of_exp e) 
and string_of_exp exp =
    match exp with
    | Int i -> sprintf "Int (%s)" (string_of_int i)
    | Var s -> sprintf "Var (%s)" s
    | App(f, e) -> sprintf "App(%s, %s)" f (string_of_exp e)
    | Add(e1, e2) -> sprintf "Add(%s, %s)" (string_of_exp e1) (string_of_exp e2)
    | Sub(e1, e2) -> sprintf "Sub(%s, %s)" (string_of_exp e1) (string_of_exp e2)
    | Mul(e1, e2) -> sprintf "Mul(%s, %s)" (string_of_exp e1) (string_of_exp e2)
    | Div(e1, e2) -> sprintf "Div(%s, %s)" (string_of_exp e1) (string_of_exp e2)
    | Ifz(e1, e2, e3) -> sprintf "Ifz(%s, %s, %s)" (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)

let asert_with_exn boolean exn =
    if boolean then raise exn   
let main () =
    (* asert_with_exn (Array.length Sys.argv != 2) (Syserr "just one input file is needed"); *)
    let cin = open_in Sys.argv.(1) in
    let lexbuf = Lexing.from_channel cin in
    Printf.printf "%s\n" (string_of_prog @@ Parser.prog Lexer.lexer lexbuf)

let _ = main ()