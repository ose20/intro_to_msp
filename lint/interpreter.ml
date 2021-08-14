open Ast

exception Yikes of string
let env0 : string -> int = fun x -> raise (Yikes "unbound variable while refering environment")
let fenv0 : string -> (int -> int) = fun x -> raise (Yikes "unbound variable while refering function environment")

let update env x v = fun y -> if x = y then v else env y

(* eval1 : exp -> (string -> int) -> (string -> (int -> int)) -> int *)
let rec eval1 e env fenv =
    match e with
    | Int i -> i
    | Var s -> env s
    | App (f, e2) -> (fenv f) (eval1 e2 env fenv)
    | Add (e1, e2) -> (eval1 e1 env fenv) + (eval1 e2 env fenv)
    | Sub (e1, e2) -> (eval1 e1 env fenv) - (eval1 e2 env fenv)
    | Mul (e1, e2) -> (eval1 e1 env fenv) * (eval1 e2 env fenv)
    | Div (e1, e2) -> (eval1 e1 env fenv) / (eval1 e2 env fenv)
    | Ifz (e1, e2, e3) ->
        if (eval1 e1 env fenv) = 0
        then (eval1 e2 env fenv) else (eval1 e3 env fenv)

let rec peval1 p env fenv =
    match p with
    | Program ([], e) -> eval1 e env fenv
    | Program (Declaration (s1, s2, e1) :: tl, e) ->
        let rec f = fun x -> eval1 e1 (update env s2 x) (update fenv s1 f)
        in peval1 (Program(tl, e)) env (update fenv s1 f)
