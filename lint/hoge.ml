type exp = Int of int | Var of string | App of string * exp
    | Add of exp * exp | Sub of exp * exp
    | Mul of exp * exp | Div of exp * exp | Ifz of exp * exp * exp

type def = Declaration of string * string * exp
type prog = Program of def list * exp

let prog1 =
    Program (
        [Declaration ("fact", "x",
            Ifz (Var "x",
                Int 1,
                Mul (Var "x",
                    App ("fact",
                        Sub (Var "x",
                            Int 1)))))
        ],
        App ("fact", Int 10)
    )

exception Yikes of string


let env0 : string -> int = fun x -> raise (Yikes "unbound variable while refering environment")
let fenv0 : string -> (int -> int) = fun x -> raise (Yikes "unbound variable while refering function environment")

(* update : ('a -> 'b) -> 'a -> 'b -> 'a -> 'b *)
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

let prog2 =
    Program([
        Program (
        [Declaration ("fact", "x",
            Ifz (Var "x",
                Int 1,
                Mul (Var "x",
                    App ("fact",
                        Sub (Var "x",
                            Int 1)))))
        ],
        App ("fact", Int 2)
    ])