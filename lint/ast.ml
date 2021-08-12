type exp = Int of int | Var of string | App of string * exp
    | Add of exp * exp | Sub of exp * exp
    | Mul of exp * exp | Div of exp * exp | Ifz of exp * exp * exp

type def = Declaration of string * string * exp

type prog = Program of def list * exp