(*  Joshua Larkin
    interp_cps.fs

    A continuation-passing style interpreter
    * continuations are anonymous functions
*)

(* Basic exception to be thrown when we encounter bad data *)
exception Oops of string


(* the target language of our interpreter *)
type Expr =
    | Number  of int
    | Var     of string
    | Boolean of bool
    | Lambda  of string * Expr
    | App     of Expr * Expr
    | Ef      of Expr * Expr * Expr
    | Sub1    of Expr
    | Mult    of Expr * Expr
    | Zero    of Expr
    | Let     of string * Expr * Expr

(* Data-structural environments *)
type Env = Map<string,Val>
and Val =
    | Numval  of int
    | Boolval of bool
    | Closure of string * Expr * Env

let empty_env = Map.empty

let update_env (env:Env) (x:string) (a:Val) =
    env.Add(x, a)

let apply_env (env:Env) (x:string) =
    env.[x]

(* continuation helpers *)
let empty_k = fun x -> x

(* the interpreter *)
let rec valof (env:Env) (exp:Expr) k =
    match exp with
    | Number n          -> k (Numval n)
    | Boolean b         -> k (Boolval b)
    | Var x             -> k (apply_env env x)
    | Lambda (x, b)     -> k (Closure(x,b,env))
    | App (rator, rand) -> valof env rator (fun rator ->  valof env rand (fun rand -> apply_closure rator rand k))
    | Ef (p, t, f)      -> valof env p (fun p -> eval_ef p t f env k)
    | Sub1 n            -> valof env n (fun n -> eval_s1 n k)
    | Mult (m, n)       -> valof env m (fun m -> valof env n (fun n -> eval_mult m n k))
    | Zero (n)          -> valof env n (fun n -> eval_zero n k)
    | Let (x,e,b)       -> valof env e (fun v -> valof (update_env env x v) b k)

and apply_closure (f:Val) a k =
    match f with
    | Closure (x, b, env) -> valof (update_env env x a) b k
    | _ -> raise (Oops("apply closure not given a closure as operator"))

and eval_ef p t f env k =
    let q = match p with
            | Boolval b -> b
            | Numval n  -> n <> 0
            | _         -> true   (*closures are true values*)
    if q then (valof env t k) else (valof env f k)

and eval_s1 n k =
    match n with
    | Numval n -> k (Numval (n - 1))
    | _ -> raise (Oops("sub1 was not given a number"))

and eval_mult m n k =
    match (m, n) with
    | Numval m, Numval n -> k (Numval (m * n))
    | _,_ -> raise (Oops("multiplication of two numbers"))

and eval_zero n k =
    match n with
    | Numval n -> k (Boolval (n = 0))
    | _ -> raise (Oops("zero? not given a number"))


(* end of function: valof *)

(* a few utility functions to make running/viewing examples painless *)
let run_val (r:Val) =
    match r with
    | Numval n          -> string n
    | Boolval b         -> string b
    | Closure (x,b,env) -> "a closure"

let eval (e:Expr) =
    run_val (valof empty_env e empty_k)

let show (e:Expr) =
    printf "result is %s\n" (eval e)

(* Some examples *)

let fact_5 =
    App(
        App(Lambda("f",
                Lambda("n",
                    Ef(Zero(Var "n"), Number 1, Mult(Var "n", App(App(Var "f", Var "f"), Sub1(Var "n"))))
                    )),
            Lambda("f",
                Lambda("n",
                    Ef(Zero(Var "n"), Number 1, Mult(Var "n", App(App(Var "f", Var "f"), Sub1(Var "n"))))))),
        Number 5)

show fact_5

let fact_5_with_let =
    Let("!",
        Lambda("f",
            Lambda("n",
                Ef(Zero(Var "n"),
                   Number 1,
                   Mult(Var "n", App(App(Var "f", Var "f"), Sub1(Var "n")))))),
        App(App(Var "!", Var "!"),Number 5))

show fact_5_with_let