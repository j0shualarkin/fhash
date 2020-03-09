(*  Joshua Larkin
    interp_cps.fs

    A continuation-passing style interpreter
    * continuations are anonymous functions

    This allows some interesting expressions in the language such as letcc and throw.
    Letcc grabs the current continuation and throw jumps out of the current continuation via a given continuation

    e.g. Letcc("k", Mult(Number 3, Throw(Var "k", Number 5))) evaluates to 5, not 15

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
//    | Letcc   of string * Expr
//    | Throw   of Expr * Expr
// we aren't quite ready for these yet, as adding the continuation to the environment is a type error >.<
// I believe data structural continuations will save the day...

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
let empty_k = fun v -> v

(* the interpreter *)
let rec valof (env:Env) (exp:Expr) k =
    match exp with
    | Number n          -> apply_k k (Numval n)
    | Boolean b         -> apply_k k (Boolval b)
    | Var x             -> apply_k k (apply_env env x)
    | Lambda (x, b)     -> apply_k k (Closure(x,b,env))
    | App (rator, rand) -> valof env rator (app_outer_k env rand k)
    | Ef (p, t, f)      -> valof env p (ef_k t f env k)
    | Sub1 n            -> valof env n (s1_k k)
    | Mult (m, n)       -> valof env m (outer_mult_k env n k)
    | Zero (n)          -> valof env n (zero_k k)
    | Let (x,e,b)       -> valof env e (let_k env x b k)
//    | Letcc (x,b)       -> valof (update_env env x k) b k
//    | Throw (kexp,vexp) -> valof env kexp (fun kval -> valof env vexp kval)

and apply_k k v =
    k v
and app_inner_k rator k =
    fun v -> apply_closure rator v k
and app_outer_k env rand k =
    fun v ->  valof env rand (app_inner_k v k)
and ef_k t f env k=
    fun v -> eval_ef v t f env k
and s1_k k =
    fun v -> eval_s1 v k
and inner_mult_k m k =
    fun v -> eval_mult m v k
and outer_mult_k env n k =
    fun v -> valof env n (inner_mult_k v k)
and zero_k k =
    fun v -> eval_zero v k
and let_k env x b k =
    fun v -> valof (update_env env x v) b k
(* end of cont helpers, begin interp helpers *)
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
    | Numval n -> apply_k k (Numval (n - 1))
    | _ -> raise (Oops("sub1 was not given a number"))
and eval_mult m n k =
    match (m, n) with
    | Numval m, Numval n -> apply_k k (Numval (m * n))
    | _,_ -> raise (Oops("multiplication of two numbers"))
and eval_zero n k =
    match n with
    | Numval n -> apply_k k (Boolval (n = 0))
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