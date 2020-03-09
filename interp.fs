(*  Joshua Larkin 
    Interp.fs
    Small interpreter like those discussed in 
      "Essentials of Programming Languages"
         by Dan Friedman and Mitch Wand 
         
    Note: some defintions are followed by the "and" keyword
    this is how mutual recursion is done in F#
    
    I allow the `ef` (aka `if`) expression to take any value as predicate,
    with the number 0 and false being the only ``false`` values.
    I.e., closures and nonzero numbers are effectively ``true`` values.
    This behavior is like that of the Racket programming language.
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


(* the interpreter *)
let rec valof (env:Env) (exp:Expr) =
    match exp with
    | Number n          -> Numval n
    | Boolean b         -> Boolval b
    | Var x             -> apply_env env x
    | Lambda (x, b)     -> Closure(x,b,env)
    | App (rator, rand) -> apply_closure (valof env rator) (valof env rand)
    | Ef (p, t, f)      -> eval_ef (valof env p) t f env
    | Sub1 n            -> eval_s1 (valof env n) 
    | Mult (m, n)       -> eval_mult (valof env m) (valof env n)
    | Zero (n)          -> eval_zero (valof env n)
    
and apply_closure (f:Val) a =
    match f with 
    | Closure (x, b, env) -> valof (update_env env x a) b
    | _ -> raise (Oops("apply closure not given a closure as operator"))
    
and eval_ef p t f env =
    let q = match p with
            | Boolval b -> b      
            | Numval n  -> n <> 0
            | _         -> true   (*closures are true values*)
    if q then (valof env t) else (valof env f) 
    
and eval_s1 n = 
    match n with 
    | Numval n -> Numval (n - 1)
    | _ -> raise (Oops("sub1 was not given a number"))
    
and eval_mult m n =
    match (m, n) with
    | Numval m, Numval n -> Numval (m * n)
    | _,_ -> raise (Oops("multiplication of two numbers"))
    
and eval_zero n = 
    match n with
    | Numval n -> Boolval (n = 0)
    | _ -> raise (Oops("zero? not given a number"))

(* end of function: valof *)

(* a few utility functions to make running/viewing examples painless *)
let run_val (r:Val) =
    match r with
    | Numval n          -> string n
    | Boolval b         -> string b
    | Closure (x,b,env) -> "a closure"

let eval (e:Expr) =
    run_val (valof empty_env e)

let show (e:Expr) = 
    printf "result is %s" (eval e)

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
