open Type;;
open Eval;;

let simple_plus (ex1:expr) (ex2:expr) : expr =
    match (ex1, ex2) with
    | (Num(a), Num(b)) -> Num(a + b)
    | (Num(a), ex2) when a = 0 -> ex2
    | (ex1, Num(b)) when b = 0 -> ex1
    | (ex1, ex2) -> Op(Add, ex1, ex2)
;;

let simple_moins (ex1:expr) (ex2:expr) : expr =
    match (ex1, ex2) with
    | (Num(a), Num(b)) -> Num(a - b)
    | (Num(a), ex2) when a = 0 -> ex2
    | (ex1, ex2) when ex1 = ex2 -> Num(0)
    | (ex1, ex2) -> Op(Sub, ex1, ex2)
;;

let simple_fois (ex1:expr) (ex2:expr) : expr =
    match (ex1, ex2) with
    | (Num(a), Num(b)) -> Num(a * b)
    | (Num(a), ex2) when a = 0 -> Num(0)
    | (ex1, Num(a)) when a = 0 -> Num(0)
    | (Num(a), ex2) when a = 1 -> ex2
    | (ex1, Num(b)) when b = 1 -> ex1
    | (ex1, ex2) -> Op(Mul, ex1, ex2)
;;

let simple_div (ex1:expr) (ex2:expr) : expr = 
    match (ex1, ex2) with
    | (Num(a), Num(b)) when b <> 0 -> Num(a / b)
    | (Num(a), ex2) when (a = 0 && ex2 <> Num(0)) -> Num(0)
    | (ex1, Num(b)) when b = 1 -> ex1
    | (ex1, ex2) when ex1 = ex2 -> Num(1)
    | (ex1, ex2) -> Op(Div, ex1, ex2)
;;

let simple_mod (ex1:expr) (ex2:expr) : expr = 
    match (ex1, ex2) with
    | (Num(a), Num(b)) when b <> 0 -> Num(a mod b)
    | (Num(a), ex2) when (a = 0 && ex2 <> Num(0)) -> Num(0);
    | (ex1, Num(b)) when b = 1 -> Num(0)
    | (ex1, ex2) when ex1 = ex2 -> Num(0)
    | (ex1, ex2) -> Op(Mod, ex1, ex2)
;;

let simple_op (ope:op) (e1:expr) (e2:expr) : expr = 
    match ope with 
    | Add -> simple_plus e1 e2
    | Sub -> simple_moins e1 e2
    | Mul -> simple_fois e1 e2
    | Div -> simple_div e1 e2
    | Mod -> simple_mod e1 e2
;;


let rec simple_expr (ex:expr) : expr = 
    match ex with
    | Op(ope, e1, e2) -> simple_op ope (simple_expr e1) (simple_expr e2)
    | x -> x
;;

let simple_cond (co:cond) : cond = 
    match co with 
    | (e1, com, e2) -> (simple_expr e1, com, simple_expr e2)
;;

let test_cond (co:cond) : bool option =
    match co with
    | (Num(a), com, Num(b)) -> Some((eval_comp com) a b)
    | x -> None
;;

let simple_print (ins:instr) : instr = 
    match ins with
    | Print(e) -> Print(simple_expr e)
    | _ -> failwith "pb print"
;;

let simple_set (ins:instr) : instr =
    match ins with
    | Set(n, e) -> Set(n, simple_expr e)
    | _ -> failwith "pb set"
;;

let rec simple_block (b:block) : block =
    match b with
    | [] -> []
    | (num, ins) :: tail -> (simple_instr ins num) @ (simple_block tail)

and simple_instr (ins:instr) (num:int) : block = 
    match ins with
    | Set(_,_) -> [(num, simple_set ins)]
    | Read(_) -> [(num, ins)]
    | Print(_) -> [(num, simple_print ins)]
    | If(_,_,_) -> simple_if ins num
    | While(_,_) -> simple_while ins num

and simple_if (ins:instr) (num:int) : block = 
    match ins with
    | If(co, b1, b2) ->
        (let co_simp = simple_cond co in
        match test_cond co_simp with 
        | None -> [(num,If(co_simp, simple_block b1, simple_block b2))]
        | Some(b) -> if b then simple_block b1 else simple_block b2)
    | _ -> failwith "pb simple if"

and simple_while (ins:instr) (num:int) : block = 
    match ins with
    | While(co, bloc) -> 
        (let co_simp = simple_cond co in
        match test_cond co_simp with
        | None -> [(num, While(co_simp, simple_block bloc))]
        | Some(b) -> 
            if b then [(num, While(co_simp, simple_block bloc))] (* BOUCLE INFINI *)
            else [])
    | _ -> failwith "pb simple while"

