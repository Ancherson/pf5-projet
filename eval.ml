(** Toutes les fonctions pour l'evaluation d'un programme polish *)
open Type;;
open My_exception;;


module Env = Map.Make (String) ;;

let eval_op (o:op) :int -> int -> int =
    match o with
    |Add -> (fun n1 n2 -> n1 + n2 )
    |Sub -> (fun n1 n2 ->  n1 - n2)
    |Mul -> (fun n1 n2 -> n1 * n2 )
    |Div -> (fun n1 n2 -> if n2 = 0 then raise (Error_Eval "division by zero")
        else n1 / n2)
    |Mod -> (fun n1 n2 -> if n2 = 0 then raise (Error_Eval "division by zero")
        else n1 mod (n2))
;;

let rec eval_expr (ex : expr) (env: int Env.t) :int =
    match ex with
    |Num x -> x
    |Var x -> Env.find x env
    |Op (o,exp1,exp2) -> eval_op o (eval_expr exp1 env) (eval_expr exp2 env)
;;

let eval_comp (com : comp) : int -> int -> bool =
    match com with 
    | Eq -> (fun x y -> x = y)
    | Ne -> (fun x y -> x <> y) 
    | Lt -> (fun x y -> x < y)
    | Le -> (fun x y -> x <= y)
    | Gt -> (fun x y -> x > y)
    | Ge -> (fun x y -> x >= y)

let eval_cond (co : cond) (env: int Env.t) : bool =
    match co with
    | (e1, com, e2) -> (eval_comp com) (eval_expr e1 env) (eval_expr e2 env)

(*à chaque appel de eval_set faire :
let env = eval_set xxxx env;; *)
let eval_set (set:instr) (env: int Env.t) : int Env.t=
    match set with
    |Set(name,exp) -> let num = eval_expr exp env in
        if not(Env.mem name env) then let env = Env.add name num env in
           env
        else
           let env = Env.remove name env in
           let env = Env.add name num env in
           env
    |_ -> raise (Error_Eval "Should be a Set")
;;

let eval_print (print:instr) (env: int Env.t) :unit=
    match print with
    |Print(exp) -> print_int (eval_expr exp env);
        print_string "\n"
    |_ -> raise (Error_Eval "Should be a Print")
;;

let eval_read (read : instr) (env: int Env.t) : int Env.t =
    match read with
    | Read(s) ->   
        print_string (s ^ "? ");
        let num = read_int() in 
        if not(Env.mem s env) then let env = Env.add s num env in
           env
        else
           let env = Env.remove s env in
           let env = Env.add s num env in
           env
    | _ -> raise (Error_Eval "Should be a read")
;;

let rec eval_block (bloc : block) (env : int Env.t) : int Env.t = 
    match bloc with
    | [] -> env
    | (num,inst)::tail ->
        let env = (eval_instr inst env) in
        eval_block tail env

and eval_instr (ins : instr) (env : int Env.t) : int Env.t =
    match ins with
    | Set(_,_) -> eval_set ins env
    | Read(_) -> eval_read ins env
    | Print(_) -> (eval_print ins env); env
    | If(_,_,_) -> eval_if ins env
    | While(_,_) -> eval_while ins env

and eval_if (ins : instr) (env : int Env.t) : int Env.t =
    match ins with
    | If(con, b1, b2) ->
        if eval_cond con env then eval_block b1 env
        else eval_block b2 env
    | _ -> raise (Error_Eval "Should be  a if")

and eval_while (ins : instr) (env : int Env.t) = 
    match ins with
    | While(con, b) -> 
        let rec while_loop co bloc env  =
            if eval_cond co env
                then while_loop co bloc (eval_block bloc env)
                else env
        in while_loop con b env
    | _ -> raise (Error_Eval "Should be a while")
        

let eval_prog (p : program) : unit =
    let env = Env.empty in
    let e = eval_block p env in ()
;;
        


