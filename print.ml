(** Toutes les fonctions pour reprint les fichiers polish *)
open Type;;

let print_op (ope:op) : string = 
    match ope with
    | Add -> "+ "
    | Sub -> "- "
    | Mul -> "* "
    | Div -> "/ "
    | Mod -> "% "
;;

let print_comp (compa:comp) : string =
    match compa with
    | Eq -> "= " 
    | Ne -> "<> "
    | Lt -> "< "
    | Le -> "<= "
    | Gt -> "> "
    | Ge -> ">= "
;;

let rec print_expr (exp:expr) : string = 
    match exp with
    | Op(ope, e1, e2) -> (print_op ope) ^ (print_expr e1) ^ (print_expr e2)
    | Var(s) -> s ^ " "
    | Num(n) -> (string_of_int n) ^ " "
;;

let print_set (incr:string) (set : instr) : string =
    match set with
    |Set(name,exp) -> incr ^ name ^ " := " ^ print_expr(exp) ^ "\n"
    |_ -> ""
;;

let print_cond (condi: cond) : string =
    match condi with
    | (e1, compa, e2) -> (print_expr e1) ^ (print_comp compa) ^ (print_expr e2)
;;


let print_read (incr:string) (inst:instr) : string = 
    match inst with 
    | Read(s) -> incr ^ "READ " ^ s ^ "\n"
    | _ -> ""
;;

let print_print (incr:string) (inst:instr) : string =
    match inst with
    | Print(e) -> incr ^ "PRINT " ^ print_expr e ^ "\n"
    | _ -> ""
;;



let rec print_block (incr:string) (bloc : block) : string =
    match bloc with
    |(num,inst)::tail -> (print_instr incr inst)^ (print_block incr tail)
    |[] -> ""
and print_while (incr:string) (inst:instr) : string =
    match inst with
    |While (con,bloc) -> incr ^ "WHILE " ^ (print_cond con) ^ "\n"
    ^ (print_block (incr^"  ")) bloc
    |_ -> ""
and print_instr (incr:string) (inst:instr) : string =
    match inst with
    |Set(_,_) -> print_set incr inst
    |Read(_) -> print_read incr inst
    |Print(_) -> print_print incr inst
    |While(_,_) -> print_while incr inst
    |If(_,_,_) -> print_if incr inst
and print_if (incr:string) (inst:instr) : string =
    match inst with
    |If(con,bloc1,bloc2) -> let s = 
        incr ^ "IF " ^ (print_cond con) ^ "\n" 
        ^ print_block (incr^"  ") bloc1 in
        if bloc2 = []
            then s
        else s ^ incr ^ "ELSE" ^ "\n"
        ^ print_block (incr^"  ") bloc2
    |_ -> ""
;;
