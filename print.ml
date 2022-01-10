(** Toutes les fonctions pour reprint les fichiers polish *)
open Type;;

(* renvoie le string de l'operateur *)
let print_op (ope:op) : string = 
    match ope with
    | Add -> "+ "
    | Sub -> "- "
    | Mul -> "* "
    | Div -> "/ "
    | Mod -> "% "
;;

(* renvoie le string du comparateur *)
let print_comp (compa:comp) : string =
    match compa with
    | Eq -> "= " 
    | Ne -> "<> "
    | Lt -> "< "
    | Le -> "<= "
    | Gt -> "> "
    | Ge -> ">= "
;;

(* renvoie le string de l'expression *)
let rec print_expr (exp:expr) : string = 
    match exp with
    | Op(ope, e1, e2) -> (print_op ope) ^ (print_expr e1) ^ (print_expr e2)
    | Var(s) -> s ^ " "
    | Num(n) -> (string_of_int n) ^ " "
;;

(* renvoie le string de l'instruction set *)
let print_set (incr:string) (set : instr) : string =
    match set with
    |Set(name,exp) -> incr ^ name ^ " := " ^ print_expr(exp) ^ "\n"
    |_ -> ""
;;

(* renvoie le string d'une condition *)
let print_cond (condi: cond) : string =
    match condi with
    | (e1, compa, e2) -> (print_expr e1) ^ (print_comp compa) ^ (print_expr e2)
;;

(* renvoie le string de l'instruction read *)
let print_read (incr:string) (inst:instr) : string = 
    match inst with 
    | Read(s) -> incr ^ "READ " ^ s ^ "\n"
    | _ -> ""
;;

(* renvoie le string de l'instruction print *)
let print_print (incr:string) (inst:instr) : string =
    match inst with
    | Print(e) -> incr ^ "PRINT " ^ print_expr e ^ "\n"
    | _ -> ""
;;

(* renvoie le string d'un block d'instruction *)
let rec print_block (incr:string) (bloc : block) : string =
    match bloc with
    |(num,inst)::tail -> (print_instr incr inst)^ (print_block incr tail)
    |[] -> ""

(* renvoie le string de l'instruction while *)
and print_while (incr:string) (inst:instr) : string =
    match inst with
    |While (con,bloc) -> incr ^ "WHILE " ^ (print_cond con) ^ "\n"
    ^ (print_block (incr^"  ")) bloc
    |_ -> ""

(* renvoie le string d'une instruction *)
and print_instr (incr:string) (inst:instr) : string =
    match inst with
    |Set(_,_) -> print_set incr inst
    |Read(_) -> print_read incr inst
    |Print(_) -> print_print incr inst
    |While(_,_) -> print_while incr inst
    |If(_,_,_) -> print_if incr inst

(* renvoie le string de l'instruction if *)
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
