(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

open My_exception;;
open Type;; 
open Read;;
open Print;;
open Eval;;
    

(***********************************************************************)

let read_polish (filename:string) : program = failwith "TODO"

let print_polish (p:program) : unit = failwith "TODO"

let eval_polish (p:program) : unit = failwith "TODO"

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | _ -> usage ()
;;
(* lancement de ce main *)
(* let () = main () *)

(***********************************************************************)
(* Fonction de test *)
let rec print_list_string l = 
  match l with 
  | [] -> print_newline()
  | x :: ll -> print_string x; print_newline();print_list_string ll;;

let print_list_int_string_super l =
  print_string "[";
  let rec print_list_int_string l =
    match l with
    | [] -> print_string "]"
    | (s, x) :: ll -> print_string "(\"";print_string s; print_string "\"";print_string(","); 
                      print_int x;print_string ")"; print_string ";";
                      print_list_int_string ll;
  in print_list_int_string l;
  print_newline();;

let rec print_list_char lc =
  match lc with 
  | [] -> print_newline()
  | x :: ll -> print_char x; print_string(" ");print_list_char ll;;

let string_of_bool b = 
  if b then "true" else "false"
;;

let print_bool b = print_string(string_of_bool b);print_newline();;

let print_expr e = 
  let rec print_expr_aux e = match e with
  | Var(a) -> print_string(a);
  | Num(x) -> print_int(x);
  | Op(op, e1, e2) -> match op with 
    | Add -> print_string("(");print_expr_aux(e1);print_string(" + ");print_expr_aux(e2);print_string(")");
    | Sub -> print_string("(");print_expr_aux(e1);print_string(" - ");print_expr_aux(e2);print_string(")");
    | Mul -> print_string("(");print_expr_aux(e1);print_string(" * ");print_expr_aux(e2);print_string(")");
    | Div -> print_string("(");print_expr_aux(e1);print_string(" / ");print_expr_aux(e2);print_string(")");
    | Mod -> print_string("(");print_expr_aux(e1);print_string(" % ");print_expr_aux(e2);print_string(")");
  in print_expr_aux e; print_newline();;

(** TEST *)
let l = read_file "./exemples/mult_russe.p";;
print_list_int_string_super l;;

print_bool(is_number "-1");;
print_bool(is_variable "14a");;

let (e,ll) = read_expression ["/"; "truc"; "*"; "+"; "bla";"2";"3"] 0;;
print_expr e;;
(*read_block 0 l false;;*)
check_error_read (fun () -> read_block 0 l false);;