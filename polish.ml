(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

open My_exception;;
open Type;; 
open Read;;
open Print;;
open Eval;;
open Simp;;
open Sign;;    


(***********************************************************************)

let read_polish (filename:string) : program = 
  try
    match (read_block 0 (read_file filename) false) with
    | (b, _) -> b
  with Error_Read(n,s) -> print_error_read (n,s); exit 1;

  ;;

let print_polish (p:program) : unit = print_string (print_block "" p);;

let print_polish (p:program) : unit = print_string (print_block "" p);;

let print_polish (p:program) : unit = print_string (print_block "" p);;

let eval_polish (p:program) : unit =
  try eval_prog(p)
  with Error_Eval (n,s) -> print_error_eval(n,s); exit 1;
;;

let simp_polish (p:program) : program = simple_block p 

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | [|_;"-simpl";file|] -> print_polish (simp_polish (read_polish file))
  | [|_;"-sign";file|] -> let (env, res) = (sign_block (read_polish file) (Env.empty)) in
      print_map_sign env; print_string res; print_newline();
  | _ -> usage ()
;;
(* lancement de ce main *)
let () = main () ;;

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

let print_bool b = print_string(string_of_bool b);;
let rec print_list_bool l =
  match l with
  | [] -> print_newline()
  | x :: ll -> print_bool x; print_string(" "); print_list_bool ll;;

let print_couple_sign x = 
  match x with
  | (s1,s2) -> print_string("(");print_sign s1;print_string(",");print_sign(s2);print_string(") ")
;;
