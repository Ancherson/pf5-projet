(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(*****************************************************************************)
(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
type position = int

(** Nom de variable *)
type name = string

(** Opérateurs arithmétiques : + - * / % *)
type op = Add | Sub | Mul | Div | Mod

(** Expressions arithmétiques *)
type expr =
  | Num of int
  | Var of name
  | Op of op * expr * expr

(** Opérateurs de comparaisons *)
type comp =
| Eq (* = *)
| Ne (* Not equal, <> *)
| Lt (* Less than, < *)
| Le (* Less or equal, <= *)
| Gt (* Greater than, > *)
| Ge (* Greater or equal, >= *)

(** Condition : comparaison entre deux expressions *)
type cond = expr * comp * expr

(** Instructions *)
type instr =
  | Set of name * expr
  | Read of name
  | Print of expr
  | If of cond * block * block
  | While of cond * block
and block = (position * instr) list

(** Un programme Polish est un bloc d'instructions *)
type program = block


(***********************************************************************)

let read_file (filename:string) : string list =
  let ci = open_in filename in
  let rec read ci = 
    try 
      let line = input_line ci in
      line :: (read ci)
    with End_of_file -> []
  in read ci;;


let explode str =
  let rec loop index acc =
    if index < 0 then acc
    else loop (index - 1) (str.[index] :: acc)
  in
  loop (String.length str - 1) []
;;

let rec is_in_list elt list = 
  match list with
  | [] -> false
  | x :: tail -> (x = elt) || (is_in_list elt tail)

let is_string_from_alpha (mot_string:string) (alpha_string:string) =
  let rec is_string_from_alpha_list (mot:char list) (alpha:char list): bool = 
    match mot with
    | [] -> true
    | x :: tail -> (is_in_list x alpha) && (is_string_from_alpha_list tail alpha)
  in let mot = explode mot_string and alpha = explode alpha_string
  in is_string_from_alpha_list mot alpha
;;

let is_number (mot:string) = is_string_from_alpha mot "0123456789"
let is_operator (mot:string) = is_string_from_alpha mot "+-*/%"

let rec read_expression (l:string list) : expr * string list =
  match l with
  | [] -> failwith "pbm" 
  | x :: ll -> 
    if is_number x then (Num(int_of_string x), ll)
    else if is_operator x then
      let (e1, lll) = read_expression ll in
      let (e2, end_list) = read_expression lll in
      match x with
      | "+" -> (Op(Add, e1, e2), end_list)
      | "-" -> (Op(Sub, e1, e2), end_list)
      | "*" -> (Op(Mul, e1, e2), end_list)
      | "/" -> (Op(Div, e1, e2), end_list)
      | "%" -> (Op(Mod, e1, e2), end_list)
      | _ -> failwith "pbm operator"
    else (Var(x), ll)
;;
    

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
let l = read_file "./exemples/abs.p";;
print_list_string l;;

let lc = explode "salut";;

print_bool(is_string_from_alpha "158648613" "0123456789");;

let (e,ll) = read_expression ["/"; "truc"; "*"; "+"; "bla";"2";"3"];;
print_expr e;;