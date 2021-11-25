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

let read_file (filename:string) : (string * int) list =
  let ci = open_in filename in
  let rec read ci num = 
    try 
      let line = input_line ci in
      (line, num) :: (read ci (num + 1))
    with End_of_file -> []
  in read ci 1;;


let is_string_from_alpha (mot:string) (alpha:string) (start:int) =
  let size_alpha = String.length alpha
  and size_mot = String.length mot in
  let rec is_string_from_alpha_loop i =
    let rec is_in_string c j =
      if j >= size_alpha then false
      else ((alpha.[j] = c) || (is_in_string c (j+1)))
    in if i >= size_mot then true
    else ((is_in_string mot.[i] 0) && (is_string_from_alpha_loop (i+1)))
  in is_string_from_alpha_loop start;;


let is_number (mot:string) = 
  if(mot.[0] = '-') then (mot <> "-") && is_string_from_alpha mot "0123456789" 1
  else is_string_from_alpha mot "0123456789" 0;;
let is_operator (mot:string) = List.exists (fun elt -> mot = elt) ["+"; "-"; "*"; "/"; "%"]
let is_variable (mot:string) = 
  List.for_all (fun elt -> (mot <> elt)) ["+"; "-"; "*"; "/"; "%"; "<"; ">"; "<="; ">="; "="; "<>"; ":="]
  && (not (is_number mot));;

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
    else if (List.exists (fun elt -> (elt = x)) ["<"; ">"; "<="; ">="; "="; "<>"; ":="]) then failwith "not a variable"
    else (Var(x), ll)
;;

let count_first_space (line:string) :int =
  let size = String.length line in
  let rec count_first_space_rec acc (line:string) :int=
    if (acc < size) 
      then if line.[acc] <> ' '
        then acc 
      else count_first_space_rec (acc+1) line
    else -1
    in
  count_first_space_rec 0 line;;

let rec suppr_empty_string_list (l:string list) :string list =
    match l with
    |[] -> []
    |x::tail -> if x = "" then suppr_empty_string_list tail
      else x::suppr_empty_string_list tail
;;
  
let get_line_elem_and_Nspace (line : string) : int * string list =
  (count_first_space line ,suppr_empty_string_list (String.split_on_char ' ' line ));;


let rec read_block (pro : int) (lines : (string * int) list) :block * ((string * int) list) =
  match lines with
  | [] -> ([],[])
  | (string_line,pos)::next_lines -> let (space_num,line) = get_line_elem_and_Nspace string_line in
    if space_num < pro 
      then if (space_num mod 2) = 0 
        then ([],next_lines)
      else failwith "Une ligne n'a pas de profondeur paire"
    else if space_num > pro 
      then failwith "Probleme indentation"
    else let (instruction, next_block_lines)= read_instr space_num line pos next_lines in
      let (block1,next_next) = read_block space_num next_block_lines in
      if instruction = None 
        then ((block1 :block), (next_next: (string*int) list))
      else (((pos,Option.get instruction)::block1 :block), (next_next : (string*int) list))

and read_instr (pro:int) (line: string list) (position: int) (next_lines: (string*int) list) : 'instr option * (string * int) list=
  match line with 
  |[] -> failwith "Pb instruction"
  |x::tail -> match x with 
    |"COMMENT" -> (None,next_lines)
    |"READ" -> if (List.tl tail) = [] 
      then (Some (Read(List.hd tail)),next_lines)
      else failwith ("trop d'arguments pour READ")
    |"PRINT" -> let (expression,empty_list) = read_expression tail in
      if empty_list = [] 
        then (Some (Print(expression)),next_lines)
      else failwith ("trop d'arguments pour PRINT")
    |"ELSE" -> failwith "manque un IF"
    |_ -> let variable = List.hd tail in
      if (List.nth tail 1) = ":="
        then let (expression,empty_list) = read_expression (List.tl (List.tl tail)) in
        if empty_list = [] 
          then (Some (Set(variable,expression)),next_lines)
        else failwith ("trop d'arguments sur la ligne")
      else failwith ("ce n'est pas une attribution de varirable")
;;


let aux_read_condition (mots :string list) (first_expr :expr) (comparator : comp) : cond =
  let (last_expr,tail2) = read_expression mots in
  match tail2 with
  |[] -> (first_expr,comparator,last_expr)
  |_ -> failwith "not only a condition"
;;

let read_condition (mots :string list)  : cond=
  let (first_expr,tail1) = read_expression mots in
  match tail1 with 
  |[] -> failwith "No condition"
  |x::ll -> match x with
    |"=" ->(*Equals *) aux_read_condition ll first_expr Eq
    |"<" ->(*Inf *) aux_read_condition ll first_expr Lt
    |">" ->(*Sup *) aux_read_condition ll first_expr Gt
    |"<>" ->(*Diff *) aux_read_condition ll first_expr Ne
    |"<=" ->(*Equals or inf *) aux_read_condition ll first_expr Le
    |">=" ->(*Equals or sup *) aux_read_condition ll first_expr Ge
    | _ -> failwith "Not a condition"
;;

let read_print (mots: string list) : instr = 
  match mots with
  | [] -> failwith "too few argument print"
  | l -> let (exp, rest) = read_expression l in
         if rest <> [] then failwith "not an expression print"
         else Print(exp)
             

let read_read (mots: string list) : instr = 
  match mots with
  | [] -> failwith "too few argument read"
  | x :: ll -> if ll <> [] then failwith "too much arguments read"
               else if is_variable x then Read(x)
               else failwith "not a variable read"
             

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

let rec print_list_int_string l =
  match l with
  | [] -> print_newline()
  | (s, x) :: ll -> print_int x;print_string(" "); 
                    print_string s;print_newline(); 
                    print_list_int_string ll;;

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
print_list_int_string l;;

print_bool(is_number "-1");;
print_bool(is_variable "14a");;

let (e,ll) = read_expression ["/"; "truc"; "*"; "+"; "bla";"2";"3"];;
print_expr e;;