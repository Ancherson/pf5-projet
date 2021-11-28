(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

open My_exception;;

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

let rec read_expression (l:string list) (num_line: int) : expr * string list =
  match l with
  | [] -> raise(Error_Read (num_line, "It's not an expression !"))
  | x :: ll -> 
    if is_number x then (Num(int_of_string x), ll)
    else if is_operator x then
      let (e1, lll) = read_expression ll num_line in
      let (e2, end_list) = read_expression lll num_line in
      match x with
      | "+" -> (Op(Add, e1, e2), end_list)
      | "-" -> (Op(Sub, e1, e2), end_list)
      | "*" -> (Op(Mul, e1, e2), end_list)
      | "/" -> (Op(Div, e1, e2), end_list)
      | "%" -> (Op(Mod, e1, e2), end_list)
      | _ -> raise(Error_Read (num_line, "Not an expression !"))
    else if (List.exists (fun elt -> (elt = x)) ["<"; ">"; "<="; ">="; "="; "<>"; ":="]) 
      then raise(Error_Read (num_line, "Not an expression, wait a variable !"))
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


let aux_read_condition (mots :string list) (first_expr :expr) (comparator : comp) (num_line: int) : cond =
  let (last_expr,tail2) = read_expression mots num_line in
  match tail2 with
  |[] -> (first_expr,comparator,last_expr)
  |_ -> raise(Error_Read (num_line, "It's not only a condition"))
;;

let read_condition (mots :string list) (num_line: int) : cond=
  let (first_expr,tail1) = read_expression mots num_line in
  match tail1 with 
  |[] -> raise(Error_Read (num_line, "It's just an expression, not a condition !"))
  |x::ll -> match x with
    |"=" ->(*Equals *) aux_read_condition ll first_expr Eq num_line
    |"<" ->(*Inf *) aux_read_condition ll first_expr Lt num_line
    |">" ->(*Sup *) aux_read_condition ll first_expr Gt num_line
    |"<>" ->(*Diff *) aux_read_condition ll first_expr Ne num_line
    |"<=" ->(*Equals or inf *) aux_read_condition ll first_expr Le num_line
    |">=" ->(*Equals or sup *) aux_read_condition ll first_expr Ge num_line
    | _ -> raise(Error_Read (num_line, "It's not a condition, wait a comparator !"))
;;

let read_print (mots: string list) (num_line: int) : instr = 
  match mots with
  | [] -> raise(Error_Read (num_line, "Need argument after Print"))
  | l -> 
    let (exp, rest) = read_expression l num_line in
    if rest <> [] then raise(Error_Read (num_line, "Too much argument after PRINT"))
    else Print(exp)
             

let read_read (mots: string list) (num_line: int) : instr = 
  match mots with
  | [] -> raise(Error_Read (num_line, "Need a variable after READ"))
  | x :: empty_list -> 
    if empty_list <> [] 
      then raise(Error_Read (num_line, "Too much argument after READ"))
    else if is_variable x then Read(x)
    else raise(Error_Read (num_line, "Wait a variable name after READ"))

let read_set(mots: string list) (num_line: int) : instr = 
  match mots with
  | [] -> raise(Error_Read (num_line, "Erreur set"))
  | x1 :: ll -> if not (is_variable x1) then raise(Error_Read (num_line, "Not a variable name in the SET instruction !"))
    else
      match ll with
      | [] -> raise(Error_Read (num_line, "Not just a variable in a SET instruction !"))
      | x2 :: lll -> if x2 <> ":=" then raise(Error_Read (num_line, "Need := after the variable in a SET instruction !"))
        else 
          let (ex, empty_list) = read_expression lll num_line in
          if empty_list <> [] then raise(Error_Read (num_line, "Need an expression after := !"))
          else Set(x1, ex)

let rec read_block (pro : int) (lines : (string * int) list) (is_else: bool) :block * ((string * int) list) =
  match lines with
  | [] -> ([],[])
  | (string_line,pos)::next_lines -> 
    let (space_num,line) = get_line_elem_and_Nspace string_line in
    if space_num < pro 
      then if (space_num mod 2) = 0 
        then ([],lines)
      else raise(Error_Read (pos, "Error indentation not an even !"))
    else if space_num > pro 
      then raise(Error_Read (pos, "Error indentation too deep !"))
    else if is_else
      then read_else pro line lines pos
    else let (instruction, next_block_lines)= read_instr space_num line pos next_lines in
      let (block1,next_next) = read_block space_num next_block_lines false in
      if instruction = None 
        then ((block1 :block), (next_next: (string*int) list))
      else ((pos,Option.get instruction)::block1, next_next)

and read_instr (pro:int) (line: string list) (position: int) (next_lines: (string*int) list) : 'instr option * (string * int) list=
  match line with 
  |[] -> raise(Error_Read (position, "Erreur empty line !"))
  |x::tail -> match x with 
    |"COMMENT" -> (None,next_lines)
    |"READ" -> (Some (read_read tail position),next_lines)
    |"PRINT" -> (Some (read_print tail position),next_lines)
    |"ELSE" -> raise(Error_Read (position, "ELSE without IF precendently !"))
    |"WHILE" -> let (while_instr,line_after_Wblock) = read_while pro tail next_lines position in
      (Some(while_instr),line_after_Wblock)
    |"IF" -> let (if_instr, line_after_Iblock) = read_if pro tail next_lines position in
      (Some(if_instr), line_after_Iblock)
    |_ -> (Some (read_set line position),next_lines)
and read_while (pro: int) (mots: string list) (lignes: (string * int) list) (num_line: int) : instr * ((string * int) list)=
  let condition = read_condition mots num_line in
  let block1,next = read_block (pro + 2) lignes false in
  (While(condition, block1), next)

and read_else (pro: int) (mots: string list) (lignes: (string * int) list) (num_line: int) : block * ((string * int) list) = 
  match mots with
  | [] -> raise(Error_Read (num_line, "Erreur ELSE"))
  | x :: tail -> 
    if(x = "ELSE" && tail = []) 
      then read_block (pro + 2) (List.tl lignes) false
    else read_block pro lignes false

and read_if (pro: int) (mots: string list) (lignes: (string * int) list) (num_line: int) : instr * ((string * int) list)=
  let condition = read_condition mots num_line in
  let block1, next = read_block (pro + 2) lignes false in
  let block2, next_next = read_block pro next true in
  (If(condition, block1, block2), next_next)
                      
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