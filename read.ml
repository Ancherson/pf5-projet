(** Fonctions pour le read un fichier polish *)

open Type;;
open My_exception;;

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
         