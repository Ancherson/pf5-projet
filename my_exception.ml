(** Toutes les exceptions + leurs fonctions associées *)

exception Error_Read of (int * string)
exception Error_Eval of (int * string)

let get_string_error_read (e: (int * string)) : string =
    match e with
    | (n, s) ->
        "Error syntax line " ^ (string_of_int n) ^ " : " ^ s ^ "\n"
;;

let print_error_read (e: (int * string)) : unit = 
    print_string (get_string_error_read e)
;;



let get_string_error_eval (e: (int * string)) : string =
    match e with
    | (n, s) ->
        "Error execution on line " ^ (string_of_int n) ^ " : " ^ s ^ "\n"
;;

let print_error_eval (e: (int * string)) : unit = 
    print_string (get_string_error_eval e)
;;