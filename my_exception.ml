(** Toutes les exceptions + leurs fonctions associées *)

exception Error_Read of (int * string)

let get_string_error_read (e: (int * string)) : string =
    match e with
    | (n, s) ->
        "Error line " ^ (string_of_int n) ^ " : " ^ s ^ "\n"
;;

let print_error_read (e: (int * string)) : unit = 
    print_string (get_string_error_read e)
;;