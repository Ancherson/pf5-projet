(** Toutes les exceptions + leurs fonctions associées *)

exception Error_Read of (int * string)


let check_error_read f = 
    try 
        Some(f())
    with Error_Read (num_line, message) -> 
        print_string "Erreur ligne ";
        print_int num_line;
        print_string " : ";
        print_string message;
        print_newline();
        None;
;;