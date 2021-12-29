open Type;;

let inverse_sign (s : sign) : sign =
    match s with
    | Pos -> Neg
    | Neg -> Pos
    | ss -> ss

let inverse_sign_list (l : sign list) : sign list = 
    List.map inverse_sign l

let rec union l1 l2 =
    match l1 with
    | [] -> l2
    | x :: ll -> 
        if mem x l2 then union ll l2
        else union ll (x :: l2)
;;

let rec inter l1 l2 =
    match l1 with
    | [] -> []
    | x :: ll -> 
        if mem x l2 then x :: (inter ll l2)
        else inter ll l2

let rec aux_compute f s1 l2 =
    match l2 with
    | [] -> []
    | s2 :: ll2 -> union (f s1 s2) (aux_compute f s1 ll2)

let rec compute f l1 l2 =
    match l1 with 
    | [] -> []
    | s1 :: ll1 -> union (aux_compute f s1 l2) (compute f ll1 l2)



let add (s1 : sign) (s2 : sign) : sign list =
    match (s1, s2) with 
    | (Pos, Pos) -> [Pos]
    | (Neg, Neg) -> [Neg]
    | (ss1, ss2) when ss1 = Error || ss2 = Error -> [Error]
    | (ss1, ss2) when ss1 = Zero  -> [ss2]
    | (ss1, ss2) when ss2 = Zero -> [ss1]
    | (_,_) -> [Pos, Zero, Neg]
;;


let sub (s1 : sign) (s2 : sign) : sign list =
    List.rev (add s1 (inverse_sign s2))
;;

let mul (s1 : sign) (s2 : sign) : sign list =
    match (s1, s2) with
    | (ss1, ss2) when ss1 = Error || ss2 = Error -> [Error]
    | (Pos, Pos) -> [Pos]
    | (Neg, Neg) -> [Pos]
    | (Pos, Neg) -> [Neg]
    | (Neg, Pos) -> [Neg]
    | (_,_) -> [Zero]
;;

let div (s1 : sign) (s2 : sign) : sign list =
    match (s1, s2) with
    | (ss1, ss2) when ss1 = Error || ss2 = Error -> [Error]
    | (Pos, Pos) -> [Pos, Zero]
    | (Neg, Neg) -> [Pos, Zero]
    | (Pos, Neg) -> [Neg, Zero]
    | (Neg, Pos) -> [Neg, Zero]
    | (Zero,ss2) when ss2 <> Zero -> [Zero]
    | (_,_) -> [Error]
;;

let modulo (s1 : sign) (s2 : sign) : sign list =
    if(s2 = Zero) then [Error]
    else if(s1 = Error || s2 = Error) then [Error]
    else [s1, Zero]
;;

let less (s1 : sign) (s2 : sign) : bool list =
    if s1 = Error || s2 = Error then [false]
    else if s1 = Pos 
        then if s2 = Pos then [true, false]
        else [false]
    else if s1 = Zero 
        then if s2 = Pos then [true]
        else [false]
    else if s2 = Neg then [true, false]
        else [true]
;;

let greater_equal (s1 : sign) (s2 : sign) : bool list =
    List.map (fun x -> not x) less s1 s2
;;

let less_equal (s1 : sign) (s2 : sign) : bool list =
    if s1 = Error || s2 = Error then [false]
    else if s1 = Pos 
        then if s2 = Pos then [true, false]
        else [false]
    else if s1 = Zero 
        then if s2 = Pos || s2 = Zero then [true]
        else [false]
    else if s2 = Neg then [true, false]
        else [true]
;;

let greater (s1 : sign) (s2 : sign) : bool list =
    List.map (fun x -> not x) less_equal s1 s2
;;

let equal (s1 : sign) (s2 : sign) : bool list = 
    if s1 = Error || s2 = Error then [false]
    else if s1 = Pos then if s2 = Pos
        then [true, false]
        else [false]
    else if s1 = Zero then if s2 = Zero
        then [true]
        else [false]
    else if s2 = Neg
        then [true, false]
        else [false]
;;

let not_equal (s1 : sign) (s2 : sign) : bool list =
    List.map (fun x -> not x) equal s1 s2
;;

