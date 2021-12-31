open Type;;

let inverse_sign (s : sign) : sign =
    match s with
    | Pos -> Neg
    | Neg -> Pos
    | ss -> ss
;;

let inverse_sign_list (l : sign list) : sign list = 
    List.map inverse_sign l
;;

let rec union l1 l2 =
    match l1 with
    | [] -> l2
    | x :: ll -> 
        if List.mem x l2 then union ll l2
        else union ll (x :: l2)
;;

let rec inter l1 l2 =
    match l1 with
    | [] -> []
    | x :: ll -> 
        if List.mem x l2 then x :: (inter ll l2)
        else inter ll l2
;;

let rec aux_compute f s1 l2 =
    match l2 with
    | [] -> []
    | s2 :: ll2 -> union (f s1 s2) (aux_compute f s1 ll2)
;;

let rec compute f l1 l2 =
    match l1 with 
    | [] -> []
    | s1 :: ll1 -> union (aux_compute f s1 l2) (compute f ll1 l2)
;;


let add (s1 : sign) (s2 : sign) : sign list =
    match (s1, s2) with 
    | (Pos, Pos) -> [Pos]
    | (Neg, Neg) -> [Neg]
    | (ss1, ss2) when ss1 = Error || ss2 = Error -> [Error]
    | (ss1, ss2) when ss1 = Zero  -> [ss2]
    | (ss1, ss2) when ss2 = Zero -> [ss1]
    | (_,_) -> [Pos; Zero; Neg]
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
    | (Pos, Pos) -> [Pos; Zero]
    | (Neg, Neg) -> [Pos; Zero]
    | (Pos, Neg) -> [Neg; Zero]
    | (Neg, Pos) -> [Neg; Zero]
    | (Zero,ss2) when ss2 <> Zero -> [Zero]
    | (_,_) -> [Error]
;;

let modulo (s1 : sign) (s2 : sign) : sign list =
    if(s2 = Zero) then [Error]
    else if(s1 = Error || s2 = Error) then [Error]
    else [s1; Zero]
;;

let less (s1 : sign) (s2 : sign) : bool list =
    if s1 = Error || s2 = Error then [false]
    else if s1 = Pos 
        then if s2 = Pos then [true; false]
        else [false]
    else if s1 = Zero 
        then if s2 = Pos then [true]
        else [false]
    else if s2 = Neg then [true; false]
        else [true]
;;

let greater_equal (s1 : sign) (s2 : sign) : bool list =
    List.map (fun x -> not x) (less s1 s2)
;;

let less_equal (s1 : sign) (s2 : sign) : bool list =
    if s1 = Error || s2 = Error then [false]
    else if s1 = Pos 
        then if s2 = Pos then [true; false]
        else [false]
    else if s1 = Zero 
        then if s2 = Pos || s2 = Zero then [true]
        else [false]
    else if s2 = Neg then [true; false]
        else [true]
;;

let greater (s1 : sign) (s2 : sign) : bool list =
    List.map (fun x -> not x) (less_equal s1 s2)
;;

let equal (s1 : sign) (s2 : sign) : bool list = 
    if s1 = Error || s2 = Error then [false]
    else if s1 = Pos then if s2 = Pos
        then [true; false]
        else [false]
    else if s1 = Zero then if s2 = Zero
        then [true]
        else [false]
    else if s2 = Neg
        then [true; false]
        else [false]
;;

let not_equal (s1 : sign) (s2 : sign) : bool list =
    List.map (fun x -> not x) (equal s1 s2)
;;

let op_sign (ope : op) = 
    match ope with
    | Add -> compute add
    | Sub -> compute sub
    | Mul -> compute mul
    | Div -> compute div
    | Mod -> compute modulo
;;

let rec sign_expr (e : expr) (env : (sign list) Env.t) : sign list =
    match e with 
    | Num(n) -> if n > 0 then [Pos] else if n = 0 then [Zero] else [Neg]
    | Var(x) -> Env.find x env (* Peut y avoir une erreur ! *) 
    | Op(ope, e1, e2) -> (op_sign ope) (sign_expr e1 env) (sign_expr e2 env)
;;

let comp_possible (c : comp) = 
    match c with 
    | Eq -> compute equal
    | Ne -> compute not_equal
    | Lt -> compute less
    | Le -> compute less_equal
    | Gt -> compute greater
    | Ge -> compute greater_equal
 
let possible_cond (c : cond) (env : (sign list) Env.t) : bool list =
    match c with 
    | (e1, com, e2) -> (comp_possible com) (sign_expr e1 env) (sign_expr e2 env)
;;

let sign_read (ins : instr) (env : (sign list) Env.t) : (sign list) Env.t =
    match ins with
    | Read(name) -> 
        if not(Env.mem name env) then let env = Env.add name [Pos; Zero; Neg] env in
           env
        else
           let env = Env.remove name env in
           let env = Env.add name [Pos; Zero; Neg] env in
           env
    | _ -> failwith "Error sign not read"
;;

let sign_set (ins : instr) (env : (sign list) Env.t) : (sign list) Env.t =
    match ins with
    | Set(name, e) -> 
        if not(Env.mem name env) then let env = Env.add name (sign_expr e env) env in
           env
        else
           let env = Env.remove name env in
           let env = Env.add name (sign_expr e env) env in
           env
    | _ -> failwith "Error sign not set"
;;

let propa_comp_aux (co : comp) (s1 : sign) (s2 : sign) : sign list =
    match co with 
    | Eq -> if(List.mem true (equal s1 s2)) then [s1] else []
    | Ne -> if(List.mem true (not_equal s1 s2)) then [s1] else []
    | Gt -> if(List.mem true (greater s1 s2)) then [s1] else []
    | Ge -> if(List.mem true (greater_equal s1 s2)) then [s1] else []
    | Lt -> if(List.mem true (less s1 s2)) then [s1] else []
    | Le -> if(List.mem true (less_equal s1 s2)) then [s1] else []
;;

let propa_comp (s1 : sign list) (s2 : sign list) (co : comp) : sign list =
    compute (propa_comp_aux co) s1 s2
;;


let reverse_comp (c : comp) : comp =
    match c with
    | Eq -> Eq
    | Ne -> Ne
    | Gt -> Lt
    | Lt -> Gt
    | Le -> Ge
    | Ge -> Le
;;

let reverse_cond (co : cond) : cond =
    match co with
    | (e1, com, e2) -> (e2, reverse_comp com, e1)
;;

let inverse_comp (c : comp) : comp =
     match c with
    | Eq -> Ne
    | Ne -> Eq
    | Gt -> Le
    | Lt -> Ge
    | Le -> Gt
    | Ge -> Lt
;;

let inverse_cond (co : cond) : cond =
    match co with
    | (e1, com, e2) -> (e1, inverse_comp com, e2)
;;

let propa_sign_aux (x : name) (co : comp) (e2 : expr) (env : (sign list) Env.t) : (sign list) Env.t =
    let s1 = Env.find x env in
    let s2 = sign_expr e2 env in
    let possible = propa_comp s1 s2 co in
    let env = Env.remove x env in
    let env = Env.add x possible env in env


let rec propa_sign (co : cond) (env : (sign list) Env.t) : (sign list) Env.t =
    match co with
    | (Var(x), com, Var(y)) -> 
        let env = propa_sign_aux x com (Var(y)) env in
        let env = propa_sign_aux y (reverse_comp com) (Var(x)) env in env
    | (Var(x), com, e2) -> propa_sign_aux x com e2 env
    | (e1, com, Var(y)) -> propa_sign (reverse_cond co) env
    | _ -> env