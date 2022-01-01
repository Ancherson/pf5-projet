open Type;;


let string_of_bool b = 
  if b then "true" else "false"
;;

let print_bool b = print_string(string_of_bool b);;
let rec print_list_bool l =
  match l with
  | [] -> print_newline()
  | x :: ll -> print_bool x; print_string(" "); print_list_bool ll;;
;;

let print_sign (s : sign) : unit =
    match s with
    | Pos -> print_string("+")
    | Neg -> print_string("-")
    | Zero -> print_string("0")
    | Error -> print_string("!")
;;


let rec print_list f l : unit =
    match l with
    | [] -> print_newline()
    | x :: ll -> (f x);print_list f ll
;;

let print_list_sign l = print_list (print_sign) l
;;

let print_elt_map_sign (key : name) (v : sign list) = 
    print_string (key ^ " ");print_list_sign v;;

let print_map_sign (env : (sign list) Env.t) = Env.iter print_elt_map_sign env;;

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

let rec aux_compute f same s1 l2 =
    match l2 with
    | [] -> []
    | s2 :: ll2 -> 
        union ((fun s1 s2 -> if ((not same) || (same && s1 = s2)) then f s1 s2 else []) s1 s2) (aux_compute f same s1 ll2)
;;

let rec compute f same l1 l2 =
    match l1 with 
    | [] -> []
    | s1 :: ll1 -> union (aux_compute f same s1 l2) (compute f same ll1 l2)
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
    if s1 = Error || s2 = Error then [true; false]
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
    if s1 = Error || s2 = Error then [true; false]
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
    if s1 = Error || s2 = Error then [true; false]
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

let op_sign (ope : op) (same : bool) = 
    match ope with
    | Add -> compute add same
    | Sub -> compute sub same
    | Mul -> compute mul same
    | Div -> compute div same
    | Mod -> compute modulo same
;;


let rec sign_expr (e : expr) (env : (sign list) Env.t) : sign list =
    match e with 
    | Num(n) -> if n > 0 then [Pos] else if n = 0 then [Zero] else [Neg]
    | Var(x) -> Env.find x env (* Peut y avoir une erreur ! *) 
    | Op(ope, e1, e2) -> (op_sign ope (e1 = e2)) (sign_expr e1 env) (sign_expr e2 env)
;;

let if_error_cond (co : cond) (env : (sign list) Env.t) : bool =
    match co with
    | (e1, com, e2) -> (List.mem Error (sign_expr e1 env) || List.mem Error (sign_expr e2 env))
;;


let comp_possible (c : comp) (same : bool) = 
    match c with 
    | Eq -> if same then (fun s1 s2 -> [true]) else compute equal same
    | Ne -> if same then (fun s1 s2 -> [false]) else compute not_equal same
    | Lt -> if same then (fun s1 s2 -> [false]) else compute less same
    | Le -> if same then (fun s1 s2 -> [true]) else compute less_equal same
    | Gt -> if same then (fun s1 s2 -> [false]) else compute greater same
    | Ge -> if same then (fun s1 s2 -> [true]) else compute greater_equal same
 
let possible_cond (c : cond) (env : (sign list) Env.t) : bool list =
    match c with 
    | (e1, com, e2) -> (comp_possible com (e1 = e2)) (sign_expr e1 env) (sign_expr e2 env)
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

let propa_comp (s1 : sign list) (s2 : sign list) (co : comp) (same : bool) : sign list =
    compute (propa_comp_aux co) same s1 s2
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
    let possible = propa_comp s1 s2 co (Var(x) = e2) in
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


let join_aux (key : name) (v1 : sign list) (v2 : sign list) : (sign list) option =
    Some(union v1 v2)
;;

let map_join (env1 : (sign list) Env.t) (env2 : (sign list) Env.t) : (sign list) Env.t =
    Env.union join_aux env1 env2
;;

let sign_read (ins : instr) (env : (sign list) Env.t) (line : int) : (sign list) Env.t =
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

let sign_set (ins : instr) (env : (sign list) Env.t) (line : int) : ((sign list) Env.t) * string =
    match ins with
    | Set(name, e) -> 
        let s = sign_expr e env in
        if not(Env.mem name env) then let env = Env.add name s env in
           (env, if List.mem Error s then "divbyzero " ^ (string_of_int line) else "safe")
        else
           let env = Env.remove name env in
           let env = Env.add name s env in
           (env, if List.mem Error s then "divbyzero " ^ (string_of_int line) else "safe")
    | _ -> failwith "Error sign not set"
;;


let sign_print (ins : instr) (env : (sign list) Env.t) (line : int) : ((sign list) Env.t) * string =
    match ins with
    |Print(e) -> if List.mem Error (sign_expr e env) then (env, "divbyzero " ^ (string_of_int line)) else (env, "safe")
    | _ -> failwith "Error sign_print not print"

let rec sign_block (bloc : block) (env : (sign list) Env.t) : ((sign list) Env.t) * string = 
    match bloc with
    | [] -> (env, "safe")
    | (num,inst)::tail ->
        let (env, st1) = (sign_instr inst env num) in
        let (env_fin, st2) = sign_block tail env in
        (env_fin, if st1 = "safe" then st2 else st1)

and sign_instr (ins : instr) (env : (sign list) Env.t) (line : int): ((sign list) Env.t) * string =
    match ins with
    | Set(_,_) -> sign_set ins env line
    | Read(_) -> (sign_read ins env line, "safe")
    | Print(_) -> sign_print ins env line
    | If(_,_,_) -> sign_if ins env line
    | _ -> failwith "TODO"
    (*| While(_,_) -> *)

and sign_if (ins : instr) (env : (sign list) Env.t) (line : int) : ((sign list) Env.t) * string =
    match ins with
    | If(con, b_if, b_else) ->
        let is_error = if_error_cond con env in
        let res = if is_error then "divbyzero " ^ (string_of_int line) else "safe" in
        let is_possible = possible_cond con env in
        if List.mem true is_possible then
            let env1 = propa_sign con env in
            let (env1, res1) = sign_block b_if env1 in
            if List.mem false is_possible  then
                let env2 = propa_sign (inverse_cond con) env in
                let (env2, res2) = sign_block b_else env2 in (map_join env1 env2, if res <> "safe" then res else if res1 <> "safe" then res2 else res1)
            else (env1, if res <> "safe" then res else res1)
        else if List.mem false is_possible  then
            let env = propa_sign (inverse_cond con) env in
            let (env, res1) = sign_block b_else env in (env, if res <> "safe" then res else res1)
        else (env, res)
    | _ -> failwith "Error sign if, not a if !" 
;;