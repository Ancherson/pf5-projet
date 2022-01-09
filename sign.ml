open Type;;

(** affiche un sign *)
let print_sign (s : sign) : unit =
    match s with
    | Pos -> print_string("+")
    | Neg -> print_string("-")
    | Zero -> print_string("0")
    | Error -> print_string("!")
;;

(** affiche une list l, où chaque élément de la liste s'affiche avec f *)
let rec print_list f l : unit =
    match l with
    | [] -> print_newline()
    | x :: ll -> (f x);print_list f ll
;;

(* affiche une liste de signe *)
let print_list_sign l = print_list (print_sign) l
;;

(* affiche les éléments d'un environnement de signes *)
let print_elt_map_sign (key : name) (v : sign list) = 
    print_string (key ^ " ");print_list_sign v;;

(* affiche tout l'environnement de signes *)
let print_map_sign (env : (sign list) Env.t) = Env.iter print_elt_map_sign env;;

(* inverse un sign *)
let inverse_sign (s : sign) : sign =
    match s with
    | Pos -> Neg
    | Neg -> Pos
    | ss -> ss
;;

(* inverse une liste de signes *)
let inverse_sign_list (l : sign list) : sign list = 
    List.map inverse_sign l
;;

(* Fait l'union de 2 listes *)
let rec union l1 l2 =
    match l1 with
    | [] -> l2
    | x :: ll -> 
        if List.mem x l2 then union ll l2
        else union ll (x :: l2)
;;
let rec inclu l1 l2 =
match l1 with
|[] -> true
|x::tail -> List.mem x l2 && inclu tail l2 
;;

let equal_list l1 l2 =
inclu l1 l2 && inclu l2 l1
;;

(* fonction auxiliaire de la fonction compute *)
let rec aux_compute f same s1 l2 =
    match l2 with
    | [] -> []
    | s2 :: ll2 -> 
        union ((fun s1 s2 -> if ((not same) || (same && s1 = s2)) then f s1 s2 else []) s1 s2) (aux_compute f same s1 ll2)
;;

(* fonction générale : 
     - prend une fonction qui prend 2 signes et renvoie une liste d'un certain type 
     - same -> indique si compute doit appliquer f sur uniquement les couples (s1, s2) où s1 = s2 
     - deux listes de signes 
     renvoie l'union des f(s1, s2) pour tout (s1, s2) appartenant à (l1 * l2) *)
let rec compute f same l1 l2 =
    match l1 with 
    | [] -> []
    | s1 :: ll1 -> union (aux_compute f same s1 l2) (compute f same ll1 l2)
;;

(* renvoie les signes possibles de s1 + s2 *)
let add (s1 : sign) (s2 : sign) : sign list =
    match (s1, s2) with 
    | (Pos, Pos) -> [Pos]
    | (Neg, Neg) -> [Neg]
    | (ss1, ss2) when ss1 = Error || ss2 = Error -> [Error]
    | (ss1, ss2) when ss1 = Zero  -> [ss2]
    | (ss1, ss2) when ss2 = Zero -> [ss1]
    | (_,_) -> [Pos; Zero; Neg]
;;

(* renvoie les signes possibles de s1 - s2 *)
let sub (s1 : sign) (s2 : sign) : sign list =
    List.rev (add s1 (inverse_sign s2))
;;

(* renvoie les signes possibles de s1 x s2 *)
let mul (s1 : sign) (s2 : sign) : sign list =
    match (s1, s2) with
    | (ss1, ss2) when ss1 = Error || ss2 = Error -> [Error]
    | (Pos, Pos) -> [Pos]
    | (Neg, Neg) -> [Pos]
    | (Pos, Neg) -> [Neg]
    | (Neg, Pos) -> [Neg]
    | (_,_) -> [Zero]
;;

(* renvoie les signes possibles de s1 / s2 *)
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

(* renvoie les signes possibles de s1 mod s2 *)
let modulo (s1 : sign) (s2 : sign) : sign list =
    if(s2 = Zero) then [Error]
    else if(s1 = Error || s2 = Error) then [Error]
    else [s1; Zero]
;;

(* renvoie si s1 < s2 est possible, impossible, les 2 *)
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

(* renvoie si s1 >= s2 est possible, impossible, les 2 *)
let greater_equal (s1 : sign) (s2 : sign) : bool list =
    List.map (fun x -> not x) (less s1 s2)
;;

(* renvoie si s1 <= s2 est possible, impossible, les 2 *)
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

(* renvoie si s1 > s2 est possible, impossible, les 2 *)
let greater (s1 : sign) (s2 : sign) : bool list =
    List.map (fun x -> not x) (less_equal s1 s2)
;;

(* renvoie si s1 = s2 est possible, impossible, les 2 *)
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

(* renvoie si s1 <> s2 est possible, impossible, les 2 *)
let not_equal (s1 : sign) (s2 : sign) : bool list =
    List.map (fun x -> not x) (equal s1 s2)
;;

(* renvoie la fonction qui calculera les signes possibles en fonction de l'opération données *)
let op_sign (ope : op) (same : bool) = 
    match ope with
    | Add -> compute add same
    | Sub -> compute sub same
    | Mul -> compute mul same
    | Div -> compute div same
    | Mod -> compute modulo same
;;

(* renvoie les signes possibles d'une expression *)
let rec sign_expr (e : expr) (env : (sign list) Env.t) : sign list =
    match e with 
    | Num(n) -> if n > 0 then [Pos] else if n = 0 then [Zero] else [Neg]
    | Var(x) -> Env.find x env (* Peut y avoir une erreur ! *) 
    | Op(ope, e1, e2) -> (op_sign ope (e1 = e2)) (sign_expr e1 env) (sign_expr e2 env)
;;

(* indique si un signe error se trouve dans l'une des expressions de la condition *)
let if_error_cond (co : cond) (env : (sign list) Env.t) : bool =
    match co with
    | (e1, com, e2) -> (List.mem Error (sign_expr e1 env) || List.mem Error (sign_expr e2 env))
;;

(* renvoie la fonction qui dira si la condition est possible en fonction du comparateur *)
let comp_possible (c : comp) (same : bool) = 
    match c with 
    | Eq -> if same then (fun s1 s2 -> [true]) else compute equal same
    | Ne -> if same then (fun s1 s2 -> [false]) else compute not_equal same
    | Lt -> if same then (fun s1 s2 -> [false]) else compute less same
    | Le -> if same then (fun s1 s2 -> [true]) else compute less_equal same
    | Gt -> if same then (fun s1 s2 -> [false]) else compute greater same
    | Ge -> if same then (fun s1 s2 -> [true]) else compute greater_equal same
 
(* indique si une condition est possible *)
let possible_cond (c : cond) (env : (sign list) Env.t) : bool list =
    match c with 
    | (e1, com, e2) -> (comp_possible com (e1 = e2)) (sign_expr e1 env) (sign_expr e2 env)
;;

(* fonction auxiliaire de propa_comp *)
let propa_comp_aux (co : comp) (s1 : sign) (s2 : sign) : sign list =
    match co with 
    | Eq -> if(List.mem true (equal s1 s2)) then [s1] else []
    | Ne -> if(List.mem true (not_equal s1 s2)) then [s1] else []
    | Gt -> if(List.mem true (greater s1 s2)) then [s1] else []
    | Ge -> if(List.mem true (greater_equal s1 s2)) then [s1] else []
    | Lt -> if(List.mem true (less s1 s2)) then [s1] else []
    | Le -> if(List.mem true (less_equal s1 s2)) then [s1] else []
;;

(* renvoie les signes possibles dans la liste de s1, suivant la liste de signe s2 et le comparateur co
   si same = true, alors on ne considère que les cas où ss1 = ss2 (où (ss1,ss2) appartient à s1 * s2 ) *)
let propa_comp (s1 : sign list) (s2 : sign list) (co : comp) (same : bool) : sign list =
    compute (propa_comp_aux co) same s1 s2
;;

(* permet de renverser le comparateur
   utile pour reverse_cond *)
let reverse_comp (c : comp) : comp =
    match c with
    | Eq -> Eq
    | Ne -> Ne
    | Gt -> Lt
    | Lt -> Gt
    | Le -> Ge
    | Ge -> Le
;;

(* permet de renverser une condition
   exemple : a < b devient b > a
   utile pour appliquer propa_comp *)
let reverse_cond (co : cond) : cond =
    match co with
    | (e1, com, e2) -> (e2, reverse_comp com, e1)
;;

(* permet d'inverser un comparateur (d'obtenir la négation)
   utile pour inverse_cond *)
let inverse_comp (c : comp) : comp =
     match c with
    | Eq -> Ne
    | Ne -> Eq
    | Gt -> Le
    | Lt -> Ge
    | Le -> Gt
    | Ge -> Lt
;;

(* permet d'inverser une condition
   exemple : a < b devient b >= a *)
let inverse_cond (co : cond) : cond =
    match co with
    | (e1, com, e2) -> (e1, inverse_comp com, e2)
;;

(* fonction auxiliaire pour propa_sign 
   mets à jour les signes possible d'une variable en propageant le signes possibles
   de (Var(x) comp e2) *)
let propa_sign_aux (x : name) (co : comp) (e2 : expr) (env : (sign list) Env.t) : (sign list) Env.t =
    let s1 = Env.find x env in
    let s2 = sign_expr e2 env in
    let possible = propa_comp s1 s2 co (Var(x) = e2) in
    let env = Env.remove x env in
    let env = Env.add x possible env in env

(* Calcule les nouveaux signes possible à partir d'une condition *)
let rec propa_sign (co : cond) (env : (sign list) Env.t) : (sign list) Env.t =
    match co with
    | (Var(x), com, Var(y)) -> 
        let env = propa_sign_aux x com (Var(y)) env in
        let env = propa_sign_aux y (reverse_comp com) (Var(x)) env in env
    | (Var(x), com, e2) -> propa_sign_aux x com e2 env
    | (e1, com, Var(y)) -> propa_sign (reverse_cond co) env
    | _ -> env


(* fonction auxiliaire pour map_join *)
let join_aux (key : name) (v1 : sign list) (v2 : sign list) : (sign list) option =
    Some(union v1 v2)
;;

(* permet de faire l'union de 2 environnements *)
let map_join (env1 : (sign list) Env.t) (env2 : (sign list) Env.t) : (sign list) Env.t =
    Env.union join_aux env1 env2
;;


(* mets à jour l'environnement lorqu'on devant une instruction read
   renvoie ce nouvelle environnement + le message de fin *)
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

(* mets à jour l'environnement lorqu'on devant une instruction set
   renvoie ce nouvelle environnement + le message de fin *)
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

(* mets à jour l'environnement lorqu'on devant une instruction print
   renvoie ce nouvelle environnement + le message de fin *)
let sign_print (ins : instr) (env : (sign list) Env.t) (line : int) : ((sign list) Env.t) * string =
    match ins with
    |Print(e) -> if List.mem Error (sign_expr e env) then (env, "divbyzero " ^ (string_of_int line)) else (env, "safe")
    | _ -> failwith "Error sign_print not print"

(* mets à jour l'environnement lorqu'on devant un block
   renvoie ce nouvelle environnement + le message de fin *)
let rec sign_block (bloc : block) (env : (sign list) Env.t) : ((sign list) Env.t) * string = 
    match bloc with
    | [] -> (env, "safe")
    | (num,inst)::tail ->
        let (env, st1) = (sign_instr inst env num) in
        let (env_fin, st2) = sign_block tail env in
        (env_fin, if st1 = "safe" then st2 else st1)

(* mets à jour l'environnement lorqu'on devant une instruction
   renvoie ce nouvelle environnement + le message de fin *)
and sign_instr (ins : instr) (env : (sign list) Env.t) (line : int): ((sign list) Env.t) * string =
    match ins with
    | Set(_,_) -> sign_set ins env line
    | Read(_) -> (sign_read ins env line, "safe")
    | Print(_) -> sign_print ins env line
    | If(_,_,_) -> sign_if ins env line
    | While(_,_) -> sign_while ins env line

(* mets à jour l'environnements lorqu'on devant une instruction if
   renvoie ce nouvelle environnement + le message de fin *)
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

(* mets à jour l'environnements lorqu'on devant une instruction while
   renvoie ce nouvelle environnement + le message de fin *)
and sign_while (ins : instr) (env : (sign list) Env.t) (line : int) : ((sign list) Env.t) * string =
    match ins with
    | While(con, b) ->
        (* fonction qui boucle jusqu'à ce que env = pre_env *)
        let rec sign_while_aux (con : cond) (pre_env : (sign list) Env.t) (line : int) =
            let is_error = if_error_cond con pre_env in
            let pre_res = if is_error then "divbyzero " ^ (string_of_int line) else "safe" in
            let env = propa_sign con env in
            let env, res = sign_block b env in
            let env = map_join pre_env env in
            if Env.equal equal_list env pre_env then (env, if pre_res <> "safe" then pre_res else res)
            else  let env, ret = sign_while_aux con env line
            in (env, if pre_res <> "safe" then pre_res else if res <> "safe" then res else ret)
        in let env, res = sign_while_aux con env line in
        (propa_sign (inverse_cond con) env, res)
    | _ -> failwith "Error sign while, not a while !"
;;
