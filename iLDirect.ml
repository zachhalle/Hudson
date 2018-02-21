(* Syntax *)

type lab = string
type var = string

type 'a row = (lab * 'a) list

type kind =
  | BaseK
  | ArrK of kind * kind
  | ProdK of kind row

open InnerPrim

(* TODO: change this to use hash consing *)
type typ = (* de Bruijn representation *)
  | VarT of int
  | PrimT of Prim.typ
  | ArrT of typ * typ
  | ProdT of typ row
  | AllT of kind * typ (* binds *)
  | AnyT of kind * typ (* binds *)
  | AppT of typ * typ
  | LamT of kind * typ (* binds *)
  | TupT of typ row
  | DotT of typ * lab
  | RecT of kind * typ (* binds *)

type exp =
  | VarE of var
  | PrimE of Prim.const
  | IfE of exp * exp * exp
  | LamE of var * typ * exp
  | AppE of exp * exp
  | TupE of exp row
  | DotE of exp * lab
  | GenE of kind * exp (* binds *)
  | InstE of exp * typ
  | PackE of typ * exp * typ
  | OpenE of exp * var * exp (* binds *)
  | RollE of exp * typ
  | UnrollE of exp
  | RecE of var * typ * exp
  | LetE of exp * var * exp

exception Error of string
exception TypeCheck of exp * typ * typ
exception Unimplemented

(* Helpers *)

let lab i = "_" ^ string_of_int i
let tup_row xs = List.mapi (fun i x -> lab (i + 1), x) xs
let map_row f r = List.map (fun (l, v) -> l, f v) r
let iter_row f r = List.iter (fun (l, v) -> f v) r
let lookup_lab l row =
  try List.assoc l row with Not_found -> raise (Error ("label " ^ l))

(* String conversion *)

let verbose_exp_flag = ref true
let verbose_typ_flag = ref true

let string_of_row sep string_of r =
    String.concat ", " (List.map (fun (l, z) -> l ^ sep ^ string_of z) r)

let rec string_of_kind = function
  | BaseK -> "*"
  | ArrK (k1, k2) -> "(" ^ string_of_kind k1 ^ "->" ^ string_of_kind k2 ^ ")"
  | ProdK [] -> "1"
  | ProdK ks -> "{" ^ string_of_row ":" string_of_kind ks ^ "}"

let rec string_of_typ = function
  | VarT(a) -> "(tvar " ^ string_of_int a ^ ")"
  | PrimT(t) -> Prim.string_of_typ t
  | ArrT(t1, t2) -> "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
  | ProdT [] -> "1"
  | ProdT ts -> "x{" ^ string_of_row " : " string_of_typ ts ^ "}"
  | AllT (k, t) ->
    "(" ^ "!" ^ string_of_kind k ^ ". " ^ string_of_typ t ^ ")"
  | AnyT (k, t) ->
    "(" ^ "?" ^ string_of_kind k ^ ". " ^ string_of_typ t ^ ")"
  | LamT(k, t) ->
    "(" ^ "\\" ^ string_of_kind k ^ ". " ^ string_of_typ t ^ ")"
  | AppT (t1, t2) -> string_of_typ t1 ^ "(" ^ string_of_typ t2 ^ ")"
  | TupT ts -> "{" ^ string_of_row " = " string_of_typ ts ^ "}"
  | DotT(t, l) -> string_of_typ t ^ "." ^ l
  | RecT(k, t) ->
    "(" ^ "@" ^ string_of_kind k ^ ". " ^ string_of_typ t ^ ")"

let rec string_of_exp = function
  | VarE x -> "(var" ^ x ^ ")"
  | PrimE c -> "(prim" ^ Prim.string_of_const c ^ ")"
  | IfE (e1, e2, e3) ->
    "(if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2 ^
      " else " ^ string_of_exp e3 ^ ")"
  | LamE (x, t, e) ->
    "(\\" ^ x ^
    (if !verbose_typ_flag then ":" ^ string_of_typ t else "") ^
    ". " ^
    (if !verbose_exp_flag then string_of_exp e else "_") ^
    ")"
  | AppE (e1, e2) -> "(" ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ ")"
  | TupE es -> "{" ^ string_of_row " = " string_of_exp es ^ "}"
  | DotE (e, l) -> string_of_exp e ^ "." ^ l
  | GenE (k, e) ->
    "(!" ^ string_of_kind k ^ ". " ^ string_of_exp e ^ ")"
  | InstE (e, t) -> "(" ^ string_of_exp e ^ " [" ^ string_of_typ t ^ "])"
  | PackE (t1, e, t2) ->
    "pack(" ^ string_of_typ t1 ^ ", " ^ string_of_exp e ^ ")" ^
    (if !verbose_typ_flag then ":" ^ string_of_typ t2 else "")
  | OpenE (e1, x, e2) ->
    "(unpack(" ^ x ^ ") = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2 ^ ")"
  | RollE (e, t) ->
    "roll(" ^ string_of_exp e ^ ")" ^
    (if !verbose_typ_flag then ":" ^ string_of_typ t else "")
  | UnrollE(e) -> "unroll(" ^ string_of_exp e ^ ")"
  | RecE(x, t, e) ->
    "(rec " ^ x ^
    (if !verbose_typ_flag then ":" ^ string_of_typ t else "") ^
    ". " ^
    (if !verbose_exp_flag then string_of_exp e else "_") ^
    ")"
  | LetE(e1, x, e2) ->
    "(let " ^ x ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2 ^ ")"

(* Substitutions *)

let rec subst_typ_main m s n l t =
  match t with
  | VarT i ->
    if i < m then
      t
    else if i < m + n then
      subst_typ_main 0 [] 0 m (List.nth s (i - m))
    else
      VarT (i - n + l)
  | LamT (k, t) -> 
    LamT (k, subst_typ_main (m + 1) s n l t)
  | AppT (t1, t2) -> 
    AppT (subst_typ_main m s n l t1, subst_typ_main m s n l t2)
  | ProdT ts ->
    ProdT (map_row (subst_typ_main m s n l) ts)
  | DotT (t, i) ->
    DotT (subst_typ_main m s n l t, i)
  | ArrT (t1, t2) ->
    ArrT (subst_typ_main m s n l t1, subst_typ_main m s n l t2)
  | AllT (k, t) ->
    AllT (k, subst_typ_main (m + 1) s n l t)
  | AnyT (k, t) ->
    AnyT (k, subst_typ_main (m + 1) s n l t)
  | TupT ts ->
    TupT (map_row (subst_typ_main m s n l) ts)
  | RecT (k, t) ->
    RecT (k, subst_typ_main (m + 1) s n l t)
  | PrimT (Prim.VarT i) ->
    if i < m then
      t
    else if i < m + n then
      subst_typ_main 0 [] 0 m (List.nth s (i - m))
    else
      PrimT (Prim.VarT (i - n + l))
  | PrimT _ -> t

let rec subst_exp_main m s n l e =
  match e with
  | VarE _ -> e
  | IfE (e1, e2, e3) ->
    IfE (subst_exp_main m s n l e1, subst_exp_main m s n l e2, subst_exp_main m s n l e3)
  | LamE (x, t, e) ->
    LamE (x, subst_typ_main m s n l t, subst_exp_main m s n l e)
  | AppE (e1, e2) ->
    AppE (subst_exp_main m s n l e1, subst_exp_main m s n l e2)
  | TupE es ->
    TupE (map_row (subst_exp_main m s n l) es)
  | DotE (e, i) ->
    DotE (subst_exp_main m s n l e, i)
  | GenE (k, e) ->
    GenE (k, subst_exp_main (m + 1) s n l e)
  | InstE (e, t) ->
    InstE (subst_exp_main m s n l e, subst_typ_main m s n l t)
  | PackE (t1, e, t2) ->
    PackE (subst_typ_main m s n l t1, subst_exp_main m s n l e, subst_typ_main m s n l t2)
  | OpenE (e1, v, e2) ->
    OpenE (subst_exp_main m s n l e1, v, subst_exp_main (m + 1) s n l e2)
  | RollE (e, t) ->
    RollE (subst_exp_main m s n l e, subst_typ_main m s n l t)
  | UnrollE e ->
    UnrollE (subst_exp_main m s n l e)
  | RecE (v, t, e) ->
    RecE (v, subst_typ_main m s n l t, subst_exp_main m s n l e)
  | LetE (e1, v, e2) ->
    LetE (subst_exp_main m s n l e1, v, subst_exp_main m s n l e2)
  | PrimE _ -> e

let lift_typ l t = if l = 0 then t else subst_typ_main 0 [] 0 l t
let lift_exp l e = if l = 0 then e else subst_exp_main 0 [] 0 l e

let subst_typ s t = subst_typ_main 0 [s] 1 0 t
let subst_exp s e = subst_exp_main 0 [s] 1 0 e

(* Environments *)

module VarMap = Map.Make(String)

type env = 
  { ksize : int ; 
    kenv : kind list ; 
    tenv : (int * typ) VarMap.t }

let empty = { ksize = 0 ; kenv = [] ; tenv = VarMap.empty }

let add_typ k { ksize ; kenv ; tenv } =
  { ksize = ksize + 1 ;
    kenv = k :: kenv ;
    tenv = tenv }

let add_val v t { ksize ; kenv ; tenv } =
  { ksize = ksize ;
    kenv = kenv ;
    tenv = VarMap.add v (ksize, t) tenv }

let lookup_typ i { ksize ; kenv ; tenv } =
  try List.nth kenv i with
  | Failure _ -> raise (Error "Undefined type variable")

let lookup_val v { ksize ; kenv ; tenv } =
  let n, c = 
    try VarMap.find v tenv with
    | Not_found -> raise (Error "Undefined variable")
  in
  lift_typ (ksize - n) c

(* Normalisation and Equality *)

let varT x = raise Unimplemented

let rec norm_typ t =
  match t with
  | VarT _ -> t
  | PrimT _ -> t
  | ArrT (t1, t2) -> ArrT (norm_typ t1, norm_typ t2)
  | ProdT ts -> ProdT (map_row norm_typ ts)
  | AllT (k, t) -> AllT (k, norm_typ t)
  | AnyT (k, t) -> AnyT (k, norm_typ t)
  | LamT (k, t) -> LamT (k, norm_typ t)
  | AppT (t1, t2) -> 
    begin match norm_typ t1, norm_typ t2 with
    | LamT (k, t), t2' -> norm_typ (subst_typ t2' t)
    | t1', t2' -> AppT (t1', t2')
    end
  | TupT ts -> TupT (map_row norm_typ ts)
  | DotT (t, l) ->
    begin match norm_typ t with
    | TupT ts -> norm_typ (lookup_lab l ts)
    | t' -> DotT (t', l)
    end
  | RecT (k, t) -> RecT (k, norm_typ t)


let rec whnf_typ typ =
  match typ with
  | AppT (t1, t2) ->
    begin match whnf_typ t1 with
    | LamT (_, t1') -> whnf_typ (subst_typ t2 t1')
    | t1' -> AppT (t1', t2)
    end
  | DotT (t, l) ->
    begin match whnf_typ t with
    | TupT ts -> whnf_typ (lookup_lab l ts)
    | t' -> DotT (t', l)
    end
  | _ -> typ

let equal_row equal r1 r2 =
  List.for_all2 (fun (l1, z1) (l2, z2) -> l1 = l2 && equal z1 z2) r1 r2

(* TODO: A better implementation would use weak-head reduction *)
let equal_typ t1 t2 = norm_typ t1 = norm_typ t2

let equal_typ_exn t1 t2 = 
  if not (equal_typ t1 t2) then raise (Error "equal_typ")

(* Checking *)

let rec infer_typ env typ =
  match typ with
  | VarT v -> lookup_typ v env
  | PrimT t -> BaseK
  | ArrT (t1, t2) ->
    check_typ env t1 BaseK "ArrT1"; check_typ env t2 BaseK "ArrT2"; BaseK
  | ProdT ts -> iter_row (fun t -> check_typ env t BaseK "ProdTi") ts; BaseK
  | AllT (k, t) -> check_typ (add_typ k env) t BaseK "AllT"; BaseK
  | AnyT (k, t) -> check_typ (add_typ k env) t BaseK "AnyT"; BaseK
  | LamT (k, t) -> ArrK (k, infer_typ (add_typ k env) t)
  | AppT (t1, t2) ->
    begin match infer_typ env t1 with
    | ArrK (k2, k) -> check_typ env t2 k2 "AppT2"; k
    | _ -> raise (Error "AppT1")
    end
  | TupT ts -> ProdK (map_row (infer_typ env) ts)
  | DotT (t, l) ->
    begin match infer_typ env t with
    | ProdK ts -> lookup_lab l ts
    | _ -> raise (Error "DotT")
    end
  | RecT (k, t) -> check_typ (add_typ k env) t k "RecT"; k

and check_typ env t k s = 
  let inferred = infer_typ env t in
  if inferred <> k then
    raise (Error s)

let whnf_annot env typ =
  check_typ env typ BaseK "whnf_annot";
  whnf_typ typ

module InferPrim = Prim.MakeInfer (
  struct 
    type typExt = typ
    let primT t = PrimT t
    let varT i = VarT i
    let arrT t1 t2 = ArrT (t1, t2)
    let prodT ts = ProdT ts
  end
) 

let rec infer_exp env exp =
  match exp with
  | VarE x -> lookup_val x env
  | PrimE c -> InferPrim.infer_prim c
  | IfE (e1, e2, e3) ->
    check_exp env e1 (PrimT Prim.BoolT) "IfE1";
    let t = infer_exp env e2 in
    check_exp env e3 t "IfE";
    t
  | LamE (x, t, e) -> ArrT (t, infer_exp (add_val x t env) e)
  | AppE (e1, e2) ->
    begin match whnf_typ (infer_exp env e1) with
    | ArrT (t2, t) -> check_exp env e2 t2 "AppE2"; t
    | t -> raise (Error "AppE1") 
    end
  | TupE es -> ProdT (map_row (infer_exp env) es)
  | DotE (e, l) ->
    begin match whnf_typ (infer_exp env e) with
    | ProdT ts -> lookup_lab l ts
    | _ -> raise (Error "DotE1")
    end
  | GenE (k, e) -> AllT (k, infer_exp (add_typ k env) e)
  | InstE (e, t) ->
    begin match whnf_typ (infer_exp env e) with
    | AllT (k, t') -> check_typ env t k "InstE"; subst_typ t t'
    | _ -> raise (Error "InstE")
    end
  | PackE (t, e, t') ->
    check_typ env t' BaseK "PackE";
    begin match whnf_typ t' with
    | AnyT (k, t'') ->
      check_typ env t k "PackE1";
      check_exp env e (subst_typ t t'') "PackE2";
      t'
    | _ -> raise (Error "PackE")
    end
  | OpenE (e1, x, e2) ->
    begin match whnf_typ (infer_exp env e1) with
    | AnyT (k, t1) ->
      let t2 = infer_exp (add_val x t1 (add_typ k env)) e2 in
      let t2' = subst_typ (VarT 0) t2 in
      check_typ env t2' BaseK "OpenE2"; t2'
    | _ -> raise (Error "OpenE")
    end
  | RollE (e, t) -> (* TODO: why does fomega use "unroll_typ" *)
    begin match whnf_typ t with
    | RecT (k, t') -> 
      check_typ env t k "RecT";
      check_exp env e (subst_typ t' t) "RecT"; 
      t
    | _ -> raise (Error "RollE")
    end
  | UnrollE e -> (* TODO: why does fomega use "unroll_typ" *)
    begin match whnf_typ (infer_exp env e) with
    | RecT (k, t') as t -> check_typ env t k "RecT"; subst_typ t t'
    | _ -> raise (Error "UnrollE")
    end
  | RecE (x, t, e) ->
    check_typ env t BaseK "RecE1";
    check_exp (add_val x t env) e t "RecE2";
    t
  | LetE (e1, x, e2) ->
    let t1 = infer_exp env e1 in
    infer_exp (add_val x t1 env) e2

and check_exp env exp typ s =
  let inferred = infer_exp env exp in
  if not (equal_typ (infer_exp env exp) typ) then
    raise (Error s)
