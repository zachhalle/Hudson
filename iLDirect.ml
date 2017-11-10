(* Syntax *)

type var = int

type kind =
  | BaseK
  | ArrK of kind * kind
  | ProdK of kind list

type typ =
  | VarT of int
  | PrimT of Prim.typ
  | ArrT of typ * typ
  | ProdT of typ list
  | AllT of kind * typ (* binds *)
  | AnyT of kind * typ (* binds *)
  | AppT of typ * typ
  | LamT of kind * typ (* binds *)
  | TupT of typ list
  | DotT of typ * int
  | RecT of kind * typ (* binds *)

type exp =
  | VarE of var
  | PrimE of Prim.const
  | IfE of exp * exp * exp
  | LamE of var * typ * exp
  | AppE of exp * exp
  | TupE of exp list
  | DotE of exp * int
  | GenE of kind * exp (* binds *)
  | InstE of exp * typ
  | PackE of typ * exp * typ
  | OpenE of exp * var * exp (* binds *)
  | RollE of exp * typ
  | UnrollE of exp
  | RecE of var * typ * exp
  | LetE of exp * var * exp

exception Error of string
exception Unimplemented

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
    ProdT (List.map (subst_typ_main m s n l) ts)
  | DotT (t, i) ->
    DotT (subst_typ_main m s n l t, i)
  | ArrT (t1, t2) ->
    ArrT (subst_typ_main m s n l t1, subst_typ_main m s n l t2)
  | AllT (k, t) ->
    AllT (k, subst_typ_main (m + 1) s n l t)
  | AnyT (k, t) ->
    AnyT (k, subst_typ_main (m + 1) s n l t)
  | TupT ts ->
    TupT (List.map (subst_typ_main m s n l) ts)
  | RecT (k, t) ->
    RecT (k, subst_typ_main (m + 1) s n l t)
  | PrimT t ->
    raise Unimplemented

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
    TupE (List.map (subst_exp_main m s n l) es)
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

module IntKey = struct
  type t = int
  let compare = compare
end

module VarMap = Map.Make(IntKey)

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

let varT = raise Unimplemented

let rec norm_typ t =
  match t with
  | VarT _ -> t
  | PrimT _ -> t
  | ArrT (t1, t2) -> ArrT (norm_typ t1, norm_typ t2)
  | ProdT ts -> ProdT (List.map norm_typ ts)
  | AllT (k, t) -> AllT (k, norm_typ t)
  | AnyT (k, t) -> AnyT (k, norm_typ t)
  | LamT (k, t) -> LamT (k, norm_typ t)
  | AppT (t1, t2) -> 
    begin match norm_typ t1, norm_typ t2 with
    | LamT (k, t), t2' -> norm_typ (subst_typ t2' t)
    | t1', t2' -> AppT (t1', t2')
    end
  | TupT ts -> TupT (List.map norm_typ ts)
  | DotT (t, l) ->
    begin match norm_typ t with
    | TupT ts -> norm_typ (List.nth ts l)
    | t' -> DotT (t', l)
    end
  | RecT (k, t) -> RecT (k, norm_typ t)

let whnf_typ typ = raise Unimplemented

(* TODO: A better implementation would weak-head reduce and compare *)
let rec equal_typ' t1 t2 =
  match t1, t2 with
  | VarT a1, VarT a2 -> a1 = a2
  | PrimT t1, PrimT t2 -> t1 = t2
  | ArrT (t11, t12), ArrT (t21, t22) -> equal_typ' t11 t21 && equal_typ' t12 t22
  | ProdT ts1, ProdT ts2 -> List.for_all2 equal_typ' ts1 ts2
  | AllT (k1, t1), AllT (k2, t2) -> k1 = k2 && equal_typ' t1 t2
  | AnyT (k1, t1), AnyT (k2, t2) -> k1 = k2 && equal_typ' t1 t2
  | LamT (k1, t1), LamT (k2, t2) -> k1 = k2 && equal_typ' t1 t2
  | AppT (t11, t12), ArrT (t21, t22) -> equal_typ' t11 t21 && equal_typ' t12 t22
  | TupT ts1, TupT ts2 -> List.for_all2 equal_typ' ts1 ts2
  | DotT (t1, l1), DotT (t2, l2) -> equal_typ' t1 t2 && l1 = l2
  | RecT (k1, t1), RecT (k2, t2) -> k1 = k2 && equal_typ' t1 t2
  | _ -> false

let equal_typ t1 t2 = equal_typ' (norm_typ t1) (norm_typ t2)
let equal_typ_exn t1 t2 =
  if equal_typ t1 t2 then
    ()
  else
    raise (Error "equal_typ")

(* Checking *)

let rec infer_typ env typ =
  match typ with
  | VarT v -> lookup_typ v env
  | PrimT t -> BaseK
  | ArrT (t1, t2) ->
    check_typ env t1 BaseK "ArrT1"; check_typ env t2 BaseK "ArrT2"; BaseK
  | ProdT ts -> List.iter (fun t -> check_typ env t BaseK "ProdTi") ts; BaseK
  | AllT (k, t) -> check_typ (add_typ k env) t BaseK "AllT"; BaseK
  | AnyT (k, t) -> check_typ (add_typ k env) t BaseK "AnyT"; BaseK
  | LamT (k, t) -> ArrK (k, infer_typ (add_typ k env) t)
  | AppT (t1, t2) ->
    begin match infer_typ env t1 with
    | ArrK (k2, k) -> check_typ env t2 k2 "AppT2"; k
    | _ -> raise (Error "AppT1")
    end
  | TupT ts -> ProdK (List.map (infer_typ env) ts)
  | DotT (t, l) ->
    begin match infer_typ env t with
    | ProdK ts -> List.nth ts l
    | _ -> raise (Error "DotT")
    end
  | RecT (k, t) -> check_typ (add_typ k env) t k "RecT"; k

and check_typ env t k s = if infer_typ env t <> k then raise (Error s)

let whnf_annot env typ =
  check_typ env typ BaseK "whnf_annot";
  whnf_typ env typ

let infer_prim_typ = function
  | Prim.VarT -> VarT 0
  | t -> PrimT t

let infer_prim_typs = function
  | [t] -> infer_prim_typ t
  | ts -> ProdT (List.map infer_prim_typ ts)

let infer_prim_fun {Prim.typ = ts1, ts2} =
  ArrT (infer_prim_typs ts1, infer_prim_typs ts2)

let infer_const = function
  | Prim.FunV f -> infer_prim_fun f
  | c -> PrimT (Prim.typ_of_const c)

let rec inhabitant k =
  match k with
  | BaseK -> ProdT []
  | ArrK (k1, k2) -> LamT (k1, inhabitant k2)
  | ProdK ks -> TupT (List.map inhabitant ks)

let rec infer_exp env exp =
  match exp with
  | VarE x -> lookup_val x env
  | PrimE c -> infer_const c
  | IfE (e1, e2, e3) ->
    check_exp env e1 (PrimT Prim.BoolT) "IfE1";
    let t = infer_exp env e2 in
    check_exp env e3 t "IfE";
    t
  | LamE (x, t, e) -> ArrT (t, infer_exp (add_val x t env) e)
  | AppE (e1, e2) ->
    begin match whnf_typ (infer_exp env e1) with
    | ArrT (t2, t) -> check_exp env e2 t2 "AppE2"; t
    | _ -> raise (Error "AppE1")
    end
  | TupE es -> ProdT (List.map (infer_exp env) es)
  | DotE (e, l) ->
    begin match whnf_typ (infer_exp env e) with
    | ProdT ts -> List.nth ts l
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
  if not (equal_typ (infer_exp env exp) typ) then raise (Error s)

(* Unrolling *)

let unroll_typ = raise Unimplemented

(* String conversion *)

let verbose_exp_flag = raise Unimplemented
let verbose_typ_flag = raise Unimplemented

let string_of_kind = raise Unimplemented
let string_of_typ = raise Unimplemented
let string_of_exp = raise Unimplemented