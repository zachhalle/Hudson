(* Syntax *)

type lab = string
type var = string

type 'a row = (lab * 'a) list

open InnerPrim

type kind =
  | BaseK
  | ArrK of kind * kind
  | ProdK of kind row

(* TODO: change this to use hash consing *)
type typ = (* de Bruijn representation *)
  | VarT of int
  | PrimT of Prim.typ
  | NotT of typ
  | ProdT of typ row
  | AnyT of kind * typ (* binds *)
  | AppT of typ * typ
  | LamT of kind * typ (* binds *)
  | TupT of typ row
  | DotT of typ * lab
  | RecT of kind * typ (* binds *)

type exp =
  | PrimE of var * InnerPrim.Prim.const * value list * exp
  | IfE of value * exp * exp
  | AppE of value * value
  | DotE of var * value * lab * exp
  | OpenE of value * var * exp (* binds *)
  | LetE of value * var * exp

and value =
  | VarV of var
  | LamV of var * typ * exp
  | TupV of value row
  | PackV of typ * value * typ
  | RollV of value * typ
  | UnrollV of value
  | RecV of var * typ * value

exception Error of string
exception Unimplemented

(* Helpers *)

let lab i = "_" ^ string_of_int i
let tup_row xs = List.mapi (fun i x -> lab (i + 1), x) xs
let map_row f r = List.map (fun (l, v) -> l, f v) r
let iter_row f r = List.iter (fun (l, v) -> f v) r
let lookup_lab l row =
  try List.assoc l row with Not_found -> raise (Error ("label " ^ l))

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
  | NotT t ->
    NotT (subst_typ_main m s n l t)
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
  | PrimE (x, p, vs, e) ->
    PrimE (x, p, List.map (subst_val_main m s n l) vs, subst_exp_main m s n l e)
  | IfE (v, e1, e2) ->
    IfE (subst_val_main m s n l v, subst_exp_main m s n l e1, subst_exp_main m s n l e2)
  | AppE (v1, v2) ->
    AppE (subst_val_main m s n l v1, subst_val_main m s n l v2)
  | DotE (x, v, l', e) ->
    DotE (x, subst_val_main m s n l v, l', subst_exp_main m s n l e)
  | OpenE (v, x, e) ->
    OpenE (subst_val_main m s n l v, x, subst_exp_main (m + 1) s n l e)
  | LetE (v, x, e) ->
    LetE (subst_val_main m s n l v, x, subst_exp_main m s n l e)

and subst_val_main m s n l v =
  match v with
  | VarV _ -> v
  | LamV (x, t, e) -> 
    LamV (x, subst_typ_main m s n l t, subst_exp_main m s n l e)
  | TupV vs -> 
    TupV (map_row (subst_val_main m s n l) vs)
  | PackV (t1, v, t2) ->
    PackV (subst_typ_main m s n l t1, subst_val_main m s n l v, subst_typ_main m s n l t2)
  | RollV (v, t) -> 
    RollV (subst_val_main m s n l v, subst_typ_main m s n l t)
  | UnrollV v -> 
    UnrollV (subst_val_main m s n l v)
  | RecV (x, t, v) ->
    RecV (x, subst_typ_main m s n l t, subst_val_main m s n l v)

let lift_typ l t = if l = 0 then t else subst_typ_main 0 [] 0 l t
let lift_exp l e = if l = 0 then e else subst_exp_main 0 [] 0 l e
let lift_val l v = if l = 0 then v else subst_val_main 0 [] 0 l v

let subst_typ s t = subst_typ_main 0 [s] 1 0 t
let subst_exp s e = subst_exp_main 0 [s] 1 0 e
let subst_val s v = subst_val_main 0 [s] 1 0 v

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

let rec norm_typ t =
  match t with
  | VarT _ -> t
  | PrimT _ -> t
  | NotT t -> NotT (norm_typ t)
  | ProdT tr -> ProdT (map_row norm_typ tr)
  | AnyT (k, t) -> AnyT (k, norm_typ t)
  | RecT (k, t) -> RecT (k, norm_typ t)
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

let whnf_typ t =
  match t with
  | AppT (t1, t2) -> 
    begin match norm_typ t1, norm_typ t2 with
    | LamT (k, t), t2' -> norm_typ (subst_typ t2' t)
    | t1', t2' -> AppT (t1', t2')
    end
  | DotT (t, l) ->
    begin match norm_typ t with
    | TupT ts -> norm_typ (lookup_lab l ts)
    | t' -> DotT (t', l)
    end
  | _ -> t

let equal_row equal r1 r2 =
  List.for_all2 (fun (l1, z1) (l2, z2) -> l1 = l2 && equal z1 z2) r1 r2

let rec equal_typ t1 t2 =
  match whnf_typ t1, whnf_typ t2 with
  | VarT i, VarT j -> i = j
  | PrimT t1, PrimT t2 -> t1 = t2
  | NotT t1, NotT t2 -> equal_typ t1 t2
  | ProdT tr1, ProdT tr2 -> equal_row equal_typ tr1 tr2
  | AnyT (k1, t1), AnyT (k2, t2) -> k1 = k2 && equal_typ t1 t2
  | AppT (t11, t12), AppT (t21, t22) -> equal_typ t11 t21 && equal_typ t12 t22
  | LamT (k1, t1), LamT (k2, t2) -> k1 = k2 && equal_typ t1 t2
  | TupT tr1, TupT tr2 -> equal_row equal_typ tr1 tr2
  | DotT (t1, l1), DotT (t2, l2) -> l1 = l2 && equal_typ t1 t2
  | RecT (k1, t1), RecT (k2, t2) -> k1 = k2 && equal_typ t1 t2
  | _ -> false

(* Checking *)

let rec infer_typ env typ =
  match typ with
  | VarT i -> lookup_typ i env
  | PrimT (Prim.VarT i) -> check_typ env (VarT i) BaseK "PrimT"; BaseK 
  | PrimT t -> BaseK
  | NotT t -> check_typ env t BaseK "NotT"; BaseK
  | ProdT tr -> iter_row (fun t -> check_typ env t BaseK "ProdTi") tr; BaseK
  | AnyT (k, t) -> check_typ (add_typ k env) t BaseK "AnyT"; BaseK
  | LamT (k, t) -> ArrK (k, infer_typ (add_typ k env) t)
  | AppT (t1, t2) ->
    begin match infer_typ env t1 with
    | ArrK (k2, k) -> check_typ env t2 k2 "AppT2"; k
    | _ -> raise (Error "AppT1")
    end
  | TupT tr -> ProdK (map_row (infer_typ env) tr)
  | DotT (t, l) ->
    begin match infer_typ env t with
    | ProdK tr -> lookup_lab l tr
    | _ -> raise (Error "DotT")
    end
  | RecT (k, t) -> check_typ (add_typ k env) t k "RecT"; k

and check_typ env typ kind s =
  if infer_typ env typ <> kind then
    raise (Error s)

let rec infer_val env value = raise Unimplemented
and check_val env value typ s =
  if not (equal_typ (infer_val env value) typ) then
    raise (Error s)

and check_exp env value typ s = raise Unimplemented

(* String conversion *)

let verbose_exp_flag = ref true
let verbose_typ_flag = ref true

let string_of_kind = raise Unimplemented
let string_of_typ = raise Unimplemented
let string_of_exp = raise Unimplemented
