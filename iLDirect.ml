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

let lift_typ l t = if l == 0 then t else subst_typ_main 0 [] 0 l t
let lift_exp l e = if l == 0 then e else subst_exp_main 0 [] 0 l e

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

let norm_typ = raise Unimplemented
let norm_exp = raise Unimplemented

let equal_typ = raise Unimplemented

let force_typ = raise Unimplemented

(* Checking *)

let infer_typ = raise Unimplemented
let infer_exp = raise Unimplemented

let check_typ = raise Unimplemented
let check_exp = raise Unimplemented

(* Unrolling *)

let unroll_typ = raise Unimplemented

(* String conversion *)

let verbose_exp_flag = raise Unimplemented
let verbose_typ_flag = raise Unimplemented

let string_of_kind = raise Unimplemented
let string_of_typ = raise Unimplemented
let string_of_exp = raise Unimplemented