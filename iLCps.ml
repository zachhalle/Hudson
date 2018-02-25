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
  | IfE of value * exp * exp
  | AppE of value * value
  | DotE of value * lab * var * exp
  | OpenE of value * var * exp (* binds *)
  | LetE of value * var * exp
  | PrimE of Prim.const * value list * var * exp

and value =
  | VarV of var
  | LamV of var * typ * exp
  | TupV of value row
  | PackV of typ * value * typ
  | RollV of value * typ
  | UnrollV of value
  | RecV of var * typ * value

exception Error of string

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
  | NotT t -> "not (" ^ string_of_typ t ^ ")"
  | ProdT [] -> "1"
  | ProdT ts -> "x{" ^ string_of_row " : " string_of_typ ts ^ "}"
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
  | IfE (v, e1, e2) ->
    "(if " ^ string_of_val v ^ " then " ^ string_of_exp e1 ^
      " else " ^ string_of_exp e2 ^ ")"
  | AppE (v1, v2) -> "(" ^ string_of_val v1 ^ " " ^ string_of_val v2 ^ ")"
  | DotE (v, l, x, e) ->
    "(proj " ^ x ^ " = " ^ string_of_val v ^ "." ^ l ^ " in " 
      ^ string_of_exp e ^ ")"
  | OpenE (v, x, e) ->
    "(unpack(" ^ x ^ ") = " ^ string_of_val v ^ " in " ^ string_of_exp e ^ ")"
  | LetE (v, x, e) ->
    "(let " ^ x ^ " = " ^ string_of_val v ^ " in " ^ string_of_exp e ^ ")"
  | PrimE (c, vs, x, e) -> 
    "(prim " ^ x ^ " = " ^ Prim.string_of_const c ^ "["
      ^ (String.concat ", " (List.map string_of_val vs))
      ^ "] in " ^ string_of_exp e ^ ")"
and string_of_val =function
  | VarV x -> "(var" ^ x ^ ")"
  | LamV (x, t, e) ->
    "(\\" ^ x ^
    (if !verbose_typ_flag then ":" ^ string_of_typ t else "") ^
    ". " ^
    (if !verbose_exp_flag then string_of_exp e else "_") ^
    ")"
  | TupV vs -> "{" ^ string_of_row " = " string_of_val vs ^ "}"
  | PackV (t1, v, t2) ->
    "pack(" ^ string_of_typ t1 ^ ", " ^ string_of_val v ^ ")" ^
    (if !verbose_typ_flag then ":" ^ string_of_typ t2 else "")
  | RollV (v, t) ->
    "roll(" ^ string_of_val v ^ ")" ^
    (if !verbose_typ_flag then ":" ^ string_of_typ t else "")
  | UnrollV (v) -> "unroll(" ^ string_of_val v ^ ")"
  | RecV (x, t, v) ->
    "(rec " ^ x ^
    (if !verbose_typ_flag then ":" ^ string_of_typ t else "") ^
    ". " ^
    (if !verbose_exp_flag then string_of_val v else "_") ^
    ")"

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
  | IfE (v, e1, e2) ->
    IfE (subst_val_main m s n l v, subst_exp_main m s n l e1, subst_exp_main m s n l e2)
  | AppE (v1, v2) ->
    AppE (subst_val_main m s n l v1, subst_val_main m s n l v2)
  | DotE (v, l', x, e) ->
    DotE (subst_val_main m s n l v, l', x, subst_exp_main m s n l e)
  | OpenE (v, x, e) ->
    OpenE (subst_val_main m s n l v, x, subst_exp_main (m + 1) s n l e)
  | LetE (v, x, e) ->
    LetE (subst_val_main m s n l v, x, subst_exp_main m s n l e)
  | PrimE (c, vs, x, e) ->
    PrimE (c, List.map (subst_val_main m s n l) vs, x, subst_exp_main m s n l e)

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

let prim_row ts = ProdT (List.mapi (fun i t -> lab i, PrimT t) ts)

let rec infer_val env value =
  match value with
  | VarV x -> lookup_val x env
  | LamV (x, t, e) -> check_exp (add_val x t env) e "LamV1"; NotT t
  | TupV vr -> ProdT (map_row (infer_val env) vr)
  | PackV (t, v, t') ->
    check_typ env t' BaseK "PackV1";
    begin match whnf_typ t' with
    | AnyT (k, t'') ->
      check_typ env t k "PackV3";
      check_val env v (subst_typ t t') "PackV4";
      t'
    | _ -> raise (Error "PackV2")
    end
  | RollV (v, t) ->
    begin match whnf_typ t with
    | RecT (k, t') ->
      check_typ env t k "RollV2";
      check_val env v (subst_typ t' t) "RollV3";
      t
    | _ -> raise (Error "RollV1")
    end
  | UnrollV v ->
    begin match whnf_typ (infer_val env v) with
    | RecT (k, t') as t -> check_typ env t k "RecT"; subst_typ t t'
    | _ -> raise (Error "UnrollV1")
    end
  | RecV (x, t, v) -> check_val (add_val x t env) v t "RecV1"; t

and check_val env value typ s =
  if not (equal_typ (infer_val env value) typ) then
    raise (Error s)

and check_exp env exp s =
  match exp with
  | IfE (v, e1, e2) ->
    check_val env v (PrimT Prim.BoolT) "IfE1";
    check_exp env e1 "IfE2"; 
    check_exp env e2 "IfE3"
  | AppE (v1, v2) ->
    begin match whnf_typ (infer_val env v1) with
    | NotT t -> check_val env v2 t "AppE2"
    | _ -> raise (Error "AppE1")
    end
  | DotE (v, l, x, e) ->
    begin match whnf_typ (infer_val env v) with
    | ProdT tr -> check_exp (add_val x (lookup_lab l tr) env) e "DotE2"
    | _ -> raise (Error "DotE1")
    end
  | OpenE (v, x, e) ->
    begin match whnf_typ (infer_val env v) with
    | AnyT (k, t) -> check_exp (add_val x t (add_typ k env)) e "OpenE2"
    | _ -> raise (Error "OpenE1")
    end
  | LetE (v, x, e) -> check_exp (add_val x (infer_val env v) env) e "LetE1"
  | PrimE (c, vs, x, e) ->
    match Prim.typ_of_const c with
    | [t1], [t2] ->
      begin match vs with
      | [v] -> 
        check_val env v (PrimT t1) "PrimE1";
        check_exp (add_val x (PrimT t2) env) e "PrimE2"
      | _ -> raise (Error "PrimE3")
      end
    | [t], ts ->
      begin match vs with
      | [v] -> 
        check_val env v (PrimT t) "PrimE4";
        check_exp (add_val x (prim_row ts) env) e "PrimE5"
      | _ -> raise (Error "PrimE6")
      end
    | ts, [t] ->
      check_val env (TupV (tup_row vs)) (prim_row ts) "PrimE7";
      check_exp (add_val x (PrimT t) env) e "PrimE8"
    | ts1, ts2 ->
      check_val env (TupV (tup_row vs)) (prim_row ts1) "PrimE9";
      check_exp (add_val x (prim_row ts2) env) e "PrimE10"
