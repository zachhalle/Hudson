exception Error of string
exception Unimplemented

module F = Fomega
module D = ILDirect

let map_row f r = List.map (fun (l, v) -> l, f v) r

let lookup_lab l row =
  try List.assoc l row with Not_found -> raise (Error ("label " ^ l))

let rec translate_kind k =
  match k with
  | F.BaseK -> D.BaseK
  | F.ArrK (k1, k2) -> D.ArrK (translate_kind k1, translate_kind k2)
  | F.ProdK ks -> D.ProdK (map_row translate_kind ks)

module Env : sig

  type env
  type depth = int

  val empty : env
  val add_typ : F.var -> env -> env
  val lookup_typ : F.var -> env -> depth

end = struct

  module VarMap = Map.Make(String)

  type env =
    { ksize : int ;
      tvar_map : int VarMap.t }

  type depth = int

  let empty = { ksize = 0 ; tvar_map = VarMap.empty }

  let add_typ var { ksize ; tvar_map } =
    { ksize = ksize + 1 ;
      tvar_map = VarMap.add var ksize tvar_map }

  let lookup_typ var { ksize ; tvar_map } =
    try
      ksize - VarMap.find var tvar_map - 1
    with
    | Not_found -> raise (Error "Undefined variable")

end

include Env

let rec force_typ = function
  | F.InferT(t', _) -> force_typ (Lazy.force t')
  | t -> t

let translate_prim_typ env p =
  match p with
  | Prim.BoolT -> InnerPrim.Prim.BoolT
  | Prim.IntT -> InnerPrim.Prim.IntT
  | Prim.CharT -> InnerPrim.Prim.CharT
  | Prim.TextT -> InnerPrim.Prim.TextT
  | Prim.VarT v -> InnerPrim.Prim.VarT (lookup_typ v env)

let rec translate_typ env typ =
  match typ with
  | F.VarT v -> D.VarT (lookup_typ v env)
  | F.PrimT t -> D.PrimT (translate_prim_typ env t)
  | F.ArrT (t1, t2) ->
    D.ArrT (translate_typ env t1, translate_typ env t2)
  | F.ProdT tr -> D.ProdT (map_row (translate_typ env) tr)
  | F.AllT (v, k, t) ->
    D.AllT (translate_kind k, translate_typ (add_typ v env) t)
  | F.AnyT (v, k, t) ->
    D.AnyT (translate_kind k, translate_typ (add_typ v env) t)
  | F.LamT (v, k, t) ->
    D.LamT (translate_kind k, translate_typ (add_typ v env) t)
  | F.AppT (t1, t2) -> D.AppT (translate_typ env t1, translate_typ env t2)
  | F.TupT tr -> D.TupT (map_row (translate_typ env) tr)
  | F.DotT (t, l) -> D.DotT (translate_typ env t, l)
  | F.RecT (v, k, t) ->
    D.RecT (translate_kind k, translate_typ (add_typ v env) t)
  | F.InferT _ -> translate_typ env (force_typ typ)

let translate_prim_fun (f : Prim.func) =
  match f.name with
  | "==" -> InnerPrim.Prim.Eq
  | "<>" -> InnerPrim.Prim.NotEq
  | "true" -> assert false
  | "false" -> assert false
  | "Int.+" -> InnerPrim.Prim.Plus
  | "Int.-" -> InnerPrim.Prim.Minus
  | "Int.*" -> InnerPrim.Prim.Times
  | "Int./" -> InnerPrim.Prim.Div
  | "Int.%" -> InnerPrim.Prim.Mod
  | "Int.<" -> InnerPrim.Prim.LT
  | "Int.>" -> InnerPrim.Prim.GT
  | "Int.<=" -> InnerPrim.Prim.LTE
  | "Int.>=" -> InnerPrim.Prim.GTE
  | "Int.print" -> InnerPrim.Prim.IntPrint
  | "Char.toInt" -> InnerPrim.Prim.CharToInt
  | "Char.fromInt" -> InnerPrim.Prim.CharFromInt
  | "Char.print" -> InnerPrim.Prim.CharPrint
  | "Text.++" -> InnerPrim.Prim.TextConcat
  | "Text.<" -> InnerPrim.Prim.TextLT
  | "Text.>" -> InnerPrim.Prim.TextGT
  | "Text.<=" -> InnerPrim.Prim.TextLTE
  | "Text.>=" -> InnerPrim.Prim.TextGTE
  | "Text.length" -> InnerPrim.Prim.TextLength
  | "Text.sub" -> InnerPrim.Prim.TextSub
  | "Text.fromChar" -> InnerPrim.Prim.TextFromChar
  | "Text.print" -> InnerPrim.Prim.TextPrint
  | s -> raise (Error ("Undefined primitive constant: " ^ s))

let translate_prim_const c =
  match c with
  | Prim.BoolV b -> InnerPrim.Prim.BoolV b
  | Prim.IntV i -> InnerPrim.Prim.IntV i
  | Prim.CharV c -> InnerPrim.Prim.CharV c
  | Prim.TextV t -> InnerPrim.Prim.TextV t
  | Prim.FunV f -> translate_prim_fun f

let rec translate_exp env exp =
  match exp with
  | F.VarE v -> D.VarE v
  | F.PrimE c -> begin match c with
    | Prim.FunV f when f.name = "true" ->
      D.LamE ("", D.ProdT [], D.PrimE (InnerPrim.Prim.BoolV true))
    | Prim.FunV f when f.name = "false" ->
      D.LamE ("", D.ProdT [], D.PrimE (InnerPrim.Prim.BoolV false))
    | _ -> D.PrimE (translate_prim_const c)
  end
  | F.IfE (e1, e2, e3) ->
    D.IfE (translate_exp env e1, translate_exp env e2, translate_exp env e3)
  | F.LamE (v, t, e) -> D.LamE (v, translate_typ env t, translate_exp env e)
  | F.AppE (e1, e2) -> D.AppE (translate_exp env e1, translate_exp env e2)
  | F.TupE er -> D.TupE (map_row (translate_exp env) er)
  | F.DotE (e, l) -> D.DotE (translate_exp env e, l)
  | F.GenE (v, k, e) ->
    D.GenE (translate_kind k, translate_exp (add_typ v env) e)
  | F.InstE (e, t) -> D.InstE (translate_exp env e, translate_typ env t)
  | F.PackE (t, e, t') ->
    D.PackE (translate_typ env t, translate_exp env e, translate_typ env t')
  | F.OpenE (e1, a, x, e2) ->
    let env' = add_typ a env in
    D.OpenE (translate_exp env e1, x, translate_exp env' e2)
  | F.RollE (e, t) -> D.RollE (translate_exp env e, translate_typ env t)
  | F.UnrollE e -> D.UnrollE (translate_exp env e)
  | F.RecE (v, t, e) -> D.RecE (v, translate_typ env t, translate_exp env e)
  | F.LetE (e1, v, e2) ->
    D.LetE (translate_exp env e1, v, translate_exp env e2)

let type_check = ref false

let translate exp =
  if !type_check then begin
    print_endline "debruijnify: checking f";
    let typ = F.infer_exp F.empty exp in
    print_endline "debruijnify: translate exp";
    let exp' = translate_exp empty exp in
    print_endline "debruijnify: translate typ";
    let typ' = translate_typ empty typ in
    print_endline "debruijnify: check direct";
    let () = D.check_exp D.empty exp' typ' "debruijnify" in
    print_endline "debruijnify: finished";
    exp'
  end else
    translate_exp empty exp
