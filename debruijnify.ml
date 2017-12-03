exception Error of string
exception Unimplemented

module F = Fomega
module D = ILDirect

let map_row f r = List.map (fun (l, v) -> l, f v) r

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
  val add_val : F.var -> env -> D.var * env

  val lookup_typ : F.var -> env -> depth
  val lookup_val : F.var -> env -> D.var

end = struct

  module VarMap = Map.Make(String)

  type env =
    { ksize : int ;
      tenv : int VarMap.t ;
      venv : int VarMap.t }

  type depth = int

  let fresh =
    let r = ref 0 in
    let f () =
      let n = !r in
      r := n + 1;
      n
    in
    f

  let empty = { ksize = 0 ; tenv = VarMap.empty ; venv = VarMap.empty }

  let add_typ var { ksize ; tenv ; venv } =
    { ksize = ksize + 1 ;
      tenv = VarMap.add var ksize tenv ;
      venv = venv }

  let add_val var { ksize ; tenv ; venv } =
    let f = fresh () in
    (f, { ksize = ksize ;
          tenv = tenv ;
          venv = VarMap.add var f venv } )

  let lookup_typ var { ksize ; tenv ; venv } =
    try VarMap.find var tenv with
    | Not_found -> raise (Error "Undefined variable")

  let lookup_val var { ksize ; tenv ; venv } =
    try VarMap.find var venv with
    | Not_found -> raise (Error "Undefined variable")

end

include Env

let rec force_typ = function
  | F.InferT(t', _) -> force_typ (Lazy.force t')
  | t -> t

let rec translate_typ env typ =
  match typ with
  | F.VarT v -> D.VarT (lookup_typ v env)
  | F.PrimT t -> D.PrimT t
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

let rec translate_exp env exp =
  match exp with
  | F.VarE v -> D.VarE (lookup_val v env)
  | F.PrimE c -> D.PrimE c
  | F.IfE (e1, e2, e3) ->
    D.IfE (translate_exp env e1, translate_exp env e2, translate_exp env e3)
  | F.LamE (v, t, e) ->
    let v', env' = add_val v env in
    D.LamE (v', translate_typ env' t, translate_exp env' e)
  | F.AppE (e1, e2) -> D.AppE (translate_exp env e1, translate_exp env e2)
  | F.TupE er -> D.TupE (map_row (translate_exp env) er)
  | F.DotE (e, l) -> D.DotE (translate_exp env e, l)
  | F.GenE (v, k, e) -> 
    D.GenE (translate_kind k, translate_exp (add_typ v env) e)
  | F.InstE (e, t) -> D.InstE (translate_exp env e, translate_typ env t)
  | F.PackE (t, e, t') -> 
    D.PackE (translate_typ env t, translate_exp env e, translate_typ env t')
  | F.OpenE (e1, a, x, e2) ->
    let x', env' = add_val x (add_typ a env) in
    D.OpenE (translate_exp env e1, x', translate_exp env' e2)
  | F.RollE (e, t) -> D.RollE (translate_exp env e, translate_typ env t)
  | F.UnrollE e -> D.UnrollE (translate_exp env e)
  | F.RecE (v, t, e) ->
    let v', env' = add_val v env in
    D.RecE (v', translate_typ env' t, translate_exp env' e)
  | F.LetE (e1, v, e2) ->
    let v', env' = add_val v env in
    D.LetE (translate_exp env e1, v', translate_exp env' e2)

let translate exp = 
  let typ = F.infer_exp F.empty exp in
  let exp' = translate_exp empty exp in
  let () = D.check_exp D.empty exp' (translate_typ empty typ) "debruijnify" in
  exp'