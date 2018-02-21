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
  val add_typ : F.var -> D.kind -> env -> env
  val add_val : F.var -> D.typ -> env -> env

  val lookup_typ : F.var -> env -> depth * D.kind
  val lookup_val : F.var -> env -> D.typ

  val get_env : env -> D.env

end = struct

  module VarMap = Map.Make(String)

  let fresh =
    let n = ref 0 in
    let f () =
      let i = !n in
      n := i + 1;
      i
    in f

  type env =
    { ksize : int ;
      tvar_map : int VarMap.t ; 
      env : D.env ;
      id : int }

  type depth = int

  let empty = 
    { ksize = 0 ; tvar_map = VarMap.empty ; env = D.empty ; id = fresh () }

  let add_typ var k { ksize ; tvar_map ; env ; id } =
    { ksize = ksize + 1 ;
      tvar_map = VarMap.add var ksize tvar_map ; 
      env = D.add_typ k env ;
      id = fresh () }

  let add_val var t { ksize ; tvar_map ; env ; id } =
    { ksize = ksize ;
      tvar_map = tvar_map ; 
      env = D.add_val var t env ;
      id = id }

  let lookup_typ var { ksize ; tvar_map ; env ; id } =
    try
      let i = ksize - VarMap.find var tvar_map - 1 in
      i, D.lookup_typ i env
    with
    | Not_found -> raise (Error "Undefined variable")

  let lookup_val var { ksize ; tvar_map ; env ; id } = D.lookup_val var env

  let get_env { ksize ; tvar_map ; env ; id } = env

end

include Env

let rec zip xs ys =
  match xs, ys with
  | [], [] -> []
  | x :: xs, y :: ys -> (x, y) :: zip xs ys 

let rec unzip zs =
  match zs with
  | [] -> [], []
  | (x, y) :: zs' ->
    let xs, ys = unzip zs' in
    x :: xs, y :: ys

let rec force_typ = function
  | F.InferT(t', _) -> force_typ (Lazy.force t')
  | t -> t

let translate_prim_typ env p =
  match p with
  | Prim.BoolT -> InnerPrim.Prim.BoolT
  | Prim.IntT -> InnerPrim.Prim.IntT
  | Prim.CharT -> InnerPrim.Prim.CharT
  | Prim.TextT -> InnerPrim.Prim.TextT
  | Prim.VarT v -> let i, _ = lookup_typ v env in InnerPrim.Prim.VarT i

let rec translate_typ env typ =
  match typ with
  | F.VarT v -> let i, _ = lookup_typ v env in D.VarT i
  | F.PrimT t -> D.PrimT (translate_prim_typ env t)
  | F.ArrT (t1, t2) -> 
    D.ArrT (translate_typ env t1, translate_typ env t2)
  | F.ProdT tr -> D.ProdT (map_row (translate_typ env) tr)
  | F.AllT (v, k, t) ->
    let k' = translate_kind k in
    D.AllT (k', translate_typ (add_typ v k' env) t)
  | F.AnyT (v, k, t) ->
    let k' = translate_kind k in
    D.AnyT (k', translate_typ (add_typ v k' env) t)
  | F.LamT (v, k, t) ->
    let k' = translate_kind k in
    D.LamT (k', translate_typ (add_typ v k' env) t)
  | F.AppT (t1, t2) -> D.AppT (translate_typ env t1, translate_typ env t2)
  | F.TupT tr -> D.TupT (map_row (translate_typ env) tr)
  | F.DotT (t, l) -> D.DotT (translate_typ env t, l)
  | F.RecT (v, k, t) ->
    let k' = translate_kind k in
    D.RecT (k', translate_typ (add_typ v k' env) t)
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

module InferPrim = InnerPrim.Prim.MakeInfer (
  struct 
    type typExt = D.typ
    let primT t = D.PrimT t
    let varT i = D.VarT i
    let arrT t1 t2 = D.ArrT (t1, t2)
    let prodT ts = D.ProdT ts
  end
)

let translate_prim_const c =
  match c with
  | Prim.BoolV b -> InnerPrim.Prim.BoolV b
  | Prim.IntV i -> InnerPrim.Prim.IntV i
  | Prim.CharV c -> InnerPrim.Prim.CharV c
  | Prim.TextV t -> InnerPrim.Prim.TextV t
  | Prim.FunV f -> translate_prim_fun f

let equal_typ s t1 t2 = 
  if not (D.equal_typ t1 t2) then begin
    raise (Error s)
  end

let stack : string list ref = ref []
let push e = 
  let s =
    match e with
    | F.VarE _ -> "var"
    | F.PrimE _ -> "prim"
    | F.IfE _ -> "if"
    | F.LamE _ -> "lam"
    | F.AppE _ -> "app"
    | F.TupE _ -> "tup"
    | F.DotE _ -> "dot"
    | F.GenE _ -> "gen"
    | F.InstE _ -> "inst"
    | F.PackE _ -> "pack"
    | F.OpenE _ -> "open"
    | F.RollE _ -> "roll"
    | F.UnrollE _ -> "unroll"
    | F.RecE _ -> "rec"
    | F.LetE _ -> "let"
  in stack := s :: !stack
let pop () = let _ :: s = !stack in stack := s
let show_stack () =
  if !stack = [] then
    print_endline "stack empty"
  else
    List.iter print_endline (List.rev (!stack)) 

let rec translate_exp env exp =
  match exp with
  | F.VarE v -> D.VarE v, lookup_val v env
  | F.PrimE c -> begin match c with
      | Prim.FunV f when f.name = "true" ->
        let c' = D.LamE ("", D.ProdT [], D.PrimE (InnerPrim.Prim.BoolV true)) in
        c', D.infer_exp D.empty c'
      | Prim.FunV f when f.name = "false" ->
        let c' = D.LamE ("", D.ProdT [], D.PrimE (InnerPrim.Prim.BoolV false)) in
        c', D.infer_exp D.empty c'
      | _ -> 
        let c' = translate_prim_const c in D.PrimE c', InferPrim.infer_prim c'
    end
  | F.IfE (e1, e2, e3) ->
    let e1', D.PrimT InnerPrim.Prim.BoolT = translate_main "if1" env e1 in
    let e2', t1 = translate_main "if2" env e2 in
    let e3', t2 = translate_main "if3" env e3 in
    equal_typ "if4" t1 t2;
    D.IfE (e1', e2', e3'), t1
  | F.LamE (v, t1, e) ->
    let t1' = translate_typ env t1 in
    D.check_typ (get_env env) t1' D.BaseK "lam3";
    let e', t2 = translate_main "lam" (add_val v t1' env) e in
    D.check_typ (get_env env) t2 D.BaseK "lam2";
    D.LamE (v, t1', e'), D.ArrT (t1', t2);
  | F.AppE (e1, e2) ->
    let e1', D.ArrT (t1, t2) = translate_main "app1" env e1 in
    let e2', t1' = translate_main "app2" env e2 in
    equal_typ "app3" t1 t1';
    D.AppE (e1', e2'), t2
  | F.TupE er ->
    let ls_es_ts = map_row (translate_main "tup" env) er in
    let ls, es_ts = unzip ls_es_ts in
    let es, ts = unzip es_ts in
    D.TupE (zip ls es), D.ProdT (zip ls ts)
  | F.DotE (e, l) ->
    let e', D.ProdT ts = translate_main "dot" env e in
    D.DotE (e', l), lookup_lab l ts
  | F.GenE (a, k, e) ->
    let k' = translate_kind k in
    let e', t = translate_main "gen" (add_typ a k' env) e in
    D.GenE (k', e'), D.AllT (k', t)
  | F.InstE (e, t1) ->
    let e', D.AllT (k, t2) = translate_main "inst1" env e in
    let t1' = translate_typ env t1 in
    D.check_typ (get_env env) t1' k "inst2";
    D.InstE (e', t1'), D.subst_typ t1' t2
  | F.PackE (t, e, aktau) ->
    let F.AnyT (a, k, tau) = aktau in
    let e, tau_e = translate_main "pack1" env e in
    let t = translate_typ env t in
    let k = translate_kind k in
    D.check_typ (get_env env) t k "pack2";
    let env' = add_typ a k env in
    let tau = translate_typ env' tau in
    D.check_typ (get_env env') tau D.BaseK "pack3";
    equal_typ "pack4" (D.subst_typ t tau) tau_e;
    D.PackE (t, e, D.AnyT (k, tau)), D.AnyT (k, tau) 
  | F.OpenE (e1, a, x, e2) ->
    let e1', D.AnyT (k, tau1) = translate_main "open1" env e1 in
    let env' = add_val x tau1 (add_typ a k env) in
    let e2', tau2' = translate_main "open2" env' e2 in
    let tau2 = D.subst_typ (D.VarT 0) tau2' in
    D.check_typ (get_env env) tau2 D.BaseK "open3";
    D.OpenE (e1', x, e2'), tau2
  | F.RollE (e, tktau) ->
    let e', _ = translate_main "roll1" env e in
    let tktau' = translate_typ env tktau in
    D.RollE (e', tktau'), tktau'
  | F.UnrollE e ->
    let e', tau_e = translate_main "unroll1" env e in
    let D.RecT (k, tau) = tau_e in
    D.UnrollE e', D.subst_typ tau_e tau
  | F.RecE (v, t, e) ->
    let t' = translate_typ env t in
    let e', t'' = translate_main "rec1" (add_val v t' env) e in
    equal_typ "rec2" t' t'';
    D.RecE (v, t', e'), t'
  | F.LetE (e1, v, e2) ->
    let e1', t1 = translate_main "let1" env e1 in
    D.check_typ (get_env env) t1 D.BaseK "let3";
    let e2', t2 = translate_main "let2" (add_val v t1 env) e2 in
    D.check_typ (get_env env) t2 D.BaseK "let4";
    D.LetE (e1', v, e2'), t2

  (*match exp with
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
    D.LetE (translate_exp env e1, v, translate_exp env e2)*)

and translate_main s env exp =
  push exp;
  let ans = 
    begin try
      let exp', typ = translate_exp env exp in
      D.check_exp (get_env env) exp' typ s;
      exp', D.whnf_typ typ
    with
      | D.Error s -> (*show_stack ();*) raise (D.Error s)
      | Error s -> (*show_stack ();*) raise (Error s)
    end
  in
  pop ();
  ans

let type_check = ref false

let translate exp =
  F.infer_exp F.empty exp;
  let exp', _ = translate_main "debruijnify" empty exp in exp'
  (*if !type_check then begin
    print_endline "debruijnify: checking f";
    let typ = F.infer_exp F.empty exp in
    print_endline "debruijnify: translate";
    let exp', typ = translate_exp empty exp in
    print_endline "debruijnify: check direct";
    let () = D.check_exp D.empty exp' (translate_typ empty typ) "debruijnify" in
    print_endline "debruijnify: finished";
    exp'
  end else
    translate_exp empty exp*)