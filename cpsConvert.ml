exception Error of string

open InnerPrim
open ILCps
module D = ILDirect

let lab i = "_" ^ string_of_int i
let tup_row xs = List.mapi (fun i x -> lab (i + 1), x) xs
let map_row f r = List.map (fun (l, v) -> l, f v) r
let lookup_lab l row =
  try List.assoc l row with Not_found -> raise (Error ("label " ^ l))
let rec unzip xys =
  match xys with
  | [] -> [], []
  | (x, y) :: xys ->
    let xs, ys = unzip xys in
    x :: xs, y :: ys

let rec zip xs ys =
  match xs, ys with
  | [], [] -> []
  | x :: xs, y :: ys -> (x, y) :: zip xs ys
  | _ -> raise (Error "zip")

let rec translate_kind k =
  match k with
  | D.BaseK -> BaseK
  | D.ArrK (k1, k2) -> ArrK (translate_kind k1, translate_kind k2)
  | D.ProdK kr -> ProdK (map_row translate_kind kr)

let rec translate_typ t =
  match t with
  | D.VarT i -> VarT i
  | D.PrimT t -> PrimT t
  | D.ArrT (t1, t2) -> NotT (ProdT (tup_row [translate_typ t1; NotT (translate_typ t2)]))
  | D.ProdT tr -> ProdT (map_row translate_typ tr)
  | D.AllT (k, t) -> NotT (AnyT (translate_kind k, NotT (translate_typ t)))
  | D.AnyT (k, t) -> AnyT (translate_kind k, translate_typ t)
  | D.AppT (t1, t2) -> AppT (translate_typ t1, translate_typ t2)
  | D.LamT (k, t) -> LamT (translate_kind k, translate_typ t)
  | D.TupT tr -> TupT (map_row translate_typ tr)
  | D.DotT (t, l) -> DotT (translate_typ t, l)
  | D.RecT (k, t) -> RecT (translate_kind k, translate_typ t)

let appV k v = AppE (VarV k, VarV v)
let app k v = AppE (VarV k, v)
let cont k e lam = LetE (lam, k, e)
let bind k' k e = LetE (VarV k, k', e)
let tupleV xs = TupV (map_row (fun v -> VarV v) (tup_row xs))

let rec translate_exp' env e =
  match e with
  | D.VarE v ->
    let k = new_var env in
    k, appV k v, lookup_val v env
  | D.LetE (e1, x, e2) ->
    let k1, e1, t1 = translate_exp env e1 in
    let k2, e2, t2 = translate_exp (add_val x t1 env) e2 in
    let k = new_var env in
    let e = cont k1 e1 (LamV (x, t1, bind k2 k e2)) in
    k, e, t2
  | D.IfE (eb, e1, e2) ->
    let kb, b, PrimT Prim.BoolT = translate_exp env eb in
    let k1, e1, t1 = translate_exp env e1 in
    let k2, e2, t2 = translate_exp env e2 in
    equal_typ_exn t1 t2;
    let k, xb, x1, x2 = new_var env, new_var env, new_var env, new_var env in
    let e =
      cont kb b (
        LamV (xb, PrimT Prim.BoolT,
          cont k1 e1 (
            LamV (x1, t1, 
              cont k2 e2 (
                LamV (x2, t1, 
                  IfE (VarV xb, appV k x1, appV k x2)
                ))))))
    in
    k, e, t1
  | D.LamE (x, t1, e) ->
    let t1 = translate_typ t1 in
    let k', e, t2 = translate_exp (add_val x t1 env) e in
    let t = ProdT (tup_row [t1; NotT t2]) in
    let k, y = new_var env, new_var env in
    let e =
      app k (LamV (y, t,
        DotE (VarV y, lab 0, x, 
          DotE (VarV y, lab 1, k', e)
        )))
    in
    k, e, NotT t
  | D.AppE (e1, e2) ->
    let k1, e1, (NotT (ProdT t1nott2) as t) = translate_exp env e1 in
    let t1, NotT t2 = lookup_lab (lab 0) t1nott2, lookup_lab (lab 1) t1nott2 in
    let k2, e2, t1' = translate_exp env e2 in
    equal_typ_exn t1 t1';
    let k, f, x = new_var env, new_var env, new_var env in
    let e =
      cont k1 e1 (LamV (f, t,
        cont k2 e2 (LamV (x, t1,
          app f (tupleV [x; k])
        ))))
    in
    k, e, t2
  | D.TupE er ->
    let ls, kets = unzip (map_row (translate_exp env) er) in
    let t = ProdT (zip ls (List.map (fun (_, _, ti) -> ti) kets)) in
    let xs = List.map (fun _ -> new_var env) kets in
    let k = new_var env in
    let base = app k (TupV (map_row (fun x -> VarV x) (zip ls xs))) in
    let combine ((ki, ei, ti), xi) e = cont ki ei (LamV (xi, ti, e)) in
    let e = List.fold_right combine (zip kets xs) base in
    k, e, t
  | D.DotE (e, l) ->
    let k', e, (ProdT tr as t) = translate_exp env e in
    let k, x, x' = new_var env, new_var env, new_var env in
    let ti = lookup_lab l tr in
    let e =
      cont k' e (LamV (x, t, 
        DotE (VarV x, l, x', appV k x')
      ))
    in
    k, e, ti
  | D.GenE (kind, e) ->
    let kind = translate_kind kind in
    let k', e, t' = translate_exp (add_typ kind env) e in
    let t = AnyT (kind, NotT t') in
    let k, x = new_var env, new_var env in
    let e = app k (LamV (x, t, OpenE (VarV x, k', e))) in
    k, e, NotT t
  | D.InstE (e, t1) ->
    let t1 = translate_typ t1 in
    let k', e, (NotT (AnyT (kind, NotT t2) as t')) = translate_exp env e in
    check_typ env t1 kind "inst1";
    check_typ (add_typ kind env) t2 BaseK "inst2";
    let k, f = new_var env, new_var env in
    let t = subst_typ t1 t2 in
    let e =
      cont k' e (LamV (f, t', 
        app f (PackV (t1, VarV k, 
          AnyT (kind, NotT t2)
        ))))
    in
    k, e, t
  | D.PackE (t1, e, akt2) ->
    let t1 = translate_typ t1 in
    let AnyT (kind, t2) as akt2 = translate_typ akt2 in
    let k', e, _ = translate_exp env e in
    let k, x = new_var env, new_var env in
    let e =
      cont k' e (
        LamV (x, subst_typ t1 t2, app k (
          PackV (t1, VarV x, akt2)
        )))
    in
    k, e, akt2
  | D.OpenE (e1, x, e2) ->
    let k1, e1, (AnyT (kind, t) as at) = translate_exp env e1 in
    let env' = add_val x t (add_typ kind env) in
    let k2, e2, t' = translate_exp env' e2 in
    let k, x1, x2 = new_var env, new_var env, new_var env in
    let e =
      cont k1 e1 (
        LamV (x1, at, 
          OpenE (VarV x1, x,
            bind k2 k e2
          )))
    in
    k, e, t'
  | D.RollE (e, t) ->
    let t = translate_typ t in
    let k', e, t_e = translate_exp env e in
    let k, x = new_var env, new_var env in
    let e = cont k' e (LamV (x, t_e, app k (RollV (VarV x, t)))) in
    k, e, t
  | D.UnrollE e ->
    let k', e, (RecT (_, t') as t) = translate_exp env e in
    let k, x = new_var env, new_var env in
    let e =
      cont k' e (LamV (x, t, 
        app k (UnrollV (VarV x))
      ))
    in
    k, e, subst_typ t t'
  | D.PrimE prim ->
    let k = new_var env in
    k, app k (PrimV prim), typ_of_prim prim
  | D.RecE (x, t, e) ->
    let t = translate_typ t in
    let k', e, _ = translate_exp (add_val x t env) e in
    let k = new_var env in
    let e = bind k k' (RecE (x, t, e)) in
    k, e, t

and translate_exp env e =
  let k, e, t = translate_exp' env e in
  check_exp (add_val k (NotT t) env) e "translate_exp";
  k, e, t

let type_check = ref true
let translate e =
  let env = empty () in
  let k, e, tau = translate_exp env e in
  let init_k = LamV (new_var env, tau, HaltE) in
  cont k e init_k
