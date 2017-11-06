(* Syntax *)

type var = int

type kind =
  | BaseK
  | ArrK of kind * kind
  | ProdK of kind list

type typ =
  | VarT of int * int option
  | PrimT of Prim.typ
  | ProdT of typ list
  | AllT of var * kind * typ
  | AnyT of var * kind * typ
  | AppT of typ * typ
  | TupT of typ list
  | DotT of typ * int
  | RecT of var * kind * typ

type exp =
  | VarE of var
  | PrimE of Prim.const
  | IfE of exp * exp * exp
  | LamE of var * typ * exp
  | AppE of exp * exp
  | TupE of exp list
  | DotE of exp * int
  | GenE of var * kind * exp
  | InstE of exp * typ
  | PackE of typ * exp * typ
  | OpenE of exp * var * var * exp
  | RollE of exp * typ
  | UnrollE of exp
  | RecE of var * typ * exp
  | LetE of exp * var * exp

exception Error of string
exception Unimplemented

(* Substitutions *)

let lift_kind = raise Unimplemented
let lift_typ = raise Unimplemented
let lift_exp = raise Unimplemented

type 'a subst = (var * 'a) list

let subst_typ = raise Unimplemented
let subst_typ_exp = raise Unimplemented
let subst_exp = raise Unimplemented

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
  try lift_kind (i + 1) (List.nth kenv i) with
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