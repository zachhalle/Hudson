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

type env = EnvImpl

let empty = raise Unimplemented
let add_typ = raise Unimplemented
let add_val = raise Unimplemented

let lookup_typ = raise Unimplemented
let lookup_val = raise Unimplemented

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