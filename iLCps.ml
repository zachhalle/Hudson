(* Syntax *)

type lab = string
type var = string

type 'a row = (lab * 'a) list

type kind =
  | BaseK
  | ArrK of kind * kind
  | ProdK of kind row

(* TODO: change this to use hash consing *)
type typ = (* de Bruijn representation *)
  | VarT of int
  | PrimT of InnerPrim.Prim.typ
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
  | RecE of var * typ * exp
  | LetE of value * var * exp

and value =
  | VarV of var
  | LamV of var * typ * exp
  | TupV of value row
  | PackV of typ * value * typ
  | RollV of value * typ
  | UnrollV of value

exception Error of string
exception Unimplemented

(* Substitutions *)

let lift_typ = raise Unimplemented
let lift_exp = raise Unimplemented

let subst_typ = raise Unimplemented
let subst_exp = raise Unimplemented

(* Environments *)

type env = int

let empty = raise Unimplemented
let add_typ = raise Unimplemented
let add_val = raise Unimplemented

let lookup_typ = raise Unimplemented
let lookup_val = raise Unimplemented

(* Normalisation and Equality *)

let norm_typ = raise Unimplemented
let whnf_typ = raise Unimplemented
let equal_typ = raise Unimplemented

(* Checking *)

let infer_typ = raise Unimplemented
let infer_exp = raise Unimplemented

let check_typ = raise Unimplemented
let check_exp = raise Unimplemented

(* String conversion *)

let verbose_exp_flag = ref true
let verbose_typ_flag = ref true

let string_of_kind = raise Unimplemented
let string_of_typ = raise Unimplemented
let string_of_exp = raise Unimplemented
