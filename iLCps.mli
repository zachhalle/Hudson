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

(* Substitutions *)

val lift_typ : int -> typ -> typ
val lift_exp : int -> exp -> exp

val subst_typ : typ -> typ -> typ
val subst_exp : typ -> exp -> exp
