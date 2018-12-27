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
  | HaltE
  | IfE of value * exp * exp
  | AppE of value * value
  | DotE of value * lab * var * exp
  | OpenE of value * var * exp (* binds *)
  | LetE of value * var * exp
  | RecE of var * typ * exp

and value =
  | VarV of var
  | LamV of var * typ * exp
  | TupV of value row
  | PackV of typ * value * typ
  | RollV of value * typ
  | UnrollV of value
  | PrimV of InnerPrim.Prim.const

exception Error of string

(* Substitutions *)

val lift_typ : int -> typ -> typ
val lift_exp : int -> exp -> exp
val lift_val : int -> value -> value

val subst_typ : typ -> typ -> typ
val subst_exp : typ -> exp -> exp
val subst_val : typ -> value -> value

(* Environments *)

type env

val empty : unit -> env
val add_typ : kind -> env -> env
val add_val : var -> typ -> env -> env
val new_var : env -> var

val lookup_typ : int -> env -> kind (* raise Error *)
val lookup_val : var -> env -> typ (* raise Error *)

(* Normalisation and Equality *)

val norm_typ : typ -> typ (* raise Error *) (* absolute normalization *)
val whnf_typ : typ -> typ
val equal_typ : typ -> typ -> bool (* raise Error *)
val equal_typ_exn : typ -> typ -> unit (* raise Error *)

(* Checking *)

val typ_of_prim : InnerPrim.Prim.const -> typ
val infer_typ : env -> typ -> kind (* raise Error *)
val infer_val : env -> value -> typ (* raise Error *)

val check_typ : env -> typ -> kind -> string -> unit (* raise Error *)
val check_exp : env -> exp -> string -> unit (* raise Error *)
val check_val : env -> value -> typ -> string -> unit (* raise Error *)

(* String conversions *)

val verbose_exp_flag : bool ref
val verbose_typ_flag : bool ref

val string_of_kind : kind -> string
val string_of_typ : typ -> string
val string_of_exp : exp -> string
val string_of_val : value -> string

val exp_node_name : exp -> string
val value_node_name : value -> string
