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
  | ArrT of typ * typ
  | ProdT of typ row
  | AllT of kind * typ (* binds *)
  | AnyT of kind * typ (* binds *)
  | AppT of typ * typ
  | LamT of kind * typ (* binds *)
  | TupT of typ row
  | DotT of typ * lab
  | RecT of kind * typ (* binds *)

type exp =
  | VarE of var
  | PrimE of InnerPrim.Prim.const
  | IfE of exp * exp * exp
  | LamE of var * typ * exp
  | AppE of exp * exp
  | TupE of exp row
  | DotE of exp * lab
  | GenE of kind * exp (* binds *)
  | InstE of exp * typ
  | PackE of typ * exp * typ
  | OpenE of exp * var * exp (* binds *)
  | RollE of exp * typ
  | UnrollE of exp
  | RecE of var * typ * exp
  | LetE of exp * var * exp

exception Error of string

(* Substitutions *)

val lift_typ : int -> typ -> typ
val lift_exp : int -> exp -> exp

val subst_typ : typ -> typ -> typ
val subst_exp : typ -> exp -> exp

(* Environments *)

type env

val empty : env
val add_typ : kind -> env -> env
val add_val : var -> typ -> env -> env

val lookup_typ : int -> env -> kind (* raise Error *)
val lookup_val : var -> env -> typ (* raise Error *)

(* Normalisation and Equality *)

val norm_typ : typ -> typ (* raise Error *) (* absolute normalization *)
val whnf_typ : typ -> typ
val equal_typ : typ -> typ -> bool (* raise Error *)

(* Checking *)

val infer_typ : env -> typ -> kind (* raise Error *)
val infer_exp : env -> exp -> typ (* raise Error *)

val check_typ : env -> typ -> kind -> string -> unit (* raise Error *)
val check_exp : env -> exp -> typ -> string -> unit (* raise Error *)

(* String conversion *)

val verbose_exp_flag : bool ref
val verbose_typ_flag : bool ref

val string_of_kind : kind -> string
val string_of_typ : typ -> string
val string_of_exp : exp -> string

val exp_node_name : exp -> string
