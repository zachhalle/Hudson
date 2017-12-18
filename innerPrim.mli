module Prim : sig
  type const =
    | BoolV of bool
    | TextV of string
    | IntV of int
    | CharV of char
    | Eq
    | NotEq
    | Plus
    | Minus
    | Times
    | Div
    | Mod
    | LT
    | GT
    | LTE
    | GTE
    | IntPrint
    | CharToInt
    | CharFromInt
    | CharPrint
    | TextConcat
    | TextLT
    | TextGT
    | TextLTE
    | TextGTE
    | TextLength
    | TextSub
    | TextFromChar
    | TextPrint

  type typ =
    | BoolT
    | IntT
    | CharT
    | TextT
    | VarT of int

  val typ_of_const : const -> typ list * typ list

  val string_of_typ : typ -> string
  val string_of_const : const -> string

  module type InferArg = sig
    type typExt
    val primT : typ -> typExt
    val varT : int -> typExt
    val arrT : typExt -> typExt -> typExt
    val prodT : (string * typExt) list -> typExt
  end

  module type Infer = sig
    type typExt
    val infer_prim : const -> typExt
  end
      
  module MakeInfer (A : InferArg) : Infer with type typExt = A.typExt

end