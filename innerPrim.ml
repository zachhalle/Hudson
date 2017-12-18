module Prim = struct

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

  let typ_of_const c =
    match c with
    | BoolV _ -> [], [BoolT]
    | TextV _ -> [], [TextT]
    | IntV _ -> [], [IntT]
    | CharV _ -> [], [CharT]
    | Eq -> [VarT 0; VarT 0], [BoolT]
    | NotEq -> [VarT 0; VarT 0], [BoolT]
    | Plus -> [IntT; IntT], [IntT]
    | Minus -> [IntT; IntT], [IntT]
    | Times -> [IntT; IntT], [IntT]
    | Div -> [IntT; IntT], [IntT]
    | Mod -> [IntT; IntT], [IntT]
    | LT -> [IntT; IntT], [BoolT]
    | GT -> [IntT; IntT], [BoolT]
    | LTE -> [IntT; IntT], [BoolT]
    | GTE -> [IntT; IntT], [BoolT]
    | IntPrint -> [IntT], []
    | CharToInt -> [CharT], [IntT]
    | CharFromInt -> [IntT], [CharT]
    | CharPrint -> [CharT], []
    | TextConcat -> [TextT; TextT], [TextT]
    | TextLT -> [TextT; TextT], [BoolT]
    | TextGT -> [TextT; TextT], [BoolT]
    | TextLTE -> [TextT; TextT], [BoolT]
    | TextGTE -> [TextT; TextT], [BoolT]
    | TextLength -> [TextT], [IntT]
    | TextSub -> [TextT; IntT], [CharT]
    | TextFromChar -> [CharT], [TextT]
    | TextPrint -> [TextT], []

  let string_of_typ t =
    match t with
    | BoolT -> "bool"
    | IntT -> "int"
    | CharT -> "char"
    | TextT -> "text"
    | VarT i -> "(tvar " ^ string_of_int i ^ ")"

  let string_of_const c =
    match c with
    | BoolV b -> string_of_bool b
    | CharV c -> "'" ^ Char.escaped c ^ "'"
    | IntV i -> string_of_int i
    | TextV t -> "\"" ^ String.escaped t ^ "\""
    | Eq -> "=="
    | NotEq -> "<>"
    | Plus -> "Int.+"
    | Minus -> "Int.-"
    | Times -> "Int.*"
    | Div -> "Int./"
    | Mod -> "Int.%"
    | LT -> "Int.<"
    | GT -> "Int.>"
    | LTE -> "Int.<="
    | GTE -> "Int.>="
    | IntPrint -> "Int.print"
    | CharToInt -> "Char.toInt"
    | CharFromInt -> "Char.fromInt"
    | CharPrint -> "Char.print"
    | TextConcat -> "Text.++"
    | TextLT -> "Text.<"
    | TextGT -> "Text.>"
    | TextLTE -> "Text.<="
    | TextGTE -> "Text.>="
    | TextLength -> "Text.length"
    | TextSub -> "Text.sub"
    | TextFromChar -> "Text.fromChar"
    | TextPrint -> "Text.print"

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
      
  module MakeInfer (A : InferArg) : Infer with type typExt = A.typExt = struct
    
    type typExt = A.typExt

    open A

    let outT t =
      match t with
      | VarT i -> varT i
      | _ -> primT t

    let lab i = "_" ^ string_of_int i
    let row_out ts = prodT (List.mapi (fun i x -> lab (i + 1), outT x) ts)

    let infer_prim c =
      match typ_of_const c with
      | [], [t] -> outT t
      | [], ts -> row_out ts
      | [t1], [t2] -> arrT (outT t1) (outT t2)
      | [t1], ts -> arrT (outT t1) (row_out ts)
      | ts, [t2] -> arrT (row_out ts) (outT t2)
      | ts1, ts2 -> arrT (row_out ts1) (row_out ts2)

   end

end