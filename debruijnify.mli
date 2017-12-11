exception Error of string

val type_check : bool ref
val translate : Fomega.exp -> ILDirect.exp