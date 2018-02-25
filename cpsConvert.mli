exception Error of string

val type_check : bool ref
val translate : ILDirect.exp -> ILCps.exp