exception Error of string
exception Unimplemented

module F = Fomega
module D = ILDirect

let rec translate_kind k =
  match k with
  | F.BaseK -> D.BaseK
  | F.ArrK (k1, k2) -> D.ArrK (translate_kind k1, translate_kind k2)
  | F.ProdK ks -> raise Unimplemented

let translate exp = raise (Error "")