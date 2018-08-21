include module type of struct include Usexp end with module Loc := Usexp.Loc

module To_sexp : sig
  type sexp = t
  include Sexp_intf.Combinators with type 'a t = 'a -> t

  val record : (string * sexp) list -> sexp

  val unknown : _ t
end with type sexp := t
