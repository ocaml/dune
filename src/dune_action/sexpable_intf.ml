open Stdune

module type Sexpable = sig
  type t

  val sexp_of_t : t -> Sexp.t

  val t_of_sexp : Sexp.t -> t Option.t
end
