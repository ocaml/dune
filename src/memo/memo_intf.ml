open Stdune

module type Sexpable = sig
  type t
  val to_sexp : t -> Sexp.t
end

module type Data = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
  val to_sexp : t -> Sexp.t
end

module type Input = Data
