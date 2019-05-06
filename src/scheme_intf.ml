open! Stdune

module type S = sig
  module Gen : sig
    type 'a t
  end

  type dir_rules

  type t = dir_rules Gen.t

  module Evaluated : sig
    type t
  end

  val evaluate : t -> Evaluated.t

  val get_rules : Evaluated.t -> dir:Path.Build.t -> dir_rules

  val all : t list -> t
end
