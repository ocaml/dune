open! Stdune

module Gen = struct
  type 'rules t =
    | Empty
    | Union of 'rules t * 'rules t
    | Approximation of Dir_set.t * 'rules t
    | Finite of 'rules Path.Build.Map.t
    | Thunk of (unit -> 'rules t)
end

module type S = sig

  type dir_rules

  type 'rules t_gen = 'rules Gen.t =
    | Empty
    | Union of 'rules t_gen * 'rules t_gen
    | Approximation of Dir_set.t * 'rules t_gen
    | Finite of 'rules Path.Build.Map.t
    | Thunk of (unit -> 'rules t_gen)

  type t = dir_rules t_gen

  module Evaluated : sig
    type t
  end

  module For_tests : sig
    val collect_rules_simple : t -> dir:Path.Build.t -> dir_rules
  end

  val evaluate : t -> Evaluated.t

  val get_rules : Evaluated.t -> dir:Path.Build.t -> dir_rules

  val all : t list -> t
end
