(** A collection of rules for one or multiple directories. *)

open! Stdune

(** Generic representation of a scheme *)
module Gen : sig
  type 'rules t =
    | Empty
    | Union of 'rules t * 'rules t
    | Approximation of Dir_set.t * 'rules t
    | Finite of 'rules Path.Build.Map.t
    | Thunk of (unit -> 'rules t)
end

module type S = Scheme_intf.S with module Gen := Gen

module Make(Directory_rules : sig
    type t
    val empty : t
    val union : t -> t -> t
  end) : S with type dir_rules := Directory_rules.t

include S with type dir_rules := Rules.rule
