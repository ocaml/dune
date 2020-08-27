(** DSL to define sets that are defined by a membership : 'a -> bool function. *)

open! Stdune

type 'a t =
  | Element of 'a
  | Compl of 'a t
  | Standard
  | Union of 'a t list
  | Inter of 'a t list

val diff : 'a t -> 'a t -> 'a t

val inter : 'a t list -> 'a t

val compl : 'a t -> 'a t

val union : 'a t list -> 'a t

val not_union : 'a t list -> 'a t

val any : 'a t

val decode_one : 'a Dune_lang.Decoder.t -> 'a t Dune_lang.Decoder.t

val decode : 'a Dune_lang.Decoder.t -> 'a t Dune_lang.Decoder.t

val encode : 'a Dune_lang.Encoder.t -> 'a t Dune_lang.Encoder.t

val to_dyn : 'a Dyn.Encoder.t -> 'a t Dyn.Encoder.t

val exec : 'a t -> standard:'a t -> ('a -> bool) -> bool

val empty : 'a t

module Glob : sig
  type glob

  type nonrec t = glob t

  val to_dyn : t -> Dyn.t

  val decode : t Dune_lang.Decoder.t

  val exec : t -> standard:t -> string -> bool

  val filter : t -> standard:t -> string list -> string list

  val of_glob : Glob.t -> t

  val of_pred : (string -> bool) -> t

  val of_string_set : String.Set.t -> t

  val true_ : t
end
