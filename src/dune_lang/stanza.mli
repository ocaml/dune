(** Stanza in dune/jbuild files *)

open! Stdune
open Dune_sexp

type repr = ..
type t

val repr : t -> repr

module Key : sig
  type stanza := t
  type 'a t

  val get : 'a t -> stanza -> 'a option
end

module type S = sig
  type stanza := t
  type t
  type repr += T of t

  val make_stanza : t -> stanza
  val key : t Key.t
end

module Make (S : sig
    type t

    val hash : t -> int
    val compare : t -> t -> Ordering.t
  end) : S with type t := S.t

val equal : t -> t -> bool
val hash : t -> int
val latest_version : Syntax.Version.t

module Parser : sig
  (** Type of stanza parser.

      Each stanza in a configuration file might produce several values of type
      [t], hence the [t list] here. *)
  type nonrec t = string * t list Decoder.t
end

(** Syntax identifier for the Dune language. [(0, X)] correspond to the Jbuild
    language while versions from [(1, 0)] correspond to the Dune one. *)
val syntax : Syntax.t
