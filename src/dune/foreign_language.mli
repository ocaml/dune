open Stdune

(* CR-soon cwong: I am deeply unsatisfied with the refactoring that lead to the
   existence of this module, and would like to delete it. This module (and the
   last three functions) were intially part of [Foreign], but needed to be split
   separately to keep [Foreign] cleanly in the front-end. However, the only real
   backend use of these files is that [Utils] uses [has_foreign_extension],
   which could theoretically be moved to the backedn by itself. However, that
   function relies on the key set of [source_extensions], which ultimately pulls
   in the rest of this infrastructure. In theory, we could just hardcode that
   key set somewhere (such as in [Utils]), that would add extra maintenance
   burden to ensure that the external keyset evolves in sync with the
   [source_extensions] here, and so this module was created to hold it. *)

type t =
  | C
  | Cxx

val compare : t -> t -> ordering

val equal : t -> t -> bool

val to_dyn : t -> Dyn.t

(** The proper name of a language, e.g. "C++" for [Cxx]. Useful for diagnostic
    messages. *)
val proper_name : t -> string

module Map : Map.S with type key = t

module Dict : sig
  type language

  type 'a t =
    { c : 'a
    ; cxx : 'a
    }

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val c : 'a t -> 'a

  val cxx : 'a t -> 'a

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val mapi : 'a t -> f:(language:language -> 'a -> 'b) -> 'b t

  val make_both : 'a -> 'a t

  val make : c:'a -> cxx:'a -> 'a t

  val update : 'a t -> language -> f:('a -> 'a) -> 'a t

  val merge : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  val get : 'a t -> language -> 'a
end
with type language := t

val source_extensions : (t * (int * int)) String.Map.t

val header_extension : string

val has_foreign_extension : fn:string -> bool
