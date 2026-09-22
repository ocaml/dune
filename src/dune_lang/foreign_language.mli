open Import

type t =
  | C
  | Cxx

val equal : t -> t -> bool
val to_dyn : t -> Dyn.t

(** The proper name of a language, e.g. "C++" for [Cxx]. Useful for diagnostic
    messages. *)
val proper_name : t -> string

module Dict : sig
  type language := t

  type 'a t =
    { c : 'a
    ; cxx : 'a
    }

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val make_both : 'a -> 'a t
  val make : c:'a -> cxx:'a -> 'a t
  val get : 'a t -> language -> 'a
end

val source_extensions : (t * (int * int)) String.Map.t
val header_extension : Filename.Extension.t
val has_foreign_extension : fn:string -> bool
