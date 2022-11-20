open Import

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
  type language := t

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

val source_extensions : (t * (int * int)) String.Map.t

val header_extension : string

val has_foreign_extension : fn:string -> bool
