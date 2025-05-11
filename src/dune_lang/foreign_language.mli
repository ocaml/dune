open Import

type t =
  [ `C
  | `Cxx
  | `Asm
  ]

val equal : t -> t -> bool

(** Compares the proper names. *)
val compare : t -> t -> Ordering.t

val to_dyn : t -> Dyn.t
val decode : t Decoder.t

(** The proper name of a language, e.g. "C++" for [Cxx]. Useful for diagnostic
    messages. *)
val proper_name : [< `C | `Cxx | `Asm ] -> string

module Dict : sig
  type 'a t

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val c : 'a t -> 'a
  val cxx : 'a t -> 'a
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val mapi : 'a t -> f:(language:[ `C | `Cxx ] -> 'a -> 'b) -> 'b t
  val make_all : 'a -> 'a t
  val make : c:'a -> cxx:'a -> 'a t
  val update : 'a t -> [ `C | `Cxx ] -> f:('a -> 'a) -> 'a t
  val merge : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val get : 'a t -> [ `C | `Cxx ] -> 'a
end

val source_extensions : (t * (int * int)) String.Map.t
val header_extension : Filename.Extension.t
val has_foreign_extension : fn:string -> bool
