(** Pretty printers *)

(** A document that is not yet rendered. The argument is the type of
    tags in the document. For instance tags might be used for
    styles. *)
type +'tag t

module type Tag_handler = sig
  type t
  type tag

  (** Initial tag handler *)
  val init : t

  (** Handle a tag: return the string that enables the tag, the
      handler while the tag is active and the string to disable the
      tag. *)
  val handle : t -> tag -> string * t * string
end

module Renderer : sig
  module type S = sig
    type tag
    type tag_handler

    val string
      :  unit
      -> (?margin:int -> ?tag_handler:tag_handler -> tag t -> string) Staged.t
    val channel
      :  out_channel
      -> (?margin:int -> ?tag_handler:tag_handler -> tag t -> unit) Staged.t
  end

  module Make(Tag_handler : Tag_handler) : S
    with type tag         := Tag_handler.tag
    with type tag_handler := Tag_handler.t
end

(** A simple renderer that doesn't take tags *)
module Render : Renderer.S
  with type tag         := unit
  with type tag_handler := unit

val nop : 'a t
val seq : 'a t -> 'a t -> 'a t
val concat : 'a t list -> 'a t
val vbox : ?indent:int -> 'a t -> 'a t
val hbox : 'a t -> 'a t

val int    : int    -> _ t
val string : string -> _ t
val char   : char   -> _ t
val list   : ('a -> 'b t) -> 'a list -> 'b t

val space   : _ t
val cut     : _ t
val newline : _ t

val text : string -> _ t

val tag : 'a -> 'a t -> 'a t
