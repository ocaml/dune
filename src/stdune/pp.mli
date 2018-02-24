(** Pretty printers *)

(** A document that is not yet rendered. The argument is the type of
    tags in the document. For instance tags might be used for
    styles. *)
type +'tag t

module type Tag_interpretation = sig
  type t
  val compare : t -> t -> Ordering.t
  val open_tag  : t -> string
  val close_tag : t -> string
end

module Renderer : sig
  module type S = sig
    type tag

    val to_string  : ?margin:int -> tag t -> string
    val to_channel : ?margin:int -> tag t -> out_channel -> unit
  end

  module Make(Tag : Tag_interpretation) : S with type tag := Tag.t
end

(** A simple renderer that doesn't take tags *)
module Render : Renderer.S with type tag := unit

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
