(** Pretty printers *)

(** A document that is not yet rendered. The argument is the type of tags in the
    document. For instance tags might be used for styles. *)
type +'tag t

(** {1 Basic combinators} *)

(** A pretty printer that prints nothing *)
val nop : _ t

(** [seq x y] prints [x] and then [y] *)
val seq : 'a t -> 'a t -> 'a t

(** [concat ?sep l] prints elements in [l] separated by [sep]. [sep] defaults to
    [nop]. *)
val concat : ?sep:'a t -> 'a t list -> 'a t

(** Convenience function for [List.map] followed by [concat] *)
val concat_map : ?sep:'a t -> 'b list -> f:('b -> 'a t) -> 'a t

val concat_mapi : ?sep:'a t -> 'b list -> f:(int -> 'b -> 'a t) -> 'a t

(** An indivisible block of text *)
val verbatim : string -> _ t

(** A single character *)
val char : char -> _ t

(** Print a bunch of text. The line may be broken at any spaces in the text. *)
val text : string -> _ t

(** Same as [text] but take a format string as argument. *)
val textf : ('a, unit, string, _ t) format4 -> 'a

(** [tag x t] Tag the material printed by [t] with [x] *)
val tag : 'a -> 'a t -> 'a t

(** {1 Break hints} *)

(** Either a newline or a space, depending on whether the line is broken at this
    point. *)
val space : _ t

(** Either a newline or nothing, depending on whether the line is broken at this
    point. *)
val cut : _ t

(** Either a newline or [nspaces] spaces. If it is a newline, [shift] is added
    to the indentation level. *)
val break : nspaces:int -> shift:int -> _ t

(** Force a newline to be printed *)
val newline : _ t

(** Convert tags in a documents *)
val map_tags : 'a t -> f:('a -> 'b) -> 'b t

val filter_map_tags : 'a t -> f:('a -> 'b option) -> 'b t

(** {1 Boxes} *)

(** Boxes are the basic components to control the layout of the text. Break
    hints such as [space] and [cut] may cause the line to be broken, depending
    on the splitting rules. Whenever a line is split, the rest of the material
    printed in the box is indented with [indent]. *)

(** Try to put as much as possible on each line. Additionally, a break hint
    always break the line if the breaking would reduce the indentation level
    ([break] with negative [shift] value). *)
val box : ?indent:int -> 'a t -> 'a t

(** Always break the line when encountering a break hint. *)
val vbox : ?indent:int -> 'a t -> 'a t

(** Print everything on one line, no matter what *)
val hbox : 'a t -> 'a t

(** If possible, print everything on one line. Otherwise, behave as a [vbox] *)
val hvbox : ?indent:int -> 'a t -> 'a t

(** Try to put as much as possible on each line. *)
val hovbox : ?indent:int -> 'a t -> 'a t

(** {1 Common convenience functions} *)

(** [enumerate l ~f] produces an enumeration of the form:

    {v - item1 - item2 - item3 ... v} *)
val enumerate : 'a list -> f:('a -> 'b t) -> 'b t

(** [chain l ~f] is used to print a succession of items that follow each other.
    It produces an output of this form:

    {v item1 -> item2 -> item3 ... v} *)
val chain : 'a list -> f:('a -> 'b t) -> 'b t

(** {1 Operators} *)

module O : sig
  (** Same as [seq] *)
  val ( ++ ) : 'a t -> 'a t -> 'a t
end

(** {1 Rendering} *)

(** Render a document to a classic formatter *)
val render :
     Format.formatter
  -> 'a t
  -> tag_handler:(Format.formatter -> 'a -> 'a t -> unit)
  -> unit

val render_ignore_tags : Format.formatter -> 'a t -> unit
