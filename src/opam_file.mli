(** Parsing and interpretation of opam files *)

open! Stdune

open OpamParserTypes

(** Type of opam files *)
type t = opamfile

(** Load a file *)
val load : Path.t -> t

(** Extracts a field *)
val get_field : t -> string -> value option

(** Parse the contents of an opam file *)
val parse : Lexing.lexbuf -> t

val of_string : path:Path.t -> string -> t

(** Replace all [pos] value by a triplet [(fname, line,
    absolute_offset)] *)
val absolutify_positions : file_contents:string -> opamfile -> opamfile

val nopos : OpamParserTypes.pos

module Mutator : sig
  open OpamParserTypes

  type t

  val (>>>) : t -> t -> t

  val mkstring : string -> value

  val mklist : ('a -> value) -> 'a list -> value

  val set_var : string -> value -> t

  (** [fixup] is a mutator that strips leading '\n's from variables.
      Without this we accumulate newlines in long strings *)
  val fixup : t

  (** Identity mutator *)
  val id : t

  (** [set_string v s] is a mutator that sets the opam variable [v] to the
      string [s]. If [v] is already bound in the opamfile the value is updated.
      If [v] is not present in the opam file it is inserted at the top of the
      file *)
  val set_string : string -> string -> t

  (** [set_list v conv l] is a mutator that sets the opam variable [v] to the
      list [l] after applying the convertor [conv] to the elements of [l]. If
      [v] is already bound in the opamfile the value is updated. If [v] is not
      present in the opam
      file it is inserted at the top of the file. *)
  val set_list : string -> ('a -> value) -> 'a list -> t

  (** [opt v f] returns an identity transformer if [v] is None and if it is
      [Some x] applies [f] to [x] to return a transformer. Useful for
      constructing a mutator that is only applied if an optional value has been
      given. *)
  val opt : 'a option -> ('a -> t) -> t

  (** [list v f] returns an identity transformer if [v] is the empty list, and
      if not returns a transformer with the semantics of {v:set_list} *)
  val list : 'a list -> ('a list -> t) -> t

  (** [apply t] returns a function that applies the transformation [t] to an
      {{val:OpamParserTypes.opamfile}opamfile} *)
  val apply : t -> OpamParserTypes.opamfile -> OpamParserTypes.opamfile
end

