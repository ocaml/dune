(** Implementation of versioned files *)

open! Stdune
module First_line = Versioned_file_first_line

module type S = sig
  type data

  module Lang : sig
    val register : Syntax.t -> data -> unit
    (** [register id data] registers a new language. Users will select this
        language by writing:

        {[ (lang <name> <version>) ]}

        as the first line of the versioned file. *)

    module Instance : sig
      type t =
        { syntax : Syntax.t
        ; data : data
        ; version : Syntax.Version.t
        }
    end

    val get_exn : string -> Instance.t
    (** Return the latest version of a language. *)
  end

  val load_exn : Path.t -> f:(Lang.Instance.t -> 'a Decoder.t) -> 'a
  (** [load_exn fn ~f] loads a versioned file. It parses the first line, looks
      up the language, checks that the version is supported and parses the rest
      of the file with [f]. *)

  val load : Path.t -> f:(Lang.Instance.t -> 'a Decoder.t) -> 'a Or_exn.t

  val parse_contents :
    Lexing.lexbuf -> f:(Lang.Instance.t -> 'a Decoder.t) -> 'a
  (** Parse the contents of a versioned file after the first line has been read. *)
end

module Make (Data : sig
  type t
end) : S with type data := Data.t

val no_more_lang : unit Decoder.fields_parser
(** Raise with an informative message when seeing a (lang ...) field. *)
