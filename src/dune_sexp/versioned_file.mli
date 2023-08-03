(** Implementation of versioned files *)

open! Stdune
module First_line = Versioned_file_first_line

module type S = sig
  type data

  module Lang : sig
    (** [register id data] registers a new language. Users will select this
        language by writing:

        {[
          (lang <name> <version>)
        ]}

        as the first line of the versioned file. *)
    val register : Syntax.t -> data -> unit

    module Instance : sig
      type t =
        { syntax : Syntax.t
        ; data : data
        ; version : Syntax.Version.t
        }
    end

    (** Return the latest version of a language. *)
    val get_exn : string -> Instance.t
  end

  (** [load_exn fn ~f] loads a versioned file. It parses the first line, looks
      up the language, checks that the version is supported and parses the rest
      of the file with [f]. *)
  val load_exn : Path.t -> f:(Lang.Instance.t -> 'a Decoder.t) -> 'a

  val load : Path.t -> f:(Lang.Instance.t -> 'a Decoder.t) -> 'a Or_exn.t

  (** Parse the contents of a versioned file after the first line has been read. *)
  val parse_contents : Lexing.lexbuf -> f:(Lang.Instance.t -> 'a Decoder.t) -> 'a
end

module Make (Data : sig
    type t
  end) : S with type data := Data.t

(** Raise with an informative message when seeing a (lang ...) field. *)
val no_more_lang : unit Decoder.fields_parser
