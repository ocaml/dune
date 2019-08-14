(** Stanza in dune/jbuild files *)

open! Stdune

type t = ..

val latest_version : Syntax.Version.t

module Parser : sig
  (** Type of stanza parser.

    Each stanza in a configuration file might produce several values of type
    [t], hence the [t list] here. *)
  type nonrec t = string * t list Dune_lang.Decoder.t
end

(** Syntax identifier for the Dune language. [(0, X)] correspond to the Jbuild
  language while versions from [(1, 0)] correspond to the Dune one. *)
val syntax : Syntax.t

module File_kind : sig
  type t = Dune_lang.File_syntax.t =
    | Jbuild
    | Dune

  val of_syntax : Syntax.Version.t -> t
end

(** Whether we are parsing a [jbuild] or [dune] file. *)
val file_kind : unit -> (File_kind.t, _) Dune_lang.Decoder.parser
