(** Stanza in dune/jbuild files *)

open Stdune

type t = ..

module Parser : sig
  (** Type of stanza parser.

      Each stanza in a configuration file might produce several values
      of type [t], hence the [t list] here. *)
  type nonrec t = string * t list Sexp.Of_sexp.t
end

(** Syntax identifier for the Dune language. [(0, X)] correspond to
    the Jbuild language while versions from [(1, 0)] correspond to the
    Dune one. *)
val syntax : Syntax.t

module File_kind : sig
  type t = Jbuild | Dune
end

(** Whether we are parsing a [jbuild] or [dune] file. *)
val file_kind : unit -> (File_kind.t, _) Sexp.Of_sexp.parser

(** Overlay for [Sexp.Of_sexp] where lists and records don't require
    an extra level of parentheses in Dune files. *)
module Of_sexp : sig
  include module type of struct include Sexp.Of_sexp end

  val record : 'a fields_parser -> 'a t
  val list : 'a t -> 'a list t
end
