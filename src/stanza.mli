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

module Of_sexp_helpers : sig
  open Sexp.Of_sexp

  (** Same as [record] for jbuild files and same as [fields] for dune
      ones. *)
  val inline_record : 'a fields_parser -> 'a t

  (** Same as [list] for jbuild files and same as [repeat] for dune
      files *)
  val inline_list : 'a t -> 'a list t

  (** Same as [enter] for jbuild files and a nop for dune files *)
  val inline_enter : 'a t -> 'a t
end
