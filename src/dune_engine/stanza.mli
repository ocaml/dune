(** Stanza in dune/jbuild files *)

open! Stdune

type t = ..

val latest_version : Dune_lang.Syntax.Version.t

module Parser : sig
  (** Type of stanza parser.

      Each stanza in a configuration file might produce several values of type
      [t], hence the [t list] here. *)
  type nonrec t = string * t list Dune_lang.Decoder.t
end

(** Syntax identifier for the Dune language. [(0, X)] correspond to the Jbuild
    language while versions from [(1, 0)] correspond to the Dune one. *)
val syntax : Dune_lang.Syntax.t
