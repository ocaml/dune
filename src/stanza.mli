(** Stanza in dune/jbuild files *)

open Stdune

type t = ..

module Parser : sig
  (** Type of stanza parser.

      Each stanza in a configuration file might produce several values
      of type [t], hence the [t list] here. *)
  type nonrec t = string * t list Sexp.Of_sexp.t
end
