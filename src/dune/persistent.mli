(** Persistent values *)

(** This module allows to store values on disk so that they persist after Dune
    has exited and can be re-used by the next run of Dune.

    Values are simply marshaled using the [Marshal] module and manually
    versioned. As such, it is important to remember to increase the version
    number when the type of persistent values changes.

    In the future, we hope to introduce a better mechanism so that persistent
    values are automatically versioned. *)

open Stdune

module type Desc = sig
  type t

  val name : string

  val version : int
end

(** Create a pair of functions to write/read a persistent value to/from a file.
    [D.name] must be unique.

    In the future, we plan to add a command [dune dump <file>] that will
    pretty-print the contents of any persistent file. This command will use the
    [D.name] stored in the persistent file to locate the appropriate pretty
    printer. *)
module Make (D : Desc) : sig
  val dump : Path.t -> D.t -> unit

  val load : Path.t -> D.t option
end
