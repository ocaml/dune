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
  val to_dyn : t -> Dyn.t
  val test_example : unit -> t
end

type data = private ..

module type Desc_with_data = sig
  include Desc

  type data += T of t
end

(** Create a pair of functions to write/read a persistent value to/from a file.
    [D.name] must be unique.

    There's the [dune dump <file>] command that can pretty-print the contents of
    any persistent file. This command can use the [D.name] stored in the
    persistent file to locate the appropriate pretty printer. *)
module Make (D : Desc) : sig
  val to_string : D.t -> string
  val dump : Path.t -> D.t -> unit
  val load : Path.t -> D.t option

  type data += T of D.t
end

(** {1 Generic API} *)

(** The following functions allow to load an arbitrary persistent file without
    knowing its type. As long as this type of file has been registered via
    [Make], Dune will know how to read it. *)

type t = T : (module Desc with type t = 'a) * 'a -> t

val load_exn : Path.t -> t
val test_examples : unit -> t Seq.t
