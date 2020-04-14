(** Response file support *)

open Stdune

type t =
  | Not_supported
  | Zero_terminated_strings of string
      (** The argument is the command line flag, such as "-args0" *)

val get : prog:Path.t -> t
(** Return whether [prog] supports a response file or not *)

val set : prog:Path.t -> t -> unit
(** Registers the fact that [prog] supports a response file *)
