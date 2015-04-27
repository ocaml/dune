(* $Id$ *)

(** Utilities for loading dynamically packages *)

val load_packages : string list -> unit
(** Load the given packages and all their dependencies dynamically. Packages
    already loaded or already in-core are not loaded again.
 *)
