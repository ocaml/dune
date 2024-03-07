(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2018 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Helper functions on the base types (from [OpamTypes]) *)

(** This module contains basic utility functions and stringifiers for the
    basic OPAM types present in OpamTypes.ml *)

open OpamParserTypes.FullPos
open OpamTypes

val string_of_std_path: std_path -> string
val std_path_of_string: string -> std_path
val all_std_paths: std_path list

(** Extract a package from a package action. *)
val action_contents: [< 'a action ] -> 'a list

val map_atomic_action: ('a -> 'b) -> 'a atomic_action -> 'b atomic_action
val map_highlevel_action: ('a -> 'b) -> 'a highlevel_action -> 'b highlevel_action
val map_concrete_action: ('a -> 'b) -> 'a concrete_action -> 'b concrete_action
val map_action: ('a -> 'b) -> 'a action -> 'b action

(** Extract a packages from a package action. This returns all concerned
    packages, including the old version for an up/down-grade. *)
val full_action_contents: 'a action -> 'a list

(** Pretty-prints the cause of an action *)
val string_of_cause: ('pkg -> string) -> 'pkg cause -> string

(** Pretty-print *)
val string_of_shell: shell -> string

(** The empty file position *)
val pos_null: pos
val nullify_pos : 'a -> 'a with_pos

(** [pos_best pos1 pos2] returns the most detailed position between [pos1] and
    [pos2] (defaulting to [pos1]) *)
val pos_best: pos -> pos -> pos

(** Position in the given file, with unspecified line and column *)
val pos_file: filename -> pos

(** Prints a file position *)
val string_of_pos: pos -> string

val string_of_user_action: user_action -> string

(** Makes sure to keep only the last binding for a given variable; doesn't
    preserve order *)
val env_array: env -> string array

(** Parses the data suitable for a filter.FIdent from a string. May raise
    [Failure msg] on bad package names. A self-reference [_] parses to [None] *)
val filter_ident_of_string:
  string -> name option list * variable * (string * string) option

val string_of_filter_ident:
  name option list * variable * (string * string) option -> string

val pkg_flag_of_string: string -> package_flag

val string_of_pkg_flag: package_flag -> string

val all_package_flags: package_flag list

(** Map on a solver result *)
val map_success: ('a -> 'b) -> ('a,'fail) result -> ('b,'fail) result
val iter_success: ('a -> unit) -> ('a, 'b) result -> unit
