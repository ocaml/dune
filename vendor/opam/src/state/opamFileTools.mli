(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Tools for manipulating and checking package definition ("opam") files *)

open OpamTypes

(** Create an OPAM package template filled with common options *)
val template: package -> OpamFile.OPAM.t

(** Utility function to print validation results *)
val warns_to_string: (int * [`Warning|`Error] * string) list -> string

(** Utility function to construct a json of validation results.
    The format is as follow:
    { "file"     : string <filename>,
      "result"   : string (passed | error | warning),
      "warnings" :
        [ { "id"      : int,
            "message" : string <warning message> },
          ...
        ],
      "errors"   :
        [ { "id"      : int,
            "message" : string <error message> },
          ...
        ]
    }
*)
val warns_to_json:
  ?filename:string -> (int * [`Warning|`Error] * string) list -> OpamJson.t

(** Read the opam metadata from a given directory (opam file, with possible
    overrides from url and descr files). Also includes the names and hashes
    of files below files/
    Warning: use [read_repo_opam] instead for correctly reading files from
    repositories!*)
val read_opam: dirname -> OpamFile.OPAM.t option

(** Like [read_opam], but additionally fills in the [metadata_dir] info
    correctly for the given repository. *)
val read_repo_opam:
  repo_name:repository_name -> repo_root:dirname ->
  dirname -> OpamFile.OPAM.t option

(** Adds data from 'url' and 'descr' files found in the specified dir or the
    opam file's metadata dir, if not already present in the opam file. if
    [files_subdir_hashes] is [true], also adds the names and hashes of files
    found below 'files/' *)
val add_aux_files:
  ?dir:dirname -> files_subdir_hashes:bool -> OpamFile.OPAM.t -> OpamFile.OPAM.t

(** {2 Tools to manipulate the [OpamFile.OPAM.t] contents} *)
val map_all_variables:
  (full_variable -> full_variable) -> OpamFile.OPAM.t -> OpamFile.OPAM.t

val map_all_filters:
  (filter -> filter) -> OpamFile.OPAM.t -> OpamFile.OPAM.t

(** Converts a dependency formula to the same format as used in opam package
    definition files. *)
val dep_formula_to_string: formula -> string

(** Sort opam fields: author, tags, depexts, depends, depopts, conflicts,
    pin_depends, extra_files, extra_sources *)
val sort_opam: OpamFile.OPAM.t -> OpamFile.OPAM.t
