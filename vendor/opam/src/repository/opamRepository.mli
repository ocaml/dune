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

(** Operations on repositories (update, fetch...) based on the different
    backends implemented in separate modules *)

open OpamTypes

(* Same as [pull_shared_tree], but for a unique label/dirname. *)
val pull_tree:
  string -> ?cache_dir:dirname -> ?cache_urls:url list -> ?working_dir:bool ->
  ?subpath:subpath -> dirname -> OpamHash.t list -> url list ->
  string download OpamProcess.job

(** Same as [pull_tree], but for fetching a single file. *)
val pull_file:
  string -> ?cache_dir:dirname -> ?cache_urls:url list -> ?silent_hits:bool ->
  filename -> OpamHash.t list -> url list ->
  unit download OpamProcess.job
