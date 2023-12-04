(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2016 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Rsync repository backend, for local or ssh sources *)

module B: OpamRepositoryBackend.S

open OpamTypes

val rsync_dirs: ?args:string list -> ?exclude_vcdirs:bool ->
  OpamUrl.t -> OpamFilename.Dir.t ->
  OpamFilename.Dir.t download OpamProcess.job
val rsync_file: ?args:string list ->
  OpamUrl.t -> OpamFilename.t ->
  OpamFilename.t download OpamProcess.job
