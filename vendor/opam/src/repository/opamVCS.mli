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

(** Layer for handling version control sources through a functor *)

open OpamTypes

(** Each backend should implement this signature. *)
module type VCS = sig

  val name: OpamUrl.backend

  (** Test whether the given repository is correctly initialized. *)
  val exists: dirname -> bool

  (** Init a repository. *)
  val init: dirname -> url -> unit OpamProcess.job

  (** Fetch changes from upstream. This is supposed to put the changes
      in a staging area.
      Be aware that the remote URL might have been changed, so make sure
      to update accordingly. *)
  val fetch:
    ?cache_dir:dirname -> ?subpath:subpath -> dirname -> url ->
    unit OpamProcess.job

  (** Reset the master branch of the repository to match the remote repository
      state. This might still fetch more data (git submodules...), so is
      unsuitable for running after validation. *)
  val reset_tree: dirname -> url -> unit OpamProcess.job

  (** Confirm that applying the patch results in a clean synchronization of
      the working tree with its repository state. *)
  val patch_applied: dirname -> url -> unit OpamProcess.job

  (** Returns the pending modifications in the form of a patch file, or None if
      [dirname] is up to date with what was last fetched. *)
  val diff: dirname -> url -> filename option OpamProcess.job

  (** Returns true if the last fetched state is equal to the current, on-disk
      state *)
  val is_up_to_date: ?subpath:subpath -> dirname -> url -> bool OpamProcess.job

  (** Returns an backend-specific identifier for the current revision. *)
  val revision: dirname -> string option OpamProcess.job

  (** Returns the list of files under version control *)
  val versioned_files: dirname -> string list OpamProcess.job

  (** Returns the absolute directory name for vc data (e.g.
      [.../project/.git]) *)
  val vc_dir: dirname -> dirname

  (** Returns the currently selected branch handle. It should be valid as the
      [hash] field of [OpamUrl.t]. *)
  val current_branch: dirname -> string option OpamProcess.job

  (** Returns true if the working tree state is different from the state
      recorded in the VCS as current. This differs from [is_up_to_date], which
      compares specifically to the last fetched state. This should always be
      [false] after [reset] has been called. *)
  val is_dirty: ?subpath:subpath -> dirname -> bool OpamProcess.job

  (** Returns the list of files under version control, modified in the working
      tree but not committed *)
  val modified_files: dirname -> string list OpamProcess.job

  (* Returns associated remote url, if found *)
  val get_remote_url: ?hash:string -> dirname -> url option OpamProcess.job

  (* Remove uncommitted changes *)
  val clean: dirname -> unit OpamProcess.job
end

(** Create a backend from a [VCS] implementation. *)
module Make(_ : VCS) : OpamRepositoryBackend.S
