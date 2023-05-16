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

(** loading and handling of the repository state of an opam root (i.e. what is
    in ~/.opam/repo) *)

open OpamTypes

open OpamStateTypes

(** Caching of repository loading (marshall of all parsed opam files) *)
module Cache: sig
  val save: [< rw] repos_state -> unit
  val load:
    dirname ->
    (OpamFile.Repo.t repository_name_map *
     OpamFile.OPAM.t package_map repository_name_map)
      option
  val remove: unit -> unit
end

val load: 'a lock -> [< unlocked ] global_state -> 'a repos_state

(** Loads the repository state as [load], and calls the given function while
    keeping it locked (as per the [lock] argument), releasing the lock
    afterwards *)
val with_:
  'a lock -> [< unlocked ] global_state -> ('a repos_state -> 'b) -> 'b

(** Returns the repo of origin and metadata corresponding to a package, if
    found, from a sorted list of repositories (highest priority first) *)
val find_package_opt: 'a repos_state -> repository_name list -> package ->
  (repository_name * OpamFile.OPAM.t) option

(** Given the repos state, and a list of repos to use (highest priority first),
    build a map of all existing package definitions *)
val build_index:
  'a repos_state -> repository_name list -> OpamFile.OPAM.t OpamPackage.Map.t

(** Finds a package repository definition from its name (assuming it's in
    ROOT/repos/) *)
val get_repo: 'a repos_state -> repository_name -> repository

val load_opams_from_dir: repository_name -> dirname -> OpamFile.OPAM.t OpamPackage.Map.t

(** Load all the metadata within the local mirror of the given repository,
    without cache *)
val load_repo:
  repository -> OpamFilename.Dir.t ->
  OpamFile.Repo.t * OpamFile.OPAM.t OpamPackage.Map.t

(** Get the (lazily extracted) repository root for the given repository *)
val get_root: 'a repos_state -> repository_name -> OpamFilename.Dir.t

(** Same as [get_root], but with a repository rather than just a name as argument *)
val get_repo_root: 'a repos_state -> repository -> OpamFilename.Dir.t

(* (\** Runs the given function with access to a (possibly temporary) directory
 *     containing the extracted structure of the given repository, and cleans it up
 *     afterwards if temporary. The basename of the directory is guaranteed to
 *     match the repository name (this is important for e.g. [tar]) *\)
 * val with_repo_root:
 *   'a global_state -> repository -> (OpamFilename.Dir.t -> 'b) -> 'b
 * 
 * (\** As [with_repo_root], but on jobs *\)
 * val with_repo_root_job:
 *   'a global_state -> repository ->
 *   (OpamFilename.Dir.t -> 'b OpamProcess.job) -> 'b OpamProcess.job *)

(** Releases any locks on the given repos_state, and cleans the tmp extracted
    tree if any unless [cleanup=false] *)
val unlock: ?cleanup:bool -> 'a repos_state -> unlocked repos_state

(** Releases any locks on the given repos_state and then ignores it.

    Using [drop ?cleanup rt] is equivalent to [ignore (unlock ?cleanup rt)],
    and safer than other uses of [ignore]
    where it is not enforced by the type-system
    that the value is unlocked before it is lost.
*)
val drop: ?cleanup:bool -> 'a repos_state -> unit

(** Cleanup before removing the repository from temporary table *)
val remove_from_repos_tmp: 'a repos_state -> repository_name -> unit

(** Clears tmp files corresponding to a repo state (uncompressed repository
    contents) *)
val cleanup: 'a repos_state -> unit

(** Calls the provided function, ensuring a temporary write lock on the given
    repository state*)
val with_write_lock:
  ?dontblock:bool -> 'a repos_state -> (rw repos_state -> 'b * rw repos_state)
  -> 'b * 'a repos_state

(** Writes the repositories config file back to disk *)
val write_config: rw repos_state -> unit

(** Display a warning if repository has not been updated since 3 weeks *)
val check_last_update: unit -> unit
