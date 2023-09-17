(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Defines the types holding global, repository and switch states *)

open OpamTypes

(** Client state *)

(** Phantom types to indicate the locking state of a state, and allow or not
    on-disk operations.

    Note that each state load is itself locking enough to return a consistent
    state: a read lock is only needed when the consistency of the actions depend
    on the fact that the given state doesn't change during the run (e.g. an
    update that depends on it). In particular, all query commands don't need a
    read lock.

    Subtyping is by guarantees given on the operations allowed, [rw] giving the
    most and being the smallest type, so that it is safe to coerce
    [(rw t :> ro t)].
*)

(** Phantom type for readwrite-locked state (ensures that there are no
    concurrent reads or writes) *)
type rw = [ `Lock_write ]

(** Type for read-locked state (ensures that there are no concurrent writes) *)
type ro = [ `Lock_read | rw ]

(** Type for unlocked state (single file reads should still be ok) *)
type unlocked = [ `Lock_none | ro ]

(** The super-type for all lock types *)
type +'a lock = [< unlocked > `Lock_write ] as 'a

(** Type of global state global variables *)
type gt_variables =
  (variable_contents option Lazy.t * string) OpamVariable.Map.t

type gt_changes = { gtc_repo: bool; gtc_switch: bool }

(** Global state corresponding to an opam root and its configuration *)
type +'lock global_state = {
  global_lock: OpamSystem.lock;

  root: OpamPath.t;
  (** The global opam root path (caution: this is stored here but some code may
      rely on OpamStateConfig.root_dir ; in other words, multiple root handling
      isn't really supported at the moment) *)

  config: OpamFile.Config.t;
  (** The main configuration file. A note of caution: this corresponds to the
      configuration as loaded from the file: to get the current options, which
      may be overridden through the command-line or environment, see
      OpamStateConfig *)

  global_variables: gt_variables;
  (** A map of variables that have been defined globally, e.g. through
      `.opam/config`. They may need evaluation so are stored as lazy values.
      The extra string is the supplied variable documentation *)

  global_state_to_upgrade: gt_changes;
  (** If the global config was upgraded on-the-fly, indicates if the either the repo or switch config
    require the global config to be written (i.e. a hard upgrade to the global config) *)

} constraint 'lock = 'lock lock

(** State corresponding to the repo/ subdir: all available packages and
    metadata, for each repository. *)
type +'lock repos_state = {
  repos_lock: OpamSystem.lock;

  repos_global: unlocked global_state;

  repositories: repository repository_name_map;
  (** The list of repositories *)

  repos_definitions: OpamFile.Repo.t repository_name_map;
  (** The contents of each repo's [repo] file *)

  repo_opams: OpamFile.OPAM.t package_map repository_name_map;
  (** All opam files that can be found in the configured repositories *)

  repos_tmp: (OpamRepositoryName.t, OpamFilename.Dir.t Lazy.t) Hashtbl.t;
  (** Temporary directories containing the uncompressed contents of the
      repositories *)
} constraint 'lock = 'lock lock


(** State of a given switch: options, available and installed packages, etc.*)
type +'lock switch_state = {
  switch_lock: OpamSystem.lock;

  switch_global: unlocked global_state;

  switch_repos: unlocked repos_state;

  switch: switch;
  (** The current active switch *)

  switch_invariant: formula;
  (** Defines the "base" of the switch, e.g. what compiler is desired *)

  compiler_packages: package_set;
  (** The packages that form the base of the current compiler. Normally equal to
      the subset of installed packages matching the invariant defined in
      switch_config *)

  switch_config: OpamFile.Switch_config.t;
  (** The configuration file for this switch *)

  repos_package_index: OpamFile.OPAM.t package_map;
  (** Metadata of all packages that could be found in the configured
      repositories (ignoring installed or pinned packages) *)

  opams: OpamFile.OPAM.t package_map;
  (** The metadata of all packages, gathered from repo, local cache and pinning
      overlays. This includes URL and descr data (even if they were originally
      in separate files), as well as the original metadata directory (that can
      be used to retrieve the files/ subdir) *)

  conf_files: OpamFile.Dot_config.t name_map;
  (** The opam-config of installed packages (from
      ".opam-switch/config/pkgname.config") *)

  packages: package_set;
  (** The set of all known packages *)

  sys_packages: sys_pkg_status package_map Lazy.t;
  (** Map of package and their system dependencies packages status. Only
      initialised for otherwise available packages *)

  available_packages: package_set Lazy.t;
  (** The set of available packages, filtered by their [available:] field *)

  pinned: package_set;
  (** The set of pinned packages (their metadata, including pinning target, is
      in [opams]) *)

  installed: package_set;
  (** The set of all installed packages *)

  installed_opams: OpamFile.OPAM.t package_map;
  (** The cached metadata of installed packages (may differ from the metadata
      that is in [opams] for updated packages) *)

  installed_roots: package_set;
  (** The set of packages explicitly installed by the user. Some of them may
      happen not to be installed at some point, but this indicates that the
      user would like them installed. *)

  reinstall: package_set Lazy.t;
  (** The set of packages which need to be reinstalled *)

  invalidated: package_set Lazy.t;
  (** The set of packages which are installed but no longer valid, e.g. because
      of removed system dependencies. Only packages which are unavailable end up
      in this set, they are otherwise put in [reinstall]. *)

  (* Missing: a cache for
     - switch-global and package variables
     - the solver universe? *)
} constraint 'lock = 'lock lock

(** Command-line setting provenance *)
type provenance = [ `Env          (** Environment variable *)
                  | `Command_line (** Command line *)
                  | `Default      (** Default value *)
                  ]

(** Pinned opam files informations *)

(* Opam file to pin informations.
   [_topin_opamfile] and _topin_name_and_opamfile] are not meant to be used
   directly ; use rather below defined types ;*)
type 'url _topin_opamfile = {
  pin_file: OpamFile.OPAM.t OpamFile.t;
  pin_locked: string option;
  pin_subpath: subpath option;
  pin_url: 'url;
}
type ('name, 'url) _topin_name_and_opamfile = {
  pin_name: 'name;
  pin: 'url _topin_opamfile;
}
type name_and_file = (name, unit) _topin_name_and_opamfile
type name_and_file_w_url = (name, url) _topin_name_and_opamfile
type nameopt_and_file = (name option, unit) _topin_name_and_opamfile
type nameopt_and_file_w_url = (name option, url) _topin_name_and_opamfile

(* Pinned package informations *)
type pinned_opam = {
  pinned_name : name;
  pinned_version : version option;
  pinned_opam : OpamFile.OPAM.t option;
  pinned_subpath: subpath option;
  pinned_url: url;
}
