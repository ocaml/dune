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

(** Loading and querying a switch state *)

open OpamTypes
open OpamStateTypes

val load:
  'a lock -> 'b global_state -> 'c repos_state -> switch -> 'a switch_state

(** Loads the switch state and calls the given function on it, releasing the
    lock afterwards.

    The repository state is automatically loaded if not provided.

    The switch is selected, if not set, using [OpamStateConfig.get_switch] --
    which can fail if no switch is configured.

    Additionally, in case of a write lock, a backup is saved and a message is
    printed on restoring if [f] raised an exception and there were changes.  *)
val with_:
  'a lock -> ?rt:([< unlocked ] repos_state) -> ?switch:switch ->
  [< unlocked ] global_state ->
  ('a switch_state -> 'b) -> 'b

(** Creates a virtual state with nothing installed.
    Availability is computed just from the global state, and [avail_default]
    (default [true]) controls the result when the availability can't be computed
    due to undefined variables.
    Useful for querying and simulating actions when no switch is yet
    configured, or querying packages directly from the repos *)
val load_virtual:
  ?repos_list: repository_name list -> ?avail_default: bool ->
  'a global_state -> 'b repos_state -> unlocked switch_state

(** Load the switch's state file, without constructing the package maps: much
    faster than loading the full switch state *)
val load_selections:
  ?lock_kind: 'a lock -> 'b global_state -> switch -> switch_selections

(** Raw function to compute the availability of all packages, in [opams], given
    the switch configuration and the set of pinned packages. (The result is
    precomputed in global_state.available_packages once the state is loaded) *)
val compute_available_packages:
  'a global_state -> switch -> OpamFile.Switch_config.t ->
  pinned:package_set -> opams:OpamFile.OPAM.t package_map ->
  package_set

(** Raw function to compute the conflicts for all packages, given
    the set of available packages and the corresponding opam files.
    This is useful to populate the `u_conflicts` field when building
    a universe manually. *)
val get_conflicts_t:
  (package -> OpamFilter.env) -> package_set ->
  OpamFile.OPAM.t package_map -> formula package_map

(** Infer a switch invariant from a switch state with compiler_packages and
    roots set, using some heuristics. Useful for migration from pre-2.1 opam *)
val infer_switch_invariant: 'a switch_state -> OpamFormula.t

(** Releases any locks on the given switch_state *)
val unlock: 'a switch_state -> unlocked switch_state

(** Releases any locks on the given switch state and then ignores it.

    Using [drop st] is equivalent to [ignore (unlock st)],
    and safer than other uses of [ignore]
    where it is not enforced by the type-system
    that the value is unlocked before it is lost.
*)
val drop: 'a switch_state -> unit

(** Calls the provided function, ensuring a temporary write lock on the given
    switch state *)
val with_write_lock:
  ?dontblock:bool -> 'a switch_state ->
  (rw switch_state -> 'b * rw switch_state) ->
  'b * 'a switch_state

(** {2 Helpers to access state data} *)

(** Returns the repositories configured in the current switch or, if none, the
    globally set default. highest priority first. *)
val repos_list: 'a switch_state -> repository_name list

val selections: 'a switch_state -> switch_selections

(** Return the OPAM file for the given package.
    @raise Not_found when appropriate *)
val opam: 'a switch_state -> package -> OpamFile.OPAM.t

(** Return the OPAM file, including URL and descr, for the given package, if
    any *)
val opam_opt: 'a switch_state -> package -> OpamFile.OPAM.t option

(** Return the URL field for the given package *)
val url: 'a switch_state -> package -> OpamFile.URL.t option

(** Returns the primary URL from the URL field of the given package *)
val primary_url: 'a switch_state -> package -> url option

val primary_url_with_subpath: 'a switch_state -> package -> url option

(** Return the descr field for the given package (or an empty descr if none) *)
val descr: 'a switch_state -> package -> OpamFile.Descr.t

(** Return the descr field for the given package *)
val descr_opt: 'a switch_state -> package -> OpamFile.Descr.t option

(** Returns the full paths of overlay files under the files/ directory *)
val files: 'a switch_state -> package -> filename list

(** Return the installed package's local configuration *)
val package_config: 'a switch_state -> name -> OpamFile.Dot_config.t

(** Check whether a package name is installed *)
val is_name_installed: 'a switch_state -> name -> bool

(** Return the installed package with the right name
    @raise Not_found when appropriate *)
val find_installed_package_by_name: 'a switch_state -> name -> package

(** Return all packages satisfying one of the given atoms from a state *)
val packages_of_atoms: 'a switch_state -> atom list -> package_set

(** Gets the current version of package [name]: pinned version, installed
    version, max available version or max existing version, tried in this order.
    @raise Not_found only if there is no package by this name *)
val get_package: 'a switch_state -> name -> package

(** "dev packages" are any package with an upstream that isn't the usual HTTP,
    and without an archive checksum. These need to be updated from upstream
    independently when installed. It's generally only the case of source-pinned
    packages, but no rule enforces it in opam itself. *)
val is_dev_package: 'a switch_state -> package -> bool

(** Checks if the given package name is pinned *)
val is_pinned: 'a switch_state -> name -> bool

(** Checks if the given package is version-pinned, i.e. pinned without
    overlay metadata, and relying on the repo's data *)
val is_version_pinned: 'a switch_state -> name -> bool

(** The set of all "dev packages" (see [is_dev_package] for a definition) *)
val dev_packages: 'a switch_state -> package_set

(** Returns the local source mirror for the given package
    ([OpamPath.Switch.sources] or [OpamPath.Switch.pinned_package], depending on
    wether it's pinned). *)
val source_dir: 'a switch_state -> package -> dirname

(** Returns the set of active external dependencies for the package, computed
    from the values of the system-specific variables *)
val depexts: 'a switch_state -> package -> OpamSysPkg.Set.t

(* {2} Helpers to retrieve computed data *)

(** Return the transitive dependency closures
    of a collection of packages.

    [depopts]: include optional dependencies (depopts: foo)
    [build]: include build dependencies (depends: foo {build})
    [post]: include post dependencies (depends: foo {post})
    [installed]: only consider already-installed packages
    [unavaiable]: also consider unavailable packages
*)
val dependencies:
  'a switch_state -> build:bool -> post:bool -> depopts:bool ->
  installed:bool -> ?unavailable:bool -> package_set -> package_set

(** Same as [dependencies] but for reverse dependencies. *)
val reverse_dependencies:
  'a switch_state -> build:bool -> post:bool -> depopts:bool ->
  installed:bool -> ?unavailable:bool -> package_set -> package_set

(** Returns required system packages of each of the given packages (elements are
    not added to the map  if they don't have system dependencies) *)
val depexts_status_of_packages:
  'a switch_state -> package_set -> OpamSysPkg.status package_map

(** Returns not found depexts for the package *)
val depexts_unavailable: 'a switch_state -> package -> OpamSysPkg.Set.t option

(** [conflicts_with st subset pkgs] returns all packages declared in conflict
    with at least one element of [subset] within [pkgs], through forward or
    backward conflict definition or common conflict-class. Packages in [subset]
    (all their versions) are excluded from the result. *)
val conflicts_with: 'a switch_state -> package_set -> package_set -> package_set

(** Put the package data in a form suitable for the solver, pre-computing some
    maps and sets. Packages in the [requested] set are the ones that will get
    affected by the global [build_test] and [build_doc] flags. [test] and [doc],
    if unspecified, are taken from [OpamStateConfig.r]. [reinstall] marks
    package not considered current in the universe, and that should therefore be
    reinstalled. If unspecified, it is the packages marked in
    [switch_state.reinstall] that are present in [requested]. *)
val universe:
  'a switch_state ->
  ?test:bool ->
  ?doc:bool ->
  ?dev_setup:bool ->
  ?force_dev_deps:bool ->
  ?reinstall:package_set ->
  requested:package_set ->
  user_action -> universe

(** Dumps the current switch state in PEF format, for interaction with Dose
    tools *)
val dump_pef_state: 'a switch_state -> out_channel -> unit

(** {2 Updating} *)

(** Sets the given opam file for the given package, updating the other related
    fields along the way *)
val update_package_metadata:
  package -> OpamFile.OPAM.t -> 'a switch_state -> 'a switch_state

(** Removes the metadata associated to the given package, also updating the
    packages and available sets. *)
val remove_package_metadata: package -> 'a switch_state -> 'a switch_state

(** Like [update_package_metadata], but also ensures the package is pinned to
    the given version. The version specified in the opam file, if any, takes
    precedence over the version of [package]. Also marks it for reinstall if
    changed. *)
val update_pin: package -> OpamFile.OPAM.t -> 'a switch_state -> 'a switch_state

(** Updates the selected repositories in the given switch (does not load the
    full switch state, but takes a transient write lock on the switch, so make
    sure not to hold other locks to avoid deadlocks). Sets the switch
    repositories in any case, even if unchanged from the defaults. *)
val update_repositories:
  'a global_state -> (repository_name list -> repository_name list) ->
  switch -> unit

(** {2 Invariant computation} *)

(* Returns installed root packages of switch invariant *)
val invariant_root_packages: 'a switch_state -> package_set

(* Compute installed invariant dependency cone *)
val compute_invariant_packages: 'a switch_state -> package_set

(* Returns set of packages of installed compiler packages and their
   dependencies (only build & depopts) *)
val compiler_packages: 'a switch_state -> package_set

(** {2 User interaction and reporting } *)

(** Returns [true] if the switch of the state is the one set in
    [$OPAMROOT/config], [false] otherwise. This doesn't imply that the switch is
    current w.r.t. either the process or the shell, for that you need to check
    [OpamStateConfig.(!r.switch_from)] *)
val is_switch_globally_set: 'a switch_state -> bool

(** Returns a message about a package or version that couldn't be found *)
val not_found_message: 'a switch_state -> atom -> string

val unavailable_reason_raw:
  'a switch_state -> name * OpamFormula.version_formula ->
  [ `UnknownVersion
  | `UnknownPackage
  | `NoDefinition
  | `Pinned of OpamPackage.t
  | `Unavailable of string
  | `ConflictsBase
  | `ConflictsInvariant of string
  | `MissingDepexts of string list
  | `Default
  ]

(** Returns a printable explanation why a package is not currently available
    (pinned to an incompatible version, unmet [available:] constraints...).
    [default] is returned if no reason why it wouldn't be available was found
    (empty string if unspecified). *)
val unavailable_reason:
  'a switch_state -> ?default:string -> name * OpamFormula.version_formula ->
  string

(** Returns [true] when the package has the [avoid-version] flag and there isn't
    already a version with that flag installed (which disables the
    constraint) *)
val avoid_version : 'a switch_state -> package -> bool

(** Handle a cache of the opam files of installed packages *)
module Installed_cache: sig
  type t = OpamFile.OPAM.t OpamPackage.Map.t
  val save: OpamFilename.t -> t -> unit
  val remove: OpamFilename.t -> unit
end
