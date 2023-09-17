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

(** Switch-related actions and changes *)

open OpamTypes
open OpamStateTypes

(** Initialises a new switch with the given name in the given opam root,
    registers it in the global config and returns the updated global state *)
val create_empty_switch:
  rw global_state ->
  ?synopsis:string -> ?repos:repository_name list -> ?invariant:formula ->
  switch -> rw global_state

(** Writes the current state file to disk (installed, pinned, root packages etc.).
    Unless [OpamStateConfig.(!r.dryrun)] *)
val write_selections: rw switch_state -> unit

(** Updates the global default switch to the one corresponding to the given
    state; fails and exits with a message if the switch is external *)
val set_current_switch: rw global_state -> 'a switch_state -> 'a switch_state

(** Create the default global_config structure for a switch, including default
    prefix *)
val gen_switch_config:
  dirname ->
  ?synopsis:string -> ?repos:repository_name list -> ?invariant:formula ->
  switch -> OpamFile.Switch_config.t

(** (Re-)install the configuration for a given root and switch *)
val install_switch_config: dirname -> switch -> OpamFile.Switch_config.t -> unit

(** Add the package metadata to the switch-local cache of installed packages *)
val install_metadata: rw switch_state -> package -> unit

(** Remove the metadata of the package from the switch-local cache of installed
    packages *)
val remove_metadata: rw switch_state -> package_set -> unit

(** Update the on-disk set of packages marked to reinstall on the current switch
    (excepting compiler packages, and pinned packages if [unpinned_only] is
    set) *)
val add_to_reinstall:
  rw switch_state -> unpinned_only:bool -> package_set -> rw switch_state

(** Updates the package selections and switch config to take into account the
    given newly installed package. The updated state is written to disk unless
    [OpamStateConfig.(!r.dry_run)] and returned. *)
val add_to_installed:
  rw switch_state -> ?root:bool -> package -> rw switch_state

(** Updates the package selections and switch config to take into account the
    removed package. The updated state is written to disk unless
    [OpamStateConfig.(!r.dry_run)], and returned. If [keep_as_root], the package
    isn't removed from the switch state [installed_roots] set. *)
val remove_from_installed:
  ?keep_as_root:bool -> rw switch_state -> package -> rw switch_state

(** Update the switch selections with the supplied optional arguments. Changes
    are written to disk and returned *)
val update_switch_state:
  ?installed: package_set ->
  ?installed_roots: package_set ->
  ?reinstall: package_set ->
  ?pinned: package_set ->
  rw switch_state -> rw switch_state
