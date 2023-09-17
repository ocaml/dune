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

(** Loading and handling of the global state of an opam root *)

open OpamTypes
open OpamStateTypes

type configuration_error

exception Configuration_error of configuration_error

val string_of_configuration_error : configuration_error -> string

(** Loads the global state (from the opam root obtained through
    [OpamStateConfig.(!r.root)]) *)
val load: 'a lock -> 'a global_state

(** Loads the global state as [load], and calls the given function while keeping
    it locked (as per the [lock] argument), releasing the lock afterwards *)
val with_: 'a lock -> ('a global_state -> 'b) -> 'b

(** The set of all installed packages, in any switch *)
val all_installed: 'a global_state -> package_set

val switches: 'a global_state -> switch list

(** Fold over switches, using switch selections. Switch selection file
   [switch-state] is loaded only read-only; no further checks are done on the opam root
   version. *)
val fold_switches:
  (switch -> switch_selections -> 'a -> 'a) -> 'b global_state -> 'a -> 'a

(** Checks a switch for existence: either configured in the opam root, or an existing
    local switch with a configuration file pointing to the current root *)
val switch_exists: 'a global_state -> switch -> bool

(** Returns the map of installed instances of the package name towards the list
    of switches they are installed in *)
val installed_versions: 'a global_state -> name -> switch list package_map

(** Default list of repositories to get packages from, ordered by decreasing
    priority. This can be overridden by switch-specific selections, and does not
    have to include all configured repositories. *)
val repos_list: 'a global_state -> repository_name list

(** Releases any locks on the given global_state *)
val unlock: 'a global_state -> unlocked global_state

(** Releases any locks on the given global state and then ignores it.

    Using [drop gt] is equivalent to [ignore (unlock gt)],
    and safer than other uses of [ignore]
    where it is not enforced by the type-system
    that the value is unlocked before it is lost.
*)
val drop: 'a global_state -> unit

(** Calls the provided function, ensuring a temporary write lock on the given
    global state *)
val with_write_lock:
  ?dontblock:bool -> 'a global_state ->
  (rw global_state -> 'b * 'c global_state) ->
  'b * 'a global_state

(** Writes back the global configuration file ~/.opam/config *)
val write: rw global_state -> unit

(** Updates the configured list of switches, making sure the current switch is
    registered if it is set and exists, and removing any non-existing switches.
    Writes back to disk if possible (ie lock is available) *)
val fix_switch_list: 'a global_state -> 'a global_state

(** Description used for system inferred variables *)
val inferred_from_system: string
