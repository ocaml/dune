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

(** Process environment setup and handling, shell configuration *)

open OpamTypes
open OpamStateTypes

(** {2 Environment handling} *)

(** Get the current environment with OPAM specific additions. If [force_path],
    the PATH is modified to ensure opam dirs are leading. [set_opamroot] and
    [set_opamswitch] can be additionally used to set the [OPAMROOT] and
    [OPAMSWITCH] variables. [scrub] is a list of environment variables to
    remove from the environment. *)
val get_full:
  set_opamroot:bool -> set_opamswitch:bool -> force_path:bool ->
  ?updates:env_update list -> ?scrub:string list -> 'a switch_state -> env

(** Get only environment modified by OPAM. If [force_path], the PATH is modified
    to ensure opam dirs are leading. [set_opamroot] and [set_opamswitch] can be
    additionally used to set the [OPAMROOT] and [OPAMSWITCH] variables.

    With [base], apply the modifications to the specified base environment *)
val get_opam:
  set_opamroot:bool -> set_opamswitch:bool -> force_path:bool ->
  'a switch_state -> env

(** Like [get_opam], but reads the cache file from the given opam root and
    switch instead of computing the environment from a switch state.

    With [base], apply the modifications to the specified base environment *)
val get_opam_raw:
  set_opamroot:bool -> set_opamswitch:bool -> ?base:env ->
  force_path:bool ->
  dirname -> switch -> env

(** Returns the running environment, with any opam modifications cleaned out,
    and optionally the given updates *)
val get_pure: ?updates:env_update list -> unit -> env

(** Update an environment, including reverting opam changes that could have been
    previously applied (therefore, don't apply to an already updated env as
    returned by e.g. [get_full]!) *)
val add: env -> env_update list -> env

(** Like [get_opam] computes environment modification by OPAM , but returns
    these [updates] instead of the new environment. *)
val updates:
  set_opamroot:bool -> set_opamswitch:bool -> ?force_path:bool ->
  'a switch_state -> env_update list

(** Check if the shell environment is in sync with the current OPAM switch,
    unless [skip] is true (it's default value is OPAMNOENVNOTICE *)
val is_up_to_date: ?skip:bool -> 'a switch_state -> bool

(** Check if the shell environment is in sync with the given opam root and
    switch (or if OPAMNOENVNOTICE has been set, in which case we just assume
    it's up to date) *)
val is_up_to_date_switch: dirname -> switch -> bool

(** Returns the current environment updates to configure the current switch with
    its set of installed packages *)
val compute_updates: ?force_path:bool -> 'a switch_state -> env_update list

(** Returns shell-appropriate statement to evaluate [cmd]. *)
val shell_eval_invocation:
  OpamTypes.shell -> string -> string

(** Returns "opam env" invocation string together with optional root and switch
    overrides *)
val opam_env_invocation:
  ?root:string -> ?switch:string -> ?set_opamswitch:bool -> OpamTypes.shell -> string

(** The shell command to run by the user to set his OPAM environment, adapted to
    the current shell (as returned by [eval `opam config env`]) *)
val eval_string:
  'a global_state -> ?set_opamswitch:bool -> switch option -> string

(** Returns the updated contents of the PATH variable for the given opam root
    and switch (set [force_path] to ensure the opam path is leading) *)
val path: force_path:bool -> dirname -> switch -> string

(** Returns the full environment with only the PATH variable updated, as per
    [path] *)
val full_with_path:
  force_path:bool -> ?updates:env_update list -> dirname -> switch -> env

(** Performs variable expansion on the strings in an environment update *)
val env_expansion: ?opam:OpamFile.OPAM.t -> 'a switch_state -> env_update -> env_update

(** {2 Shell and initialisation support} *)

(** Sets the opam configuration in the user shell, after detailing the process
    and asking the user if either [update_config] or [shell_hook] are unset *)
val setup:
  dirname -> interactive:bool -> ?dot_profile:filename ->
  ?update_config:bool -> ?env_hook:bool -> ?completion:bool -> ?inplace:bool ->
  shell -> unit

(* (\** Display the global and user configuration for OPAM. *\)
 * val display_setup: dirname -> dot_profile:filename -> shell -> unit *)

(** Update the user configuration in $HOME for good opam integration. *)
val update_user_setup:
  dirname -> ?dot_profile:filename -> shell -> unit

(** Write the generic scripts in ~/.opam/opam-init needed to import state for
    various shells. If specified, completion and env_hook files can also be
    written or removed (the default is to keep them as they are). If [inplace]
    is true, they are updated if they exist. *)
val write_static_init_scripts:
  dirname -> ?completion:bool -> ?env_hook:bool -> ?inplace:bool -> unit -> unit

(** Write into [OpamPath.hooks_dir] the given custom scripts (listed as
    (filename, content)), normally provided by opamrc ([OpamFile.InitConfig]) *)
val write_custom_init_scripts:
  dirname -> (string * string) list -> unit

(** Update the shell scripts containing the current switch configuration in
    ~/.opam/opam-init ; prints a warning and skips if a write lock on the global
    state can't be acquired (note: it would be better to acquire a write lock
    beforehand, but only when working on the switch selected in
    ~/.opam/config) *)
val write_dynamic_init_scripts: 'a switch_state -> unit

(** Removes the dynamic init scripts setting the variables for any given
    switch. *)
val clear_dynamic_init_scripts: rw global_state -> unit

(** Print a warning if the environment is not set-up properly.
    (General message) *)
val check_and_print_env_warning: 'a switch_state -> unit

(** Hook directory environment *)
val hook_env:
  OpamPath.t -> OpamVariable.variable_contents option OpamVariable.Map.t
