(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Configuration options for the state lib (record, global reference, setter,
    initialisation) *)

open OpamTypes
open OpamStateTypes

module E : sig
  type OpamStd.Config.E.t +=
    | BUILDDOC of bool option
    | BUILDTEST of bool option
    | DOWNLOADJOBS of int option
    | DRYRUN of bool option
    | IGNORECONSTRAINTS of string option
    | JOBS of int option
    | LOCKED of string option
    | MAKECMD of string option
    | NODEPEXTS of bool option
    | NOENVNOTICE of bool option
    | ROOT of string option
    | SWITCH of string option
    | UNLOCKBASE of bool option
    | WITHDEVSETUP of bool option
    | WITHDOC of bool option
    | WITHTEST of bool option
  val root: unit -> string option
  val switch: unit -> string option
end

type t = private {
  root_dir: OpamFilename.Dir.t;
  current_switch: OpamSwitch.t option;
  switch_from: provenance;
  jobs: int Lazy.t;
  dl_jobs: int;
  build_test: bool;
  build_doc: bool;
  dev_setup: bool;
  dryrun: bool;
  makecmd: string Lazy.t;
  ignore_constraints_on: name_set;
  unlock_base: bool;
  no_env_notice: bool;
  locked: string option;
  no_depexts : bool;
}

type 'a options_fun =
  ?root_dir:OpamFilename.Dir.t ->
  ?current_switch:OpamSwitch.t ->
  ?switch_from:provenance ->
  ?jobs:(int Lazy.t) ->
  ?dl_jobs:int ->
  ?build_test:bool ->
  ?build_doc:bool ->
  ?dev_setup:bool ->
  ?dryrun:bool ->
  ?makecmd:string Lazy.t ->
  ?ignore_constraints_on:name_set ->
  ?unlock_base:bool ->
  ?no_env_notice:bool ->
  ?locked:string option ->
  ?no_depexts: bool ->
  'a

include OpamStd.Config.Sig
  with type t := t
   and type 'a options_fun := 'a options_fun

(** Default state values *)
val default : t

(** Get the initial opam root value (from default, env or optional argument).
    This allows one to get it before doing the init, which is useful to get the
    configuration file used to fill some options to init() *)
val opamroot: ?root_dir:dirname -> unit -> dirname

(** Loads the global configuration file, protecting against concurrent writes *)
val load: ?lock_kind: 'a lock -> dirname -> OpamFile.Config.t option
val safe_load: ?lock_kind: 'a lock -> dirname -> OpamFile.Config.t

(** Loads the config file from the OPAM root and updates default values for all
    related OpamXxxConfig modules. Doesn't read the env yet, the [init]
    functions should still be called afterwards. OpamFormat should be
    initialised beforehand, as it may impact the config file loading.

    Returns the config file that was found, if any *)
val load_defaults:
  ?lock_kind:'a lock -> OpamFilename.Dir.t -> OpamFile.Config.t option

(** Returns the current switch, failing with an error message is none is set. *)
val get_switch: unit -> switch

(** Returns the current switch, if any is set. *)
val get_switch_opt: unit -> switch option

(** The function used to locate an external switch from parents of the current
    directory. Takes the opam root as parameter, and rejects any external switch
    configured with a different root *)
val get_current_switch_from_cwd: OpamFilename.Dir.t -> switch option

(** Checks if a local switch exists and is configurade for the given root *)
val local_switch_exists: OpamFilename.Dir.t -> switch -> bool

(** Resolves the switch if it is a link to a global switch in the given root
    (return unchanged otherwise) *)
val resolve_local_switch: OpamFilename.Dir.t -> switch -> switch

(** Given the required lock, returns [true] if the opam root is newer than the
    binary, so that it can only be loaded read-only by the current binary. *)
val is_newer_than_self: ?lock_kind:'a lock -> 'b global_state -> bool

(** Check config root version regarding self-defined one *)
val is_newer: OpamFile.Config.t ->  bool

val load_config_root:
  ?lock_kind:'a lock ->
  ((OpamFile.Config.t OpamFile.t -> 'b) * (OpamFile.Config.t OpamFile.t -> 'b)) ->
  dirname -> 'b

module Switch : sig
  val safe_load_t:
    ?lock_kind: 'a lock -> dirname -> switch -> OpamFile.Switch_config.t
  val safe_load:
    ?lock_kind: 'a lock -> 'b global_state -> switch -> OpamFile.Switch_config.t
  val safe_read_selections:
    ?lock_kind: 'a lock -> 'b global_state -> switch -> switch_selections
  val read_opt:
    ?lock_kind: 'a lock -> 'b global_state -> switch ->
    OpamFile.Switch_config.t option
end

module Repos : sig
  val safe_read:
    ?lock_kind: 'a lock -> 'b global_state -> OpamFile.Repos_config.t
end

(* Raw read an switch config to downgrade its [opam-version] from 2.1 to 2.0.
   It is necessary to handle opam root and switch upgrade from 2.1
   intermediates roots to 2.1: this allows a workaround for a bug in versions
   2.1~alpha which wrongly updated the declared switch versions, requiring that
   we fix it during [OpamFormatUpgrade] from these specific intermediate
   versions, and at switch loading for that specific case. *)
val downgrade_2_1_switch:
  OpamFile.Switch_config.t OpamFile.t -> OpamFile.Switch_config.t option
