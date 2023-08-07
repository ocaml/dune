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

open OpamTypes
open OpamStd.Op
open OpamFilename.Op

open OpamStateTypes

let log fmt = OpamConsole.log "GSTATE" fmt
let slog = OpamConsole.slog

type configuration_error = string

exception Configuration_error of configuration_error

let string_of_configuration_error configuration_error = configuration_error

let load_config lock_kind global_lock root =
  let config =
    match OpamStateConfig.load ~lock_kind root with
    | Some c -> c
    | exception (OpamPp.Bad_version _ as e) ->
      OpamFormatUpgrade.hard_upgrade_from_2_1_intermediates ~global_lock root;
      raise e
    | None ->
      if OpamFilename.exists (root // "aliases") then
        OpamFile.Config.(with_opam_version (OpamVersion.of_string "1.1") empty)
      else
        raise (Configuration_error (Printf.sprintf
          "%s exists, but does not appear to be a valid opam root. Please \
           remove it and use `opam init', or specify a different `--root' \
           argument"
          (OpamFilename.Dir.to_string root)))
  in
  let config =
    OpamFormatUpgrade.as_necessary lock_kind global_lock root config
  in
  OpamStd.Option.iter
    (fun cygbin ->
       OpamCoreConfig.update ~cygbin:(OpamFilename.Dir.to_string cygbin) ())
    (OpamSysInteract.Cygwin.cygbin_opt (fst config));
  config

let inferred_from_system = "Inferred from system"

let load lock_kind =
  let root = Lazy.force OpamStateConfig.(!r.root_dir) in
  log "LOAD-GLOBAL-STATE %@ %a" (slog OpamFilename.Dir.to_string) root;
  (* Always take a global read lock, this is only used to prevent concurrent
     ~/.opam format changes *)
  let has_root = OpamFilename.exists_dir root in
  let global_lock =
    if has_root then
      OpamFilename.flock `Lock_read (OpamPath.lock root)
    else OpamSystem.lock_none
  in
  (* The global_state lock actually concerns the global config file only (and
     the consistence thereof with the repository and switch sets, and the
     currently installed shell init scripts) *)
  if not has_root then
    raise (Configuration_error
      "Opam has not been initialised, please run `opam init'");
  let config_lock = OpamFilename.flock lock_kind (OpamPath.config_lock root) in
  let config, global_state_to_upgrade =
    try load_config lock_kind global_lock root
    with OpamFormatUpgrade.Upgrade_done _ as e ->
      OpamSystem.funlock config_lock;
      raise e
  in
  if OpamStateConfig.is_newer config && lock_kind <> `Lock_write then
    log "root version (%s) is greater than running binary's (%s); \
         load with best-effort (read-only)"
      (OpamVersion.to_string (OpamFile.Config.opam_root_version config))
      (OpamVersion.to_string (OpamFile.Config.root_version));
  let switches =
    List.filter
      (fun sw -> not (OpamSwitch.is_external sw) ||
                 OpamFilename.exists_dir (OpamSwitch.get_root root sw))
      (OpamFile.Config.installed_switches config)
  in
  let config = OpamFile.Config.with_installed_switches switches config in
  let global_variables =
    List.fold_left (fun acc (v,value) ->
        OpamVariable.Map.add v
          (lazy (Some (OpamStd.Option.default (S "unknown") (Lazy.force value))),
           (* Careful on changing it, it is used to determine user defined
              variables on `config report`. See [OpamConfigCommand.help]. *)
           inferred_from_system)
          acc)
      OpamVariable.Map.empty
      (OpamSysPoll.variables)
  in
  let global_variables =
    List.fold_left (fun acc (v,value,doc) ->
        OpamVariable.Map.add v (lazy (Some value), doc) acc)
      global_variables
      (OpamFile.Config.global_variables config)
  in
  let eval_variables = OpamFile.Config.eval_variables config in
  let global_variables =
    let env = lazy (OpamEnv.get_pure () |> OpamTypesBase.env_array) in
    List.fold_left (fun acc (v, cmd, doc) ->
        OpamVariable.Map.update v
          (fun previous_value ->
             (lazy
               (try
                  let ret =
                    OpamSystem.read_command_output
                      ~env:(Lazy.force env)
                      ~allow_stdin:false
                      cmd
                  in
                  Some (S (OpamStd.String.strip (String.concat "\n" ret)))
                with e ->
                  OpamStd.Exn.fatal e;
                  log "Failed to evaluate global variable %a: %a"
                    (slog OpamVariable.to_string) v
                    (slog Printexc.to_string) e;
                  Lazy.force (fst previous_value))),
             doc)
          (lazy None, "")
          acc)
      global_variables eval_variables
  in
  { global_lock = config_lock;
    root;
    config;
    global_variables;
    global_state_to_upgrade;
    }

let switches gt =
  OpamFile.Config.installed_switches gt.config

let fold_switches f gt acc =
  List.fold_left (fun acc switch ->
      f switch
        (OpamStateConfig.Switch.safe_read_selections
           ~lock_kind:`Lock_read gt switch)
        acc
    ) acc (OpamFile.Config.installed_switches gt.config)

let switch_exists gt switch =
  if OpamSwitch.is_external switch then
    OpamStateConfig.local_switch_exists gt.root switch
  else List.mem switch (switches gt)

let all_installed gt =
  fold_switches (fun _ sel acc ->
      OpamPackage.Set.union acc sel.sel_installed)
    gt  OpamPackage.Set.empty

let installed_versions gt name =
  fold_switches (fun switch sel acc ->
      let installed =
        OpamPackage.packages_of_name sel.sel_installed name
      in
      try
        let nv = OpamPackage.Set.choose installed in
        try OpamPackage.Map.add nv (switch::OpamPackage.Map.find nv acc) acc
        with Not_found -> OpamPackage.Map.add nv [switch] acc
      with Not_found -> acc)
    gt OpamPackage.Map.empty

let repos_list gt = OpamFile.Config.repositories gt.config

let unlock gt =
  OpamSystem.funlock gt.global_lock;
  (gt :> unlocked global_state)

let drop gt =
  let _ = unlock gt in ()

let with_write_lock ?dontblock gt f =
  if OpamStateConfig.is_newer_than_self gt then
    OpamConsole.error_and_exit `Locked
      "The opam root has been upgraded by a newer version of opam-state \
       and cannot be written to";
  let ret, gt =
    OpamFilename.with_flock_upgrade `Lock_write ?dontblock gt.global_lock
    @@ fun _ -> f ({ gt with global_lock = gt.global_lock } : rw global_state)
    (* We don't actually change the field value, but this makes restricting the
       phantom lock type possible*)
  in
  ret, { gt with global_lock = gt.global_lock }

let with_ lock f =
  let gt = load lock in
  try let r = f gt in drop gt; r
  with e -> OpamStd.Exn.finalise e (fun () -> drop gt)

let write gt =
  OpamFile.Config.write (OpamPath.config gt.root) gt.config

let fix_switch_list gt =
  let known_switches0 = switches gt in
  let known_switches =
    match OpamStateConfig.get_switch_opt () with
    | None -> known_switches0
    | Some sw ->
      if List.mem sw known_switches0 || not (switch_exists gt sw)
      then known_switches0
      else sw::known_switches0
  in
  let known_switches = List.filter (switch_exists gt) known_switches in
  if known_switches = known_switches0 then gt else
  let config =
    OpamFile.Config.with_installed_switches known_switches gt.config
  in
  let gt = { gt with config } in
  if not OpamCoreConfig.(!r.safe_mode)
  && OpamSystem.get_lock_flag gt.global_lock = `Lock_write then
    try
      snd @@ with_write_lock ~dontblock:true gt @@ fun gt ->
      write gt, gt
    with OpamSystem.Locked -> gt
  else gt
