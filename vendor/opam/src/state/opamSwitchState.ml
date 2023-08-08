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

open OpamTypes
open OpamStd.Op
open OpamPackage.Set.Op

let log ?level fmt = OpamConsole.log ?level "STATE" fmt
let slog = OpamConsole.slog

open OpamStateTypes

let load_selections ?lock_kind gt switch =
  OpamStateConfig.Switch.safe_read_selections ?lock_kind gt switch

let load_switch_config ?lock_kind gt switch =
  match OpamStateConfig.Switch.read_opt ?lock_kind gt switch with
  | Some c -> c
  | exception (OpamPp.Bad_version _ as e) ->
    OpamFormatUpgrade.hard_upgrade_from_2_1_intermediates
      ~global_lock:gt.global_lock gt.root;
    raise e
  | None ->
    (OpamConsole.error
       "No config file found for switch %s. Switch broken?"
       (OpamSwitch.to_string switch);
     OpamFile.Switch_config.empty)

let filter_available_packages gt switch switch_config ~opams =
  OpamPackage.keys @@
    OpamPackage.Map.filter (fun package opam ->
        OpamFilter.eval_to_bool ~default:false
          (OpamPackageVar.resolve_switch_raw ~package gt switch switch_config)
          (OpamFile.OPAM.available opam))
      opams

let compute_available_and_pinned_packages gt switch switch_config ~pinned ~opams =
  (* remove all versions of pinned packages, but the pinned-to version *)
  let pinned_names = OpamPackage.names_of_packages pinned in
  let (opams, pinned) =
    OpamPackage.Map.partition
      (fun nv _ ->
         not (OpamPackage.Name.Set.mem nv.name pinned_names) ||
         OpamPackage.Set.mem nv pinned)
      opams
  in
  (filter_available_packages gt switch switch_config ~opams, pinned)

let compute_available_packages gt switch switch_config ~pinned ~opams =
  fst @@ compute_available_and_pinned_packages gt switch switch_config ~pinned ~opams

let repos_list_raw rt switch_config =
  let global, repos =
    match switch_config.OpamFile.Switch_config.repos with
    | None -> true, OpamGlobalState.repos_list rt.repos_global
    | Some repos -> false, repos
  in
  let found, notfound =
    List.partition (fun r ->
        OpamRepositoryName.Map.mem r rt.repositories)
      repos
  in
  List.iter (fun r ->
      log "Ignoring %s-selected repository %S, no configured repository by \
           this name found"
        (if global then "globally" else "switch")
        (OpamRepositoryName.to_string r))
    notfound;
  found

let repos_list st =
  repos_list_raw st.switch_repos st.switch_config

let infer_switch_invariant_raw
    gt switch switch_config opams
    packages compiler_packages installed_roots available_packages
  =
  let compiler = compiler_packages %% installed_roots in
  let compiler =
    if OpamPackage.Set.is_empty compiler then compiler_packages
    else compiler
  in
  let env nv v =
    if List.mem v OpamPackageVar.predefined_depends_variables then
      match OpamVariable.Full.to_string v with
      | "dev" | "with-test" | "with-doc" | "with-dev-setup" -> Some (B false)
      | _ -> None
    else
      OpamPackageVar.resolve_switch_raw ~package:nv gt switch switch_config v
  in
  let resolve_deps nv =
    let opam = OpamPackage.Map.find nv opams in
    OpamPackageVar.filter_depends_formula
      ~build:true ~post:true ~default:true ~env:(env nv)
      (OpamFormula.ands [
          OpamFile.OPAM.depends opam;
          OpamFile.OPAM.depopts opam
        ])
    |> OpamFormula.packages packages
  in
  let dmap =
    OpamPackage.Set.fold (fun nv dmap ->
        let deps = resolve_deps nv in
        let dmap =
          OpamPackage.Map.update nv ((++) deps) OpamPackage.Set.empty dmap
        in
        let dmap =
          OpamPackage.Set.fold (fun d dmap ->
              OpamPackage.Map.update d (OpamPackage.Set.add nv)
                OpamPackage.Set.empty dmap)
            deps dmap
        in
        dmap)
      (OpamPackage.packages_of_names available_packages @@
       OpamPackage.names_of_packages @@
       compiler)
      OpamPackage.Map.empty
  in
  let counts =
    OpamPackage.Set.fold (fun nv counts ->
        let count =
          try OpamPackage.Set.cardinal (OpamPackage.Map.find nv dmap)
          with Not_found -> 0
        in
        (nv, count) :: counts
      )
      compiler []
  in
  match List.sort (fun (_, c1) (_, c2) -> compare c1 c2) counts with
  | (nv, _) :: _ ->
    let versions =
      OpamPackage.packages_of_name available_packages nv.name
    in
    let n = OpamPackage.Set.cardinal versions in
    if n <= 1 then
      OpamFormula.Atom (nv.name, Empty)
    else if nv = OpamPackage.Set.max_elt versions then
      OpamFormula.Atom (nv.name, Atom (`Geq, nv.version))
    else
      OpamFormula.Atom (nv.name, Atom (`Eq, nv.version))
  | [] -> OpamFormula.Empty

let infer_switch_invariant st =
  let compiler_packages =
    if OpamPackage.Set.is_empty st.compiler_packages then
      OpamPackage.Set.filter (fun nv ->
          OpamFile.OPAM.has_flag Pkgflag_Compiler
            (OpamPackage.Map.find nv st.opams))
        st.installed
    else st.compiler_packages
  in
  let lazy available_packages = st.available_packages in
  infer_switch_invariant_raw
    st.switch_global st.switch st.switch_config st.opams
    st.packages compiler_packages st.installed_roots available_packages

let depexts_raw ~env nv opams =
  try
    let opam = OpamPackage.Map.find nv opams in
    List.fold_left (fun depexts (names, filter) ->
        if OpamFilter.eval_to_bool ~default:false env filter then
          OpamSysPkg.Set.Op.(names ++ depexts)
        else depexts)
      OpamSysPkg.Set.empty
      (OpamFile.OPAM.depexts opam)
  with Not_found -> OpamSysPkg.Set.empty

module Installed_cache = OpamCached.Make(struct
    type t = OpamFile.OPAM.t OpamPackage.Map.t
    let name = "installed"
  end)

let depexts_status_of_packages_raw
    ~depexts ?env global_config switch_config packages =
  if OpamPackage.Set.is_empty packages then OpamPackage.Map.empty else
  let open OpamSysPkg.Set.Op in
  let syspkg_set, syspkg_map =
    OpamPackage.Set.fold (fun nv (set, map) ->
        let s = depexts nv in
        s ++ set,
        if OpamSysPkg.Set.is_empty s then map
        else OpamPackage.Map.add nv s map)
      packages (OpamSysPkg.Set.empty, OpamPackage.Map.empty)
  in
  let chronos = OpamConsole.timer () in
  let bypass =
    OpamFile.Config.depext_bypass global_config ++
    switch_config.OpamFile.Switch_config.depext_bypass
  in
  let syspkg_set = syspkg_set -- bypass in
  let ret =
    match OpamSysInteract.packages_status ?env global_config syspkg_set with
    | avail, not_found ->
      let avail, not_found =
        if OpamStateConfig.(!r.no_depexts) then
          (* Mark all as available. This is necessary to store the exceptions
             afterwards *)
          avail ++ not_found, OpamSysPkg.Set.empty
        else if OpamFile.Config.depext_cannot_install global_config then
          OpamSysPkg.Set.empty, avail ++ not_found
        else
          avail, not_found
      in
      OpamPackage.Map.map (fun set ->
          { OpamSysPkg.s_available = set %% avail;
            OpamSysPkg.s_not_found = set %% not_found}
        ) syspkg_map
    | exception (Failure msg) ->
      OpamConsole.note "%s\nYou can disable this check using 'opam \
                        option --global depext=false'"
        msg;
      OpamPackage.Map.empty
  in
  log "depexts loaded in %.3fs" (chronos());
  ret

let depexts_unavailable_raw sys_packages nv =
  match OpamPackage.Map.find_opt nv sys_packages with
  | Some { OpamSysPkg.s_not_found; _}
    when not (OpamSysPkg.Set.is_empty s_not_found) ->
    Some s_not_found
  | _ -> None

let load lock_kind gt rt switch =
  OpamFormatUpgrade.as_necessary_repo_switch_light_upgrade lock_kind `Switch gt;
  let chrono = OpamConsole.timer () in
  log "LOAD-SWITCH-STATE %@ %a" (slog OpamSwitch.to_string) switch;
  if not (OpamGlobalState.switch_exists gt switch) then
    (log "The switch %a does not appear to be installed according to %a"
       (slog OpamSwitch.to_string) switch
       (slog @@ OpamFile.to_string @* OpamPath.config) gt.root;

     OpamConsole.error_and_exit
       (if OpamStateConfig.(!r.switch_from = `Command_line) then `Bad_arguments
        else `Configuration_error)
       "The selected switch %s is not installed.%s"
       (OpamSwitch.to_string switch)
     @@ match OpamStateConfig.(!r.switch_from) with
     | `Command_line -> ""
     | `Default ->
       " Please choose a different one using 'opam switch <name>', or use the \
        '--switch <name>' flag."
     | `Env ->
       " Please fix the value of the OPAMSWITCH environment variable, or use \
        the '--switch <name>' flag")
  else
  let gt = OpamGlobalState.fix_switch_list gt in
  let lock =
    OpamFilename.flock lock_kind (OpamPath.Switch.lock gt.root switch)
  in
  let switch_config = load_switch_config ~lock_kind gt switch in
  if OpamStateConfig.is_newer_than_self gt then
    log "root version (%s) is greater than running binary's (%s); \
         load with best-effort (read-only)"
      (OpamVersion.to_string (OpamFile.Config.opam_root_version gt.config))
      (OpamVersion.to_string (OpamFile.Config.root_version));
  if OpamVersion.compare
      switch_config.opam_version
      OpamFile.Switch_config.oldest_compatible_format_version
     < 0 then
    OpamConsole.error_and_exit `Configuration_error
      "Could not load opam switch %s: it reports version %s while >= %s was \
       expected"
      (OpamSwitch.to_string switch)
      (OpamVersion.to_string (switch_config.opam_version))
      (OpamVersion.to_string
         OpamFile.Switch_config.oldest_compatible_format_version);
  let { sel_installed = installed; sel_roots = installed_roots;
        sel_pinned = pinned; sel_compiler = compiler_packages; } =
    load_selections ~lock_kind gt switch
  in
  let pinned, pinned_opams =
    OpamPackage.Set.fold (fun nv (pinned,opams) ->
        let overlay_dir =
          OpamPath.Switch.Overlay.package gt.root switch nv.name
        in
        match OpamFileTools.read_opam overlay_dir with
        | None -> (* No overlay => just pinned to a version *)
          OpamPackage.Set.add nv pinned, opams
        | Some o ->
          let version =
            match OpamFile.OPAM.version_opt o with
            | Some v when v <> nv.version ->
              log "warn: %s has conflicting pinning versions between \
                   switch-state (%s) and overlay (%s). Using %s."
                (OpamPackage.Name.to_string nv.name)
                (OpamPackage.Version.to_string nv.version)
                (OpamPackage.Version.to_string v)
                (OpamPackage.Version.to_string v);
              v
            | _ -> nv.version
          in
          let nv = OpamPackage.create nv.name version in
          let o = OpamFile.OPAM.with_version version o in
          OpamPackage.Set.add nv pinned,
          OpamPackage.Map.add nv o opams
      )
      pinned (OpamPackage.Set.empty, OpamPackage.Map.empty)
  in
  let installed_opams =
    let cache_file = OpamPath.Switch.installed_opams_cache gt.root switch in
    match Installed_cache.load cache_file with
    | Some opams ->
      OpamPackage.Map.mapi (fun nv opam ->
          let metadata_dir =
            OpamPath.Switch.installed_opam gt.root switch nv
            |> OpamFile.filename
            |> OpamFilename.dirname
            |> OpamFilename.Dir.to_string
          in
          OpamFile.OPAM.with_metadata_dir (Some (None, metadata_dir)) opam)
        opams
    | None ->
      let opams =
        OpamPackage.Set.fold (fun nv opams ->
            OpamStd.Option.Op.(
              (OpamFile.OPAM.read_opt
                 (OpamPath.Switch.installed_opam gt.root switch nv)
               >>| fun opam -> OpamPackage.Map.add nv opam opams)
              +! opams))
          installed OpamPackage.Map.empty
      in
      Installed_cache.save cache_file opams;
      opams
  in
  let repos_package_index =
    OpamRepositoryState.build_index rt (repos_list_raw rt switch_config)
  in
  let opams =
    OpamPackage.Map.union (fun _ x -> x) repos_package_index pinned_opams
  in
  let available_packages =
    lazy (compute_available_and_pinned_packages gt switch switch_config
            ~pinned ~opams)
  in
  let opams =
    (* Keep definitions of installed packages, but lowest priority, and after
       computing availability *)
    OpamPackage.Map.union (fun _ x -> x) installed_opams opams
  in
  let packages = OpamPackage.keys opams in
  let installed_without_def =
    OpamPackage.Set.fold (fun nv nodef ->
        if OpamPackage.Map.mem nv installed_opams then nodef else
        try
          let o = OpamPackage.Map.find nv opams in
          if lock_kind = `Lock_write then (* auto-repair *)
            (log "Definition missing for installed package %s, \
                  copying from repo"
               (OpamPackage.to_string nv);
             OpamFile.OPAM.write
               (OpamPath.Switch.installed_opam gt.root switch nv) o);
          nodef
        with Not_found -> OpamPackage.Set.add nv nodef)
      installed OpamPackage.Set.empty
  in
  if not (OpamPackage.Set.is_empty installed_without_def) then
    OpamConsole.error
      "No definition found for the following installed packages: %s\n\
       This switch may need to be reinstalled"
      (OpamPackage.Set.to_string installed_without_def);
  let changed = lazy (
    (* Note: This doesn't detect changed _dev_ packages, since it's based on the
       metadata or the archive hash changing and they don't have an archive
       hash. Therefore, dev package update needs to add to the reinstall file *)
    let changed =
      OpamPackage.Map.merge (fun _ opam_new opam_installed ->
          match opam_new, opam_installed with
          | Some r, Some i when not (OpamFile.OPAM.effectively_equal ~modulo_state:true i r) ->
            Some ()
          | _ -> None)
        opams installed_opams
      |> OpamPackage.keys
    in
    log "Detected changed packages (marked for reinstall): %a"
      (slog OpamPackage.Set.to_string) changed;
    changed
  ) in
  let switch_config, switch_invariant =
    match switch_config.invariant with
    | Some invariant -> switch_config, invariant
    | None ->
      let available_packages =
        let lazy (available_packages, pinned) = available_packages in
        OpamPackage.Set.union available_packages @@
          filter_available_packages gt switch switch_config ~opams:pinned
      in
      let invariant =
        infer_switch_invariant_raw
          gt switch switch_config opams
          packages compiler_packages installed_roots available_packages
      in
      log "Inferred invariant: from base packages %a, (roots %a) => %a"
        (slog OpamPackage.Set.to_string) compiler_packages
        (slog @@ fun () ->
         OpamPackage.Set.to_string (compiler_packages %% installed_roots)) ()
        (slog OpamFileTools.dep_formula_to_string) invariant;
      let min_opam_version = OpamVersion.of_string "2.0" in
      let opam_version =
        if OpamVersion.compare switch_config.opam_version min_opam_version < 0
        then min_opam_version
        else switch_config.opam_version
      in
      let switch_config =
        {switch_config with invariant = Some invariant; opam_version}
      in
      if lock_kind = `Lock_write then
        OpamFile.Switch_config.write
          (OpamPath.Switch.switch_config gt.root switch)
          switch_config;
      switch_config, invariant
  in
  (* Detect and initialise missing switch description *)
  let switch_config =
    if switch_config <> OpamFile.Switch_config.empty &&
       switch_config.synopsis = "" then
      let synopsis =
        if switch_invariant = OpamFormula.Empty then
          OpamSwitch.to_string switch
        else
          OpamFormula.to_string switch_invariant
      in
      let conf = { switch_config with synopsis } in
      if lock_kind = `Lock_write then (* auto-repair *)
        OpamFile.Switch_config.write
          (OpamPath.Switch.switch_config gt.root switch)
          conf;
      conf
    else switch_config
  in
  let conf_files =
    let conf_files =
      OpamFilename.files (OpamPath.Switch.config_dir gt.root switch)
    in
    List.fold_left (fun acc f ->
        if OpamFilename.check_suffix f ".config" then
          match
            OpamPackage.Name.of_string
              OpamFilename.(Base.to_string (basename (chop_extension f)))
          with
          | name when OpamPackage.has_name installed name ->
            OpamPackage.Name.Map.add name
              (OpamFile.Dot_config.safe_read
                 (OpamPath.Switch.config gt.root switch name))
              acc
          | exception (Failure _) -> acc
          | _ -> acc
        else acc)
      OpamPackage.Name.Map.empty
      conf_files
  in
  let ext_files_changed = lazy (
    OpamPackage.Name.Map.fold (fun name conf acc ->
        let nv = OpamPackage.package_of_name installed name in
        let path = lazy (
          OpamStd.Sys.split_path_variable (OpamStd.Env.get "PATH")
          |> List.map OpamFilename.Dir.of_string
        ) in
        if
          List.exists (fun (file, hash) ->
              let exists = OpamFilename.exists file in
              let should_exist = OpamHash.is_null hash in
              let changed =
                exists <> should_exist ||
                exists && not (OpamHash.check_file (OpamFilename.to_string file) hash)
              in
              if not exists && should_exist then
                OpamConsole.warning
                  "System file %s, which package %s depends upon, no longer \
                   exists.\n\
                   The package will need to either be removed, or reinstalled. \
                   You may need to restore its system dependencies for the \
                   latter."
                  (OpamFilename.to_string file) (OpamPackage.to_string nv)
              else if changed then
                OpamConsole.warning
                  "File %s, which package %s depends upon, was changed on your \
                   system.\n\
                   The package will need to be reinstalled."
                  (OpamFilename.to_string file) (OpamPackage.to_string nv)
              else
                (let exec_not_in_path =
                   should_exist &&
                   OpamFilename.is_exec file &&
                   let dirname = OpamFilename.dirname file in
                   not @@ List.exists (OpamFilename.Dir.equal dirname)
                     (Lazy.force path)
                 in
                 if exec_not_in_path then
                   OpamConsole.warning
                     "File %s, which package %s depends upon, \
                      is no longer in your %s."
                     (OpamFilename.to_string file) (OpamPackage.to_string nv)
                     (OpamConsole.colorise `bold "PATH"));
              changed)
            (OpamFile.Dot_config.file_depends conf)
        then OpamPackage.Set.add nv acc
        else acc)
      conf_files
      OpamPackage.Set.empty
  ) in
  (* depext check *)
  let available_packages = OpamCompat.Lazy.map fst available_packages in
  let sys_packages =
    if not (OpamFile.Config.depext gt.config)
    || OpamStateConfig.(!r.no_depexts) then
      lazy OpamPackage.Map.empty
    else lazy (
      depexts_status_of_packages_raw gt.config switch_config
        ~env:gt.global_variables
        (Lazy.force available_packages)
        ~depexts:(fun package ->
            let env =
              OpamPackageVar.resolve_switch_raw ~package gt switch switch_config
            in
            depexts_raw ~env package opams)
    )
  in
  let available_packages =
    if not (OpamFile.Config.depext gt.config) then available_packages
    else lazy (
      let sys_packages = Lazy.force sys_packages in
      OpamPackage.Set.filter (fun nv ->
          depexts_unavailable_raw sys_packages nv = None)
        (Lazy.force available_packages)
    )
  in
  let sys_packages_changed = lazy (
    let sys_packages =
      OpamPackage.Map.filter (fun pkg spkg ->
          OpamPackage.Set.mem pkg installed
          && not (OpamSysPkg.Set.is_empty spkg.OpamSysPkg.s_available
                  && OpamSysPkg.Set.is_empty spkg.OpamSysPkg.s_not_found))
        (Lazy.force sys_packages)
    in
    if OpamPackage.Map.is_empty sys_packages then
      OpamPackage.Set.empty
    else
    let lchanged = OpamPackage.Map.keys sys_packages in
    let changed = OpamPackage.Set.of_list lchanged in
    let sgl_pkg = OpamPackage.Set.is_singleton changed in
    let open OpamSysPkg.Set.Op in
    let missing_map =
      OpamPackage.Map.map (fun sys ->
          sys.OpamSysPkg.s_available ++ sys.OpamSysPkg.s_not_found)
        sys_packages
    in
    let missing_set =
      OpamPackage.Map.fold (fun _ -> OpamSysPkg.Set.union)
        missing_map
        OpamSysPkg.Set.empty
    in
    let sgl_spkg = OpamSysPkg.Set.is_singleton missing_set in
    if sgl_pkg then
      OpamConsole.warning
        "Opam package %s depends on the following system package%s that can \
         no longer be found: %s"
        (OpamPackage.to_string (OpamPackage.Set.choose_one changed))
        (if sgl_spkg then "" else "s")
        (OpamStd.List.concat_map " " OpamSysPkg.to_string
           (OpamSysPkg.Set.elements missing_set))
    else
      (OpamConsole.warning
         "Opam packages %s depend on the following system package%s that are \
          no longer installed: %s"
         (OpamStd.Format.pretty_list (List.map OpamPackage.to_string lchanged))
         (if sgl_spkg then "" else "s")
         (OpamStd.List.concat_map " " OpamSysPkg.to_string
            (OpamSysPkg.Set.elements missing_set));
       if OpamConsole.verbose () then
         OpamConsole.errmsg "%s"
           (OpamStd.Format.itemize (fun (pkg, spkg) ->
                Printf.sprintf "%s: depends on %s"
                  (OpamPackage.to_string pkg)
                  (OpamStd.List.concat_map ", " OpamSysPkg.to_string
                     (OpamSysPkg.Set.elements spkg)))
               (OpamPackage.Map.bindings missing_map)));
    changed
  ) in
  let available_packages = lazy (
    let chrono = OpamConsole.timer () in
    let r = Lazy.force available_packages in
    log ~level:2 "Availability of packages computed in %.3fs." (chrono ());
    r
  ) in
  let reinstall = lazy (
    OpamFile.PkgList.safe_read (OpamPath.Switch.reinstall gt.root switch) ++
    Lazy.force changed ++
    (Lazy.force ext_files_changed %% Lazy.force available_packages) ++
    Lazy.force sys_packages_changed
  ) in
  let invalidated = lazy (
    Lazy.force changed ++
    Lazy.force ext_files_changed ++
    Lazy.force sys_packages_changed
    -- Lazy.force available_packages
  ) in
  let st = {
    switch_global = (gt :> unlocked global_state);
    switch_repos = (rt :> unlocked repos_state);
    switch_lock = lock;
    switch; switch_invariant; compiler_packages; switch_config;
    repos_package_index; installed_opams;
    installed; pinned; installed_roots;
    opams; conf_files;
    packages; available_packages; sys_packages; reinstall; invalidated;
  } in
  log "Switch state loaded in %.3fs" (chrono ());
  st

let load_virtual ?repos_list ?(avail_default=true) gt rt =
  let repos_list = match repos_list with
    | Some rl -> rl
    | None -> OpamGlobalState.repos_list gt
  in
  let opams =
    OpamRepositoryState.build_index rt repos_list
  in
  let packages = OpamPackage.keys opams in
  let available_packages = lazy (
    OpamPackage.Map.filter (fun _ opam ->
        OpamFilter.eval_to_bool ~default:avail_default
          (OpamPackageVar.resolve_global gt)
          (OpamFile.OPAM.available opam))
      opams
    |> OpamPackage.keys
  ) in
  {
    switch_global = (gt :> unlocked global_state);
    switch_repos = (rt :> unlocked repos_state);
    switch_lock = OpamSystem.lock_none;
    switch = OpamSwitch.unset;
    switch_invariant = OpamFormula.Empty;
    compiler_packages = OpamPackage.Set.empty;
    switch_config = {
      OpamFile.Switch_config.empty
      with OpamFile.Switch_config.repos = Some repos_list;
    };
    installed = OpamPackage.Set.empty;
    installed_opams = OpamPackage.Map.empty;
    pinned = OpamPackage.Set.empty;
    installed_roots = OpamPackage.Set.empty;
    repos_package_index = opams;
    opams;
    conf_files = OpamPackage.Name.Map.empty;
    packages;
    sys_packages = lazy OpamPackage.Map.empty;
    available_packages;
    reinstall = lazy OpamPackage.Set.empty;
    invalidated = lazy (OpamPackage.Set.empty);
  }

let selections st =
  { sel_installed = st.installed;
    sel_roots = st.installed_roots;
    sel_compiler = st.compiler_packages;
    sel_pinned = st.pinned; }

let unlock st =
  OpamSystem.funlock st.switch_lock;
  (st :> unlocked switch_state)

let drop st =
  let _ = unlock st in ()

let with_write_lock ?dontblock st f =
  if OpamStateConfig.is_newer_than_self st.switch_global then
    OpamConsole.error_and_exit `Locked
      "The opam root has been upgraded by a newer version of opam-state \
       and cannot be written to";
  let ret, st =
    OpamFilename.with_flock_upgrade `Lock_write ?dontblock st.switch_lock
    @@ fun _ -> f ({ st with switch_lock = st.switch_lock } : rw switch_state)
    (* We don't actually change the field value, but this makes restricting the
       phantom lock type possible*)
  in
  ret, { st with switch_lock = st.switch_lock }

let opam st nv = OpamPackage.Map.find nv st.opams

let opam_opt st nv = try Some (opam st nv) with Not_found -> None

let descr_opt st nv =
  OpamStd.Option.Op.(opam_opt st nv >>= OpamFile.OPAM.descr)

let descr st nv =
  OpamStd.Option.Op.(descr_opt st nv +! OpamFile.Descr.empty)

let url st nv =
  OpamStd.Option.Op.(opam_opt st nv >>= OpamFile.OPAM.url)

let primary_url st nv =
  OpamStd.Option.Op.(url st nv >>| OpamFile.URL.url)

let primary_url_with_subpath st nv =
  match url st nv with
  | None -> None
  | Some urlf ->
    let url = OpamFile.URL.url urlf in
    match OpamFile.URL.subpath urlf with
    | None -> Some url
    | Some subpath ->
      Some OpamUrl.Op.(url / (OpamFilename.SubPath.to_string subpath))

let files st nv =
  match opam_opt st nv with
  | None -> []
  | Some opam ->
    List.map (fun (file,_base,_hash) -> file)
      (OpamFile.OPAM.get_extra_files
         ~repos_roots:(OpamRepositoryState.get_root st.switch_repos)
         opam)

let package_config st name =
  OpamPackage.Name.Map.find name st.conf_files

let is_name_installed st name =
  OpamPackage.has_name st.installed name

let find_installed_package_by_name st name =
  OpamPackage.package_of_name st.installed name

let packages_of_atoms st atoms = OpamFormula.packages_of_atoms st.packages atoms

let get_package st name =
  try OpamPinned.package st name with Not_found ->
  try find_installed_package_by_name st name with Not_found ->
  try OpamPackage.max_version (Lazy.force st.available_packages) name
  with Not_found ->
    OpamPackage.max_version st.packages name

let is_dev_package st nv =
  let opam_opt =
    if OpamPackage.Set.mem nv st.pinned then
      opam_opt st nv
    else
      match OpamPackage.Map.find_opt nv st.repos_package_index with
      | None -> opam_opt st nv
      | some -> some
  in
  match opam_opt with
  | Some opam -> OpamPackageVar.is_dev_package st opam
  | None -> false

let is_pinned st name =
  OpamPackage.has_name st.pinned name

let is_version_pinned st name =
  match OpamPackage.package_of_name_opt st.pinned name with
  | None -> false
  | Some nv ->
    match opam_opt st nv with
    | Some opam ->
      OpamPackage.Map.find_opt nv st.repos_package_index = Some opam
    | None -> false

let source_dir st nv =
  if OpamPackage.Set.mem nv st.pinned
  then OpamPath.Switch.pinned_package st.switch_global.root st.switch nv.name
  else OpamPath.Switch.sources st.switch_global.root st.switch nv

let depexts st nv =
  let env v = OpamPackageVar.resolve_switch ~package:nv st v in
 depexts_raw ~env nv st.opams

let depexts_status_of_packages st set =
  depexts_status_of_packages_raw st.switch_global.config st.switch_config set
    ~env:st.switch_global.global_variables ~depexts:(depexts st)

let depexts_unavailable st nv =
  depexts_unavailable_raw (Lazy.force st.sys_packages) nv

let dev_packages st =
  OpamPackage.Set.filter (is_dev_package st)
    (st.installed ++ OpamPinned.packages st)

let conflicts_with st subset =
  let forward_conflicts, conflict_classes =
    OpamPackage.Set.fold (fun nv (cf,cfc) ->
        try
          let opam = OpamPackage.Map.find nv st.opams in
          let conflicts =
            OpamFilter.filter_formula ~default:false
              (OpamPackageVar.resolve_switch ~package:nv st)
              (OpamFile.OPAM.conflicts opam)
          in
          OpamFormula.ors [cf; conflicts],
          List.fold_right OpamPackage.Name.Set.add
            (OpamFile.OPAM.conflict_class opam) cfc
        with Not_found -> cf, cfc)
      subset (OpamFormula.Empty, OpamPackage.Name.Set.empty)
  in
  OpamPackage.Set.filter
    (fun nv ->
       not (OpamPackage.has_name subset nv.name) &&
       (OpamFormula.verifies forward_conflicts nv ||
        try
          let opam = OpamPackage.Map.find nv st.opams in
          List.exists (fun cl -> OpamPackage.Name.Set.mem cl conflict_classes)
            (OpamFile.OPAM.conflict_class opam)
          ||
          let backwards_conflicts =
            OpamFilter.filter_formula ~default:false
              (OpamPackageVar.resolve_switch ~package:nv st)
              (OpamFile.OPAM.conflicts opam)
          in
          OpamPackage.Set.exists
            (OpamFormula.verifies backwards_conflicts) subset
       with Not_found -> false))

let remove_conflicts st subset pkgs =
  pkgs -- conflicts_with st subset pkgs

let get_conflicts_t env packages opams_map =
  let conflict_classes =
    OpamPackage.Map.fold (fun nv opam acc ->
        List.fold_left (fun acc cc ->
            OpamPackage.Name.Map.update cc
              (OpamPackage.Set.add nv) OpamPackage.Set.empty acc)
          acc
          (OpamFile.OPAM.conflict_class opam))
      opams_map
      OpamPackage.Name.Map.empty
  in
  let conflict_class_formulas =
    OpamPackage.Name.Map.map (fun pkgs ->
        OpamPackage.to_map pkgs |>
        OpamPackage.Name.Map.mapi (fun name versions ->
            let all_versions = OpamPackage.versions_of_name packages name in
            if OpamPackage.Version.Set.equal versions all_versions then Empty
            else
              (* OpamFormula.simplify_version_set all_versions (*a possible optimisation?*) *)
                (OpamFormula.ors
                   (List.map (fun v -> Atom (`Eq, v))
                      (OpamPackage.Version.Set.elements versions)))))
      conflict_classes
  in
  OpamPackage.Map.fold (fun nv opam acc ->
      let conflicts =
        OpamFilter.filter_formula ~default:false
          (env nv)
          (OpamFile.OPAM.conflicts opam)
      in
      let conflicts =
        List.fold_left (fun acc cl ->
            let cmap =
              OpamPackage.Name.Map.find cl conflict_class_formulas |>
              OpamPackage.Name.Map.remove nv.name
            in
            OpamPackage.Name.Map.fold
              (fun name vformula acc ->
                 OpamFormula.ors [acc; Atom (name, vformula)])
              cmap acc)
          conflicts
          (OpamFile.OPAM.conflict_class opam)
      in
      OpamPackage.Map.add nv conflicts acc)
    opams_map
    OpamPackage.Map.empty

let get_conflicts st packages opams_map =
  get_conflicts_t
    (fun package -> OpamPackageVar.resolve_switch ~package st)
    packages opams_map

let avoid_version st nv =
  let open OpamStd.Option.Op in
  let opam = opam st nv in
  let has_avoid_flag opam =
    OpamFile.OPAM.has_flag Pkgflag_AvoidVersion opam
    || OpamFile.OPAM.has_flag Pkgflag_Deprecated opam
  in
  has_avoid_flag opam
  && not ((OpamPackage.package_of_name_opt st.installed nv.name >>=
           (fun nv -> OpamPackage.Map.find_opt nv st.installed_opams) >>|
           has_avoid_flag)
          +! false)

let package_env_t st ~force_dev_deps ~test ~doc ~dev_setup
    ~requested_allpkgs nv v =
  if List.mem v OpamPackageVar.predefined_depends_variables then
    match OpamVariable.Full.to_string v with
    | "dev" ->
      Some (B (force_dev_deps || is_dev_package st nv))
    | "with-test" ->
      Some (B (test && OpamPackage.Set.mem nv requested_allpkgs))
    | "with-doc" ->
      Some (B (doc && OpamPackage.Set.mem nv requested_allpkgs))
    | "with-dev-setup" ->
      Some (B (dev_setup && OpamPackage.Set.mem nv requested_allpkgs))
    | _ -> None (* Computation delayed to the solver *)
  else
  let r = OpamPackageVar.resolve_switch ~package:nv st v in
  if r = None then
    (if OpamFormatConfig.(!r.strict) then
       OpamConsole.error_and_exit `File_error
         "Undefined filter variable %s in dependencies of %s"
     else
       log
         "ERR: Undefined filter variable %s in dependencies of %s")
      (OpamVariable.Full.to_string v) (OpamPackage.to_string nv);
  r

let get_dependencies_t st ~force_dev_deps ~test ~doc ~dev_setup
    ~requested_allpkgs deps opams =
  let filter_undefined nv =
    OpamFormula.map (fun (name, fc) ->
        let fc =
          OpamFormula.map (function
              | Constraint (_, FIdent (_, v, _))
              | Constraint (_, FUndef (FIdent (_, v, _))) ->
                (if OpamFormatConfig.(!r.strict) then
                   OpamConsole.error_and_exit `File_error
                 else OpamConsole.warning)
                  "Undefined filter variable %s in dependencies of %s"
                  (OpamVariable.to_string v) (OpamPackage.to_string nv);
                Atom (Filter (FBool false))
              | f -> Atom f)
            fc
        in
        Atom (name, fc))
  in
  OpamPackage.Map.mapi (fun nv opam ->
      OpamFilter.partial_filter_formula
        (package_env_t st ~force_dev_deps ~test ~doc
           ~dev_setup ~requested_allpkgs nv)
        (deps opam)
      |> filter_undefined nv) opams

let universe st
    ?(test=OpamStateConfig.(!r.build_test))
    ?(doc=OpamStateConfig.(!r.build_doc))
    ?(dev_setup=OpamStateConfig.(!r.dev_setup))
    ?(force_dev_deps=false)
    ?reinstall
    ~requested
    user_action =
  let chrono = OpamConsole.timer () in
  let names = OpamPackage.names_of_packages requested in
  let requested_allpkgs =
    OpamPackage.packages_of_names st.packages names
  in
  let env =
    package_env_t st
      ~force_dev_deps ~test ~doc ~dev_setup
      ~requested_allpkgs
  in
  let get_deps =
    get_dependencies_t st
      ~force_dev_deps ~test ~doc ~dev_setup
      ~requested_allpkgs
  in
  let u_depends =
    let depend =
      let ignored = OpamStateConfig.(!r.ignore_constraints_on) in
      if OpamPackage.Name.Set.is_empty ignored then OpamFile.OPAM.depends
      else fun opam ->
        OpamFormula.map (fun (name, cstr as atom) ->
            if OpamPackage.Name.Set.mem name ignored then
              let cstr =
                OpamFormula.map
                  (function Constraint _ -> Empty | Filter _ as f -> Atom f)
                  cstr
              in
              Atom (name, cstr)
            else Atom atom)
          (OpamFile.OPAM.depends opam)
    in
    get_deps depend st.opams
  in
  let u_depopts = get_deps OpamFile.OPAM.depopts st.opams in
  let u_conflicts = get_conflicts st st.packages st.opams in
  let u_invariant =
    if OpamStateConfig.(!r.unlock_base) then OpamFormula.Empty
    else st.switch_invariant
  in
  let u_available =
    (* TODO: removing what conflicts with the base is no longer correct now that
       we use invariants instead. Removing what conflicts with the invariant
       would be much more involved, but some solvers might struggle without any
       cleanup at this point *)
    (* remove_conflicts st base *)
    (Lazy.force st.available_packages)
  in
  let u_reinstall =
    (* Ignore reinstalls outside of the dependency cone of
       [requested_allpkgs] *)
    let resolve_deps nv =
      OpamPackageVar.filter_depends_formula
        ~build:true ~post:true ~default:true ~env:(env nv)
        (OpamFormula.ands [ OpamPackage.Map.find nv u_depends;
                            OpamPackage.Map.find nv u_depopts ])
      |> OpamFormula.packages st.packages
    in
    let requested_deps =
      OpamPackage.Set.fixpoint resolve_deps requested_allpkgs
    in
    requested_deps %% Lazy.force st.reinstall ++
    match reinstall with
    | Some set -> set
    | None -> OpamPackage.Set.empty
  in
  let missing_depexts =
    OpamPackage.Map.fold (fun nv status acc ->
        if OpamSysPkg.Set.is_empty status.OpamSysPkg.s_available
        then acc
        else OpamPackage.Set.add nv acc)
      (Lazy.force st.sys_packages)
      OpamPackage.Set.empty
  in
  let avoid_versions =
    OpamPackage.Set.filter (avoid_version st) u_available
  in
  let u =
{
  u_packages  = st.packages;
  u_action = user_action;
  u_installed = st.installed;
  u_available;
  u_depends;
  u_depopts;
  u_conflicts;
  u_installed_roots = st.installed_roots;
  u_pinned    = OpamPinned.packages st;
  u_invariant;
  u_reinstall;
  u_attrs     = ["opam-query", requested;
                 "missing-depexts", missing_depexts;
                 "avoid-version", avoid_versions];
}
  in
  log ~level:2 "Universe load: %.3fs" (chrono ());
  u

let dump_pef_state st oc =
  let conflicts = get_conflicts st st.packages st.opams in
  let print_def nv opam =
    Printf.fprintf oc "package: %s\n" (OpamPackage.name_to_string nv);
    Printf.fprintf oc "version: %s\n" (OpamPackage.version_to_string nv);
    let installed = OpamPackage.Set.mem nv st.installed in
    (* let root = OpamPackage.Set.mem nv st.installed_roots in *)
    let inv = OpamPackage.Set.mem nv st.compiler_packages in
    let pinned = OpamPackage.Set.mem nv st.pinned in
    let available = OpamPackage.Set.mem nv (Lazy.force st.available_packages) in
    let reinstall = OpamPackage.Set.mem nv (Lazy.force st.reinstall) in
    let dev = OpamPackageVar.is_dev_package st opam in
    (* current state *)
    Printf.fprintf oc "available: %b\n" available;
    if installed then output_string oc "installed: true\n";
    if pinned then output_string oc "pinned: true\n";
    if inv then output_string oc "invariant-pkg: true\n";
    if reinstall then output_string oc "reinstall: true\n";

    (* metadata (resolved for the current switch) *)
    OpamStd.List.concat_map ~left:"maintainer: " ~right:"\n" ~nil:"" " , "
      String.escaped (OpamFile.OPAM.maintainer opam) |>
    output_string oc;

    OpamFile.OPAM.depends opam |>
    OpamPackageVar.filter_depends_formula ~default:false ~dev
      ~env:(OpamPackageVar.resolve_switch ~package:nv st) |>
    OpamFormula.to_cnf |>
    OpamStd.List.concat_map ~left:"depends: " ~right:"\n" ~nil:"" " , "
      (OpamStd.List.concat_map " | " OpamFormula.string_of_atom) |>
    output_string oc;

    OpamFile.OPAM.depopts opam |>
    OpamPackageVar.filter_depends_formula ~default:false ~dev
      ~env:(OpamPackageVar.resolve_switch ~package:nv st) |>
    OpamFormula.to_cnf |>
    OpamStd.List.concat_map ~left:"recommends: " ~right:"\n" ~nil:"" " , "
      (OpamStd.List.concat_map " | " OpamFormula.string_of_atom) |>
    output_string oc;

    OpamFormula.ors
      [Atom (nv.name, Empty); OpamPackage.Map.find nv conflicts] |>
    OpamFormula.set_to_disjunction st.packages |>
    OpamStd.List.concat_map ~left:"conflicts: " ~right:"\n" ~nil:"" " , "
      OpamFormula.string_of_atom |>
    output_string oc;

    output_string oc "\n";
  in
  OpamPackage.Map.iter print_def st.opams


(* User-directed helpers *)

let is_switch_globally_set st =
  OpamFile.Config.switch st.switch_global.config = Some st.switch

let not_found_message st (name, cstr) =
  match cstr with
  | Some (relop,v) when OpamPackage.has_name st.packages name ->
    Printf.sprintf "Package %s has no version %s%s."
      (OpamPackage.Name.to_string name)
      (match relop with `Eq -> "" | r -> OpamPrinter.FullPos.relop_kind r)
      (OpamPackage.Version.to_string v)
  | _ ->
    Printf.sprintf "No package named %s found."
      (OpamPackage.Name.to_string name)

(* Display a meaningful error for an unavailable package *)
let unavailable_reason_raw st (name, vformula) =
  let candidates = OpamPackage.packages_of_name st.packages name in
  let candidates =
    OpamPackage.Set.filter
      (fun nv -> OpamFormula.check_version_formula vformula nv.version)
      candidates
  in
  if OpamPackage.Set.is_empty candidates then
    (if OpamPackage.has_name st.packages name then `UnknownVersion
     else `UnknownPackage)
  else
  let nv =
    try OpamPinned.package st name
    with Not_found ->
    match vformula with
    | Atom (_, v) when
        OpamPackage.Set.mem (OpamPackage.create name v) candidates ->
      OpamPackage.create name v
    | _ -> OpamPackage.max_version candidates name
  in
  match opam_opt st nv with
  | None -> `NoDefinition
  | Some opam ->
    let avail = OpamFile.OPAM.available opam in
    if not (OpamPackage.Set.mem nv candidates) then
      `Pinned nv
    else if not (OpamFilter.eval_to_bool ~default:false
                   (OpamPackageVar.resolve_switch ~package:nv st)
                   avail) then
      `Unavailable
        (Printf.sprintf "%s'%s'"
           (if OpamPackage.Set.cardinal candidates = 1 then ": "
            else ", e.g. ")
           (OpamFilter.to_string avail))
    else if OpamPackage.has_name
        (Lazy.force st.available_packages --
         remove_conflicts st st.compiler_packages
           (Lazy.force st.available_packages))
        name then
      `ConflictsBase
    else if OpamPackage.has_name st.compiler_packages name &&
            not OpamStateConfig.(!r.unlock_base) then
      `ConflictsInvariant
        (OpamFileTools.dep_formula_to_string st.switch_invariant)
    else
    match depexts_unavailable st (OpamPackage.Set.max_elt candidates) with
    | Some missing ->
      let missing =
        List.rev_map OpamSysPkg.to_string (OpamSysPkg.Set.elements missing)
      in
      `MissingDepexts missing
    | None -> `Default

(* Display a meaningful error for an unavailable package *)
let unavailable_reason st ?(default="") atom =
  match unavailable_reason_raw st atom with
  | `UnknownVersion ->
    "no matching version"
  | `UnknownPackage ->
    "unknown package"
  | `NoDefinition ->
    "no package definition found"
  | `Pinned nv ->
    Printf.sprintf
      "not available because the package is pinned to version %s"
      (OpamPackage.version_to_string nv)
  | `Unavailable msg ->
    Printf.sprintf "unmet availability conditions%s" msg
  | `ConflictsBase ->
    "conflict with the base packages of this switch"
  | `ConflictsInvariant invariant ->
    Printf.sprintf
      "incompatible with the switch invariant %s (use `--update-invariant' \
       to force)"
      (OpamConsole.colorise `bold invariant)
  | `MissingDepexts missing ->
    let msg =
      match missing with
      | [pkg] -> " '" ^ pkg ^ "'"
      | pkgs ->
        "s " ^ (OpamStd.Format.pretty_list
                  (List.rev_map (Printf.sprintf "'%s'") pkgs))
    in
    Printf.sprintf
      "depends on the unavailable system package%s. Use \
       `--no-depexts' to attempt installation anyway, or it is \
       possible that a depext package name in the opam file \
       is incorrect." msg
  | `Default ->
    default

let update_package_metadata nv opam st =
  { st with
    opams = OpamPackage.Map.add nv opam st.opams;
    packages = OpamPackage.Set.add nv st.packages;
    available_packages = lazy (
      if OpamFilter.eval_to_bool ~default:false
          (OpamPackageVar.resolve_switch_raw ~package:nv
             st.switch_global st.switch st.switch_config)
          (OpamFile.OPAM.available opam)
      then OpamPackage.Set.add nv (Lazy.force st.available_packages)
      else OpamPackage.Set.remove nv (Lazy.force st.available_packages)
    );
    reinstall = lazy
      (match OpamPackage.Map.find_opt nv st.installed_opams with
       | Some inst ->
         if OpamFile.OPAM.effectively_equal inst opam
         then OpamPackage.Set.remove nv (Lazy.force st.reinstall)
         else OpamPackage.Set.add nv (Lazy.force st.reinstall)
       | _ -> Lazy.force st.reinstall);
  }

let remove_package_metadata nv st =
  { st with
    opams = OpamPackage.Map.remove nv st.opams;
    packages = OpamPackage.Set.remove nv st.packages;
    available_packages =
      lazy (OpamPackage.Set.remove nv (Lazy.force st.available_packages));
  }

let update_pin nv opam st =
  let version =
    OpamStd.Option.default nv.version (OpamFile.OPAM.version_opt opam)
  in
  let nv = OpamPackage.create nv.name version in
  let pinned =
    OpamPackage.Set.add nv (OpamPackage.filter_name_out st.pinned nv.name)
  in
  let available_packages = lazy (
    OpamPackage.filter_name_out (Lazy.force st.available_packages) nv.name
  ) in
  let st =
    update_package_metadata nv opam { st with pinned; available_packages }
  in
  if not (OpamFile.Config.depext st.switch_global.config)
  || OpamSysPkg.Set.is_empty (depexts st nv)
  then st else
  let sys_packages = lazy (
    OpamPackage.Map.union (fun _ n -> n)
      (Lazy.force st.sys_packages)
      (depexts_status_of_packages st (OpamPackage.Set.singleton nv))
  ) in
  let available_packages = lazy (
    OpamPackage.Set.filter (fun nv -> depexts_unavailable st nv = None)
      (Lazy.force st.available_packages)
  ) in
  { st with sys_packages; available_packages }

let do_backup lock st = match lock with
  | `Lock_write ->
    let file = OpamPath.Switch.backup st.switch_global.root st.switch in
    let previous_selections = selections st in
    OpamFile.SwitchSelections.write file previous_selections;
    (function
      | true -> OpamFilename.remove (OpamFile.filename file)
      | false ->
        (* Reload, in order to skip the message if there were no changes *)
        let new_selections =
          load_selections ~lock_kind:lock st.switch_global st.switch
        in
        if new_selections.sel_installed = previous_selections.sel_installed
        then OpamFilename.remove (OpamFile.filename file)
        else
          OpamConsole.errmsg "%s"
            (OpamStd.Format.reformat
               (Printf.sprintf
                  "\nThe former state can be restored with:\n\
                  \    %s switch import %S\n"
                  Sys.executable_name (OpamFile.to_string file) ^
                if OpamPackage.Set.is_empty
                    (new_selections.sel_roots -- new_selections.sel_installed)
                then "" else
                  Printf.sprintf
                    "Or you can retry to install your package selection with:\n\
                    \    %s install --restore\n"
                  Sys.executable_name)))
  | _ -> fun _ -> ()

let with_ lock ?rt ?(switch=OpamStateConfig.get_switch ()) gt f =
  (match rt with
   | Some rt -> fun f -> f (rt :> unlocked repos_state)
   | None -> OpamRepositoryState.with_ `Lock_none gt)
  @@ fun rt ->
  let st = load lock gt rt switch in
  let cleanup_backup = do_backup lock st in
  try let r = f st in drop st; cleanup_backup true; r
  with e ->
    OpamStd.Exn.finalise e @@ fun () ->
    drop st;
    if not OpamCoreConfig.(!r.keep_log_dir) then cleanup_backup false

let update_repositories gt update_fun switch =
  OpamFilename.with_flock `Lock_write (OpamPath.Switch.lock gt.root switch)
  @@ fun _ ->
  let conf = load_switch_config ~lock_kind:`Lock_write gt switch in
  let repos =
    match conf.OpamFile.Switch_config.repos with
    | None -> OpamGlobalState.repos_list gt
    | Some repos -> repos
  in
  let conf =
    { conf with
      OpamFile.Switch_config.repos = Some (update_fun repos) }
  in
  OpamFile.Switch_config.write
    (OpamPath.Switch.switch_config gt.root switch)
    conf


(* dependencies computation *)

let dependencies_filter_to_formula_t ~build ~post st nv =
  let env v =
    if List.mem v OpamPackageVar.predefined_depends_variables then
      match OpamVariable.Full.to_string v with
      | "build" -> Some (B build)
      | "post" -> Some (B post)
      | _ -> None
    else
      OpamPackageVar.resolve_switch ~package:nv st v
  in
  OpamFilter.filter_formula ~default:true env

let dependencies_t st base_deps_compute deps_compute
    ~depopts ~installed ?(unavailable=false) packages =
  if OpamPackage.Set.is_empty packages then OpamPackage.Set.empty else
  let base =
    packages ++
    if installed then st.installed
    else if unavailable then st.packages
    else Lazy.force st.available_packages
  in
  log ~level:3 "dependencies packages=%a"
    (slog OpamPackage.Set.to_string) packages;
  let timer = OpamConsole.timer () in
  let base_depends =
    let filter = base_deps_compute base in
    let get_deps =
      get_dependencies_t st
        ~force_dev_deps:false ~test:false ~doc:false
        ~dev_setup:false ~requested_allpkgs:packages
    in
    let opams =
      OpamPackage.Set.fold (fun pkg opams ->
          OpamPackage.Map.add pkg (OpamPackage.Map.find pkg st.opams) opams)
        base OpamPackage.Map.empty
    in
    let u_depends = get_deps OpamFile.OPAM.depends opams in
    let depends =
      OpamPackage.Map.filter_map filter u_depends
    in
    if depopts then
      let u_depopts = get_deps OpamFile.OPAM.depopts opams in
      let depopts =
        OpamPackage.Map.filter_map filter u_depopts
      in
      OpamPackage.Map.union (fun d d' -> OpamFormula.And (d, d'))
        depopts depends
    else
      depends
  in
  let result = deps_compute base base_depends packages in
  log "dependencies (%.3f) result=%a" (timer ())
    (slog OpamPackage.Set.to_string) result;
  result

let dependencies st ~build ~post =
  dependencies_t st
    (fun base nv ff ->
       if OpamPackage.Set.mem nv base then Some ff else None)
    (fun base base_depends packages ->
       let open OpamPackage.Set.Op in
       let get_deps nvs =
         OpamPackage.Set.fold (fun nv set ->
             let depends_formula =
               dependencies_filter_to_formula_t ~build ~post st nv
                 (OpamPackage.Map.find nv base_depends)
             in
             if depends_formula = Empty then set else
             let deps = OpamFormula.packages base depends_formula in
             if OpamPackage.Set.is_empty deps then set else
               deps ++ set)
           nvs OpamPackage.Set.empty
       in
       let rec aux all deps =
         let new_deps = get_deps deps in
         if OpamPackage.Set.is_empty new_deps then all
         else aux (all ++ new_deps) (new_deps -- all)
       in
       aux packages packages)

let reverse_dependencies st ~build ~post =
  dependencies_t st
    (fun base nv ff ->
       if OpamPackage.Set.mem nv base then
         Some (dependencies_filter_to_formula_t ~build ~post st nv ff)
       else None)
    (fun base base_depends packages ->
       let base_int_pkg =
         OpamPackage.Set.fold (fun nv map ->
             OpamStd.IntMap.add (Hashtbl.hash nv) nv map)
           base OpamStd.IntMap.empty
       in
       let rev_deps =
         OpamPackage.Map.fold (fun nv depends_formula rev_deps ->
             let depends =
               OpamPackage.Set.fold (fun nv deps ->
                   Hashtbl.hash nv :: deps)
                 (OpamFormula.packages base depends_formula) []
             in
             List.fold_left (fun rev_deps rev_nv ->
                 OpamStd.IntMap.update rev_nv
                   (fun l -> Hashtbl.hash nv :: l)
                   [] rev_deps)
               rev_deps depends)
           base_depends OpamStd.IntMap.empty
       in
       let open OpamStd.IntSet.Op in
       let get_revdeps all done_ packages =
         OpamStd.IntSet.fold (fun nv (all, done_, remaining) ->
             if OpamStd.IntSet.mem nv done_ then
               all, done_, remaining
             else
             match OpamStd.IntMap.find_opt nv rev_deps with
             | Some deps ->
               let deps = (OpamStd.IntSet.of_list deps) in
               deps ++ all,
               OpamStd.IntSet.add nv done_,
               deps ++ remaining
             | None -> all, done_, remaining)
           packages (all, done_, OpamStd.IntSet.empty)
       in
       let rec aux all done_ remaining =
         let all, done_, remaining =
           get_revdeps all done_ remaining
         in
         if OpamStd.IntSet.is_empty remaining then all else
           aux all done_ remaining
       in
       let int_revdeps =
         let packages =
           OpamPackage.Set.fold (fun nv set ->
               OpamStd.IntSet.add (Hashtbl.hash nv) set)
             packages OpamStd.IntSet.empty
         in
         aux OpamStd.IntSet.empty OpamStd.IntSet.empty packages
       in
       OpamStd.IntSet.fold (fun hash result ->
           match OpamStd.IntMap.find_opt hash base_int_pkg with
           | Some nv -> OpamPackage.Set.add nv result
           | None -> OpamStd.Sys.exit_because `Internal_error)
         int_revdeps packages)

(* invariant computation *)

let invariant_root_packages st =
  OpamPackage.Set.filter (OpamFormula.verifies st.switch_invariant) st.installed

let compute_invariant_packages st =
  let pkgs = invariant_root_packages st in
  dependencies ~build:false ~post:true ~depopts:false ~installed:true
    ~unavailable:false st pkgs

let compiler_packages st =
  let compiler_packages =
    OpamPackage.Set.filter (fun nv ->
        try OpamFile.OPAM.has_flag Pkgflag_Compiler (opam st nv)
        with Not_found -> false)
      st.installed
  in
  dependencies ~build:true ~post:false ~depopts:true ~installed:true
    ~unavailable:false st compiler_packages
