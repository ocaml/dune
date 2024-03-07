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
open OpamTypesBase
open OpamStateTypes
open OpamStd.Op
open OpamFilename.Op

let log fmt = OpamConsole.log "FMT_UPG" fmt
let slog = OpamConsole.slog

exception Upgrade_done of OpamFile.Config.t * (OpamFile.Config.t -> unit) option

(* - Package and aux functions - *)

let upgrade_depexts_to_2_0_beta5 filename depexts =
  let arch = OpamVariable.of_string "arch" in
  let os = OpamVariable.of_string "os" in
  let os_family = OpamVariable.of_string "os-family" in
  let distro = OpamVariable.of_string "os-distribution" in
  let eq var v = FOp (FIdent ([], var, None), `Eq, FString v) in
  (* Transform all known tags into the corresponding filters *)
  let rec transform_filter =
    let transform_tag = function
      | "amd64"       -> eq arch "x86_64"
      | "x86"         -> eq arch "x86_32"
      | "arm"|"armv7" -> eq arch "arm32"
      | "ppc"         -> eq arch "ppc32"
      | "x86_64" | "ppc64" as a -> eq arch a
      | "osx"         -> eq os "macos"
      | "linux" | "unix" | "xenserver" | "freebsd" | "openbsd" | "netbsd"
      | "dragonfly" | "win32" | "cygwin" as o -> eq os o

      | "nixpkgs"     -> eq distro "nixos"
      | "arch"        -> eq distro "archlinux"
      | "homebrew" | "macports" | "debian" | "ubuntu" | "centos" | "fedora"
      | "rhel" | "opensuse" | "oraclelinux" | "ol" | "mageia" | "alpine"
      | "archlinux" | "gentoo" | "nixos" as d -> eq distro d

      | "bsd"         -> eq os_family "bsd"
      | "mswindows"   -> eq os_family "windows"
      | "source"      -> failwith "\"source\" tag"
      | s             -> failwith ("Unknown tag "^s)
    in
    function
    | FAnd (f1, f2) -> FAnd (transform_filter f1, transform_filter f2)
    | FString s -> transform_tag s
    | _ -> raise Exit (* the filter is already in the new format if it
                         contains anything else *)
  in
  OpamStd.List.filter_map
    (fun (names, filter) ->
       try Some (names, transform_filter filter) with
       | Exit -> Some (names, filter)
       | Failure m ->
         OpamConsole.warning "Ignored depext in %s: %s" filename m;
         None)
    depexts

let v2_0 = OpamVersion.of_string "2.0"

let opam_file_from_1_2_to_2_0 ?filename opam =
  let ocaml_pkgname = OpamPackage.Name.of_string "ocaml" in

  let ocaml_wrapper_pkgname = OpamPackage.Name.of_string "ocaml" in
  let ocaml_official_pkgname =
    OpamPackage.Name.of_string "ocaml-base-compiler"
  in
  let ocaml_variants_pkgname = OpamPackage.Name.of_string "ocaml-variants" in
  let ocaml_system_pkgname = OpamPackage.Name.of_string "ocaml-system" in

  let filename = match filename with
    | Some f -> OpamFile.to_string f
    | None -> match OpamFile.OPAM.metadata_dir opam with
      | Some (Some r, rel_d) ->
        Printf.sprintf "<%s>/%s/opam" (OpamRepositoryName.to_string r) rel_d
      | Some (None, abs_d) ->
        let d = OpamFilename.Dir.of_string abs_d in
        OpamFilename.to_string (d // "opam")
      | None -> "opam file"
  in
  let available =
    OpamFilter.distribute_negations (OpamFile.OPAM.available opam)
  in
  let sym_op = function
    | (`Eq | `Neq) as op -> op
    | `Gt -> `Lt
    | `Geq -> `Leq
    | `Lt -> `Gt
    | `Leq -> `Geq
  in
  let mk_constraint op v = Atom (Constraint (op, FString v)) in
  let get_atom ?(op=`Eq) v =
    if v = "system" then
      ocaml_system_pkgname, Empty
    else
      (if String.contains v '+' then ocaml_variants_pkgname
       else ocaml_official_pkgname),
      mk_constraint op v
  in
  let module NMap = OpamPackage.Name.Map in
  let pkg_deps, pkg_conflicts, available_opt =
    let rec aux avail = match avail with
      | FOp (FString _ as fs, op, (FIdent _ as fid)) ->
        aux (FOp (fid, sym_op op, fs))
      | FOp (FIdent ([],var,None), op, FString v) ->
        (match OpamVariable.to_string var, op with
         | "ocaml-version", _ ->
           NMap.singleton ocaml_wrapper_pkgname (mk_constraint op v),
           NMap.empty,
           None
         | "compiler", `Neq ->
           NMap.empty, NMap.of_list [get_atom v], None
         | "compiler", op ->
           NMap.of_list [get_atom ~op v], NMap.empty, None
         | _ -> NMap.empty, NMap.empty, Some avail)
      | FIdent ([], v, None) when
          OpamVariable.to_string v = "preinstalled" ->
        NMap.singleton ocaml_system_pkgname Empty,
        NMap.empty,
        None
      | FNot (FIdent ([], v, None)) when
          OpamVariable.to_string v = "preinstalled" ->
        NMap.empty,
        NMap.singleton ocaml_system_pkgname Empty,
        None
      | FNot f ->
        let pkg_deps, pkg_conflicts, available_opt = aux f in
        pkg_conflicts, pkg_deps,
        OpamStd.Option.map (fun f -> FNot f) available_opt
      | FAnd (f1,f2) ->
        let deps1, cflt1, f1 = aux f1 in
        let deps2, cflt2, f2 = aux f2 in
        (NMap.union (fun d1 d2 -> OpamFormula.ands [d1; d2]) deps1 deps2,
         NMap.union (fun c1 c2 -> OpamFormula.ors [c1; c2]) cflt1 cflt2,
         match f1, f2 with
         | Some f1, Some f2 -> Some (FAnd (f1, f2))
         | None, f | f, None -> f)
      | FOr (f1,f2) ->
        let deps1, cflt1, f1 = aux f1 in
        let deps2, cflt2, f2 = aux f2 in
        let err () =
          OpamConsole.error "Unconvertible 'available:' disjunction in %s"
            filename
        in
        (NMap.union (fun d1 d2 -> OpamFormula.ors [d1; d2]) deps1 deps2,
         NMap.union (fun c1 c2 -> OpamFormula.ands [c1; c2]) cflt1 cflt2,
         match f1, f2 with
         | Some f1, Some f2 -> Some (FOr (f1,f2))
         | None, None -> None
         | None, f | f, None -> err (); f)
      | f -> NMap.empty, NMap.empty, Some f
    in
    aux available
  in
  let pkg_deps =
    if OpamVersion.compare (OpamFile.OPAM.opam_version opam) v2_0 >= 0 ||
       NMap.mem ocaml_wrapper_pkgname pkg_deps ||
       OpamFile.OPAM.has_flag Pkgflag_Conf opam
    then pkg_deps
    else NMap.add ocaml_wrapper_pkgname Empty pkg_deps
  in
  let available =
    OpamStd.Option.default (FBool true) available_opt
  in
  if List.exists (fun v -> match OpamVariable.Full.to_string v with
      | "ocaml-version" | "compiler" | "preinstalled"
      | "ocaml-native" | "ocaml-native-tools"
      | "ocaml-native-dynlink" -> true
      | _ -> false)
      (OpamFilter.variables available)
  then OpamConsole.warning
      "Could not translate some 'ocaml-*' variables in the 'available:' \
       field of %s: %s"
      filename (OpamFilter.to_string available);
  let depends =
    let to_add,conj =
      List.fold_left (fun (to_add,conj) -> function
          | Atom (pkgname, cstr) as atom ->
            (try
               NMap.remove pkgname to_add,
               Atom (pkgname,
                     OpamFormula.ands [cstr; NMap.find pkgname to_add])
               :: conj
             with Not_found -> to_add, atom :: conj)
          | f -> to_add, f::conj)
        (pkg_deps, [])
        (OpamFormula.ands_to_list (OpamFile.OPAM.depends opam))
    in
    let remain =
      List.map (fun (name, cstr) -> Atom (name, cstr))
        (NMap.bindings to_add)
    in
    OpamFormula.ands (remain @ List.rev conj)
  in
  let rwr_depflags =
    let rwr_vars v = match OpamVariable.Full.to_string v with
      | "test" -> OpamVariable.Full.of_string "with-test"
      | "doc" -> OpamVariable.Full.of_string "with-doc"
      | _ -> v
    in
    OpamFormula.map (fun (name, cstr) ->
        let cstr =
          OpamFormula.map (function
              | Filter f -> Atom (Filter (OpamFilter.map_variables rwr_vars f))
              | Constraint _ as c -> Atom c)
            cstr
        in
        Atom (name, cstr))
  in
  let depends = rwr_depflags depends in
  let depopts = rwr_depflags (OpamFile.OPAM.depopts opam) in
  let conflicts =
    let to_add, disj =
      List.fold_left (fun (to_add,disj) -> function
          | Atom (pkgname, cstr) as atom ->
            (try
               NMap.remove pkgname to_add,
               Atom (pkgname,
                     OpamFormula.ors [cstr; NMap.find pkgname to_add])
               :: disj
             with Not_found -> to_add, atom :: disj)
          | f -> to_add, f::disj)
        (pkg_conflicts,[])
        (OpamFormula.ors_to_list (OpamFile.OPAM.conflicts opam))
    in
    let remain =
      List.map (fun (name, cstr) -> Atom (name, cstr))
        (NMap.bindings to_add)
    in
    OpamFormula.ors (remain @ List.rev disj)
  in
  let rewrite_var v =
    let mkvar s =
      OpamVariable.Full.create ocaml_pkgname (OpamVariable.of_string s)
    in
    match OpamVariable.Full.scope v with
    | OpamVariable.Full.Global ->
      (match OpamVariable.(to_string (Full.variable v)) with
       | "compiler" -> mkvar "compiler"
       | "preinstalled" -> mkvar "preinstalled"
       | "ocaml-version" -> mkvar "version"
       | "ocaml-native" -> mkvar "native"
       | "ocaml-native-tools" -> mkvar "native-tools"
       | "ocaml-native-dynlink" -> mkvar "native-dynlink"
       | _ -> v)
    | _ -> v
  in
  let auto_add_flags opam =
    (* Automatically add light-uninstall for trivial commands that won't
       need the source *)
    if OpamFile.OPAM.remove opam <> [] &&
       List.for_all
         (fun (cmd, _filter) -> match cmd with
            | [] | (CString ("ocamlfind" | "rm"), _) :: _ -> true
            | _ -> false)
         (OpamFile.OPAM.remove opam)
    then OpamFile.OPAM.add_flags [Pkgflag_LightUninstall] opam
    else opam
  in
  let filter_out_flagtags opam =
    OpamFile.OPAM.with_tags
      (List.filter (fun tag -> OpamFile.OPAM.flag_of_tag tag = None)
         (OpamFile.OPAM.tags opam))
      opam
  in
  let build_doc, install_doc =
    let rec split acc = function
      | [] -> List.rev acc, []
      | (cmd, _ as c) :: r as l ->
        if List.exists (function
            | CString s, _ -> OpamStd.String.contains ~sub:"install" s
            | _ -> false)
            cmd
        then List.rev acc, l
        else split (c::acc) r
    in
    split [] (OpamFile.OPAM.deprecated_build_doc opam)
  in
  let add_filter to_add cmdlist =
    List.map (fun (cmd,filter) ->
        cmd, match filter with
        | None -> Some to_add
        | Some f -> Some (FAnd (to_add, f)))
      cmdlist
  in
  let test_filter =
    FIdent ([], OpamVariable.of_string "with-test", None)
  in
  let doc_filter =
    FIdent ([], OpamVariable.of_string "with-doc", None)
  in
  let build =
    OpamFile.OPAM.build opam @
    add_filter test_filter (OpamFile.OPAM.deprecated_build_test opam) @
    add_filter doc_filter build_doc
  in
  let install =
    OpamFile.OPAM.install opam @
    add_filter doc_filter install_doc
  in
  let dev_repo =
    OpamStd.Option.map
      (OpamUrl.parse ~handle_suffix:true @* OpamUrl.to_string)
      (OpamFile.OPAM.dev_repo opam)
  in
  let depexts =
    upgrade_depexts_to_2_0_beta5 filename (OpamFile.OPAM.depexts opam)
  in
  let rwr_os_filters = OpamSysPoll.normalise_os in
  let rwr_arch_filters = OpamSysPoll.normalise_arch in
  let rewrite_filter =
    OpamFilter.map_up (function
        | FOp (FIdent ([],vname,None) as v, op, FString value) as ft ->
          (match OpamVariable.to_string vname with
           | "os" -> FOp (v, op, FString (rwr_os_filters value))
           | "arch" -> FOp (v, op, FString (rwr_arch_filters value))
           | _ -> ft)
        | FOp (FString value, op, (FIdent ([],vname,None) as v)) as ft ->
          (match OpamVariable.to_string vname with
           | "os" -> FOp (FString (rwr_os_filters value), op, v)
           | "arch" -> FOp (FString (rwr_arch_filters value), op, v)
           | _ -> ft)
        | ft -> ft)
  in
  opam |>
  OpamFile.OPAM.with_opam_version v2_0 |>
  OpamFile.OPAM.with_depends depends |>
  OpamFile.OPAM.with_depopts depopts |>
  OpamFile.OPAM.with_conflicts conflicts |>
  OpamFile.OPAM.with_available available |>
  OpamFile.OPAM.with_build build |>
  OpamFile.OPAM.with_install install |>
  OpamFile.OPAM.with_dev_repo_opt dev_repo |>
  OpamFile.OPAM.with_deprecated_build_test [] |>
  OpamFile.OPAM.with_deprecated_build_doc [] |>
  OpamFileTools.map_all_variables rewrite_var |>
  OpamFileTools.map_all_filters rewrite_filter |>
  OpamFile.OPAM.with_depexts depexts |>
  auto_add_flags |>
  filter_out_flagtags


(* Global state changes that need to be propagated *)
let gtc_none = { gtc_repo = false; gtc_switch = false }
let _gtc_repo = { gtc_repo = true; gtc_switch = false }
let _gtc_switch = { gtc_repo = false; gtc_switch = true }
let _gtc_both = { gtc_repo = true; gtc_switch = true }

(* - Progressive version update functions - *)

let v1_1 = OpamVersion.of_string "1.1"

let from_1_0_to_1_1 ~on_the_fly:_ root _config =
  OpamConsole.error_and_exit `Configuration_error
    "You appear to have an opam setup dating back to opam 1.0, which is no \
     longer supported since opam 2.0. Please remove \"%s\" and run \
     `opam init`"
    (OpamFilename.Dir.to_string root)

let v1_2 = OpamVersion.of_string "1.2"

let from_1_1_to_1_2 ~on_the_fly:_ root config =
  log "Upgrade pinned packages format to 1.2";
  let aliases = OpamFile.Aliases.safe_read (OpamFile.make (root // "aliases")) in
  let remove_pinned_suffix d =
    let s = OpamFilename.Dir.to_string d in
    if Filename.check_suffix s ".pinned" then
      OpamFilename.move_dir ~src:d
        ~dst:(OpamFilename.Dir.of_string (Filename.chop_suffix s ".pinned"))
  in
  let packages = lazy (
    OpamPackage.Set.of_list
      (OpamPackage.Map.keys
         (OpamFile.Package_index.safe_read
            (OpamFile.make (root / "repo" // "package-index"))))
  ) in
  OpamSwitch.Map.iter (fun switch _ ->
      let switch_root = root / OpamSwitch.to_string switch in
      let pinned_version name =
        try
          let f =
            OpamFile.make (switch_root / "overlay" /
                           OpamPackage.Name.to_string name // "opam")
          in
          match OpamFile.OPAM.version_opt (OpamFile.OPAM.read f) with
          | None -> raise Not_found
          | Some v -> v
        with e ->
          OpamStd.Exn.fatal e;
          try OpamPackage.version
                (OpamPackage.max_version (Lazy.force packages) name)
          with Not_found -> OpamPackage.Version.of_string "0" in
      let fix_version nv =
        let obsolete_pinned_v = OpamPackage.Version.of_string "pinned" in
        if nv.version = obsolete_pinned_v then
          let name = nv.name in
          OpamPackage.create name (pinned_version name)
        else nv in
      List.iter remove_pinned_suffix
        (OpamFilename.dirs (switch_root / "packages.dev"));
      List.iter remove_pinned_suffix
        (OpamFilename.dirs (switch_root / "overlay"));
      let switch_prefix = switch_root in
      let installed_f =
        OpamFile.make OpamFilename.Op.(switch_prefix // "installed")
      in
      let installed = OpamFile.PkgList.safe_read installed_f in
      OpamFile.PkgList.write installed_f
        (OpamPackage.Set.map fix_version installed);
      let installed_roots_f =
        OpamFile.make OpamFilename.Op.(switch_prefix // "installed.roots")
      in
      let installed_roots = OpamFile.PkgList.safe_read installed_roots_f in
      OpamFile.PkgList.write installed_roots_f
        (OpamPackage.Set.map fix_version installed_roots);
      (* Move .config files *)
      List.iter (fun f ->
          let name =
            OpamFilename.Base.to_string @@
            OpamFilename.basename @@
            OpamFilename.chop_extension f in
          if name <> "global-config" then
            let dst = switch_root / "lib" / name // "opam.config" in
            OpamFilename.mkdir (OpamFilename.dirname dst);
            OpamFilename.move ~src:f ~dst
        )
        (OpamFilename.files (switch_root / "config"))
    ) aliases;
  config, gtc_none

let v1_3_dev2 = OpamVersion.of_string "1.3~dev2"

let from_1_2_to_1_3_dev2 ~on_the_fly:_ root config =
  log "Upgrade switch state files format to 1.3";
  let aliases =
    OpamFile.Aliases.safe_read (OpamFile.make (root // "aliases"))
  in
  OpamSwitch.Map.iter (fun switch c ->
      let switch_dir = root / OpamSwitch.to_string switch in
      let installed_f = switch_dir // "installed" in
      let installed_roots_f = switch_dir // "installed.roots" in
      let pinned_f = switch_dir // "pinned" in
      let installed =
        OpamFile.PkgList.safe_read (OpamFile.make installed_f)
      in
      let installed_roots =
        OpamFile.PkgList.safe_read (OpamFile.make installed_roots_f)
      in
      let pinned =
        OpamFile.Pinned_legacy.safe_read (OpamFile.make pinned_f)
      in
      let pinned =
        OpamPackage.Name.Map.mapi (fun name pin ->
            let v =
              match pin with
              | OpamFile.Pinned_legacy.Version v -> v
              | OpamFile.Pinned_legacy.Source _ ->
                let overlay =
                  OpamFile.make (switch_dir / "overlay" /
                                 OpamPackage.Name.to_string name // "opam")
                in
                let opam = OpamFile.OPAM.safe_read overlay in
                OpamStd.Option.default (OpamPackage.Version.of_string "0")
                  (OpamFile.OPAM.version_opt opam)
            in
            v, pin)
          pinned
      in
      let sel_pinned =
        OpamPackage.Name.Map.fold
          (fun name (v,_) -> OpamPackage.Set.add (OpamPackage.create name v))
          pinned OpamPackage.Set.empty
      in
      let compiler =
        let version = match OpamStd.String.cut_at c '+' with
          | Some (v,_) -> v
          | None -> c
        in
        let comp =
          OpamFile.Comp.read
            (OpamFile.make (root / "compilers" / version / c // (c ^".comp")))
        in
        let atoms = OpamFormula.atoms (OpamFile.Comp.packages comp) in
        List.fold_left (fun acc (name,_) ->
            let nv =
              try
                let v, _ = OpamPackage.Name.Map.find name pinned in
                OpamPackage.create name v
              with Not_found ->
              try OpamPackage.max_version installed name
              with Not_found ->
                OpamPackage.create name
                  (OpamPackage.Version.of_string "~unknown")
            in
            OpamPackage.Set.add nv acc)
          OpamPackage.Set.empty atoms
      in
      OpamFile.LegacyState.write (OpamFile.make (switch_dir // "state"))
        { sel_installed = installed;
          sel_roots = installed_roots;
          sel_pinned;
          sel_compiler = compiler };
      OpamFilename.remove installed_f;
      OpamFilename.remove installed_roots_f;
      OpamFilename.remove pinned_f;
      (* Move .config files back *)
      OpamPackage.Set.iter (fun nv ->
          let name = nv.name in
          let src =
            switch_dir / "lib" / OpamPackage.Name.to_string name //
            "opam.config"
          in
          let dst =
            switch_dir / "config" //
            (OpamPackage.Name.to_string name ^ ".config")
          in
          if OpamFilename.exists src then
            OpamFilename.move ~src ~dst)
        installed)
    aliases;
  config, gtc_none

let v1_3_dev5 = OpamVersion.of_string "1.3~dev5"

let from_1_3_dev2_to_1_3_dev5 ~on_the_fly:_ root conf =
  log "Upgrade switch state files format to 1.3 step 2";
  let aliases_f = OpamFile.make (root // "aliases") in
  let aliases = OpamFile.Aliases.safe_read aliases_f in
  OpamSwitch.Map.iter (fun switch comp_name ->
      (* Convert state-file table format to selections file, opam syntax
         format *)
      let switch_dir = root / OpamSwitch.to_string switch in
      let state_f = OpamFile.make (switch_dir // "state") in
      let selections = OpamFile.LegacyState.safe_read state_f in
      let selections_f = OpamFile.make (switch_dir // "switch-state") in
      let comp_version = match OpamStd.String.cut_at comp_name '+' with
        | Some (v,_) -> v
        | None -> comp_name
      in
      (* Change comp file to a package *)
      let selections =
        if comp_name <> "empty" then
          let comp_f =
            OpamFile.make (root / "compilers" / comp_version /
                           comp_name // (comp_name ^ ".comp"))
          in
          let comp = OpamFile.Comp.read comp_f in
          let descr_f =
            OpamFile.make (root / "compilers" / comp_version /
                           comp_name // (comp_name ^ ".descr"))
          in
          let descr =
            OpamStd.Option.default
              (OpamFile.Descr.create
                 "Switch relying on a system-wide installation of OCaml")
              (OpamFile.Descr.read_opt descr_f)
          in
          let comp_opam =
            OpamFile.Comp.to_package comp (Some descr)
          in
          let nv = OpamFile.OPAM.package comp_opam in
          let name = nv.name in
          let switch_config_f =
            OpamFile.make
              (switch_dir / "config" // "global-config.config")
          in
          let switch_config = OpamFile.Dot_config.safe_read switch_config_f in
          let config =
            if OpamFile.Comp.preinstalled comp then
              let config =
                OpamFile.Dot_config.create @@
                List.map (fun (v,c) -> OpamVariable.of_string v, c) @@
                [ "compiler", S comp_name;
                  "preinstalled", B true;
                ]
              in
              let ocamlc =
                try
                  let path =
                    OpamStd.Env.get "PATH" |>
                    OpamStd.Sys.split_path_variable |>
                    List.filter (fun s ->
                        not (OpamStd.String.starts_with
                               ~prefix:(OpamFilename.Dir.to_string root) s))
                  in
                  List.fold_left (function
                      | None -> fun d ->
                        let f =
                          OpamStd.Sys.executable_name
                            (Filename.concat d "ocamlc")
                        in
                        if Sys.file_exists f
                        then Some (OpamFilename.of_string f)
                        else None
                      | s -> fun _ -> s)
                    None path
                with Not_found -> None
              in
              match ocamlc with
              | Some ocamlc ->
                let vnum =
                  OpamSystem.read_command_output ~verbose:false
                    [ OpamFilename.to_string ocamlc ; "-vnum" ]
                in
                config |>
                OpamFile.Dot_config.with_file_depends
                  [ocamlc, OpamHash.compute (OpamFilename.to_string ocamlc)] |>
                OpamFile.Dot_config.set
                  (OpamVariable.of_string "ocaml-version")
                  (Some (S (String.concat "" vnum)))
              | None -> config
            else
            let get_dir d =
              match OpamFile.Dot_config.variable switch_config
                      (OpamVariable.of_string d)
              with
              | Some (S d) -> OpamFilename.Dir.of_string d
              | _ -> OpamPath.Switch.get_stdpath root switch
                       OpamFile.Switch_config.empty (std_path_of_string d)
            in
            OpamFile.Dot_config.create @@
            List.map (fun (v,c) -> OpamVariable.of_string v, c) @@
            [ "ocaml-version",
              S (OpamFile.Comp.version comp);
              "compiler", S comp_name;
              "preinstalled", B false;
              "ocaml-native",
              B (OpamFilename.exists (get_dir "bin" // "ocamlopt"));
              "ocaml-native-tools",
              B (OpamFilename.exists (get_dir "bin" // "ocamlc.opt"));
              "ocaml-native-dynlink",
              B (OpamFilename.exists
                   (get_dir "lib" / "ocaml" // "dynlink.cmxa"));
              "ocaml-stubsdir",
              S (OpamFilename.Dir.to_string
                   (get_dir "stublibs"));
            ]
          in
          let config_f =
            OpamFile.make
              (switch_dir / "config" //
               (OpamPackage.Name.to_string name ^".config"))
          in
          OpamFile.OPAM.write
            (OpamFile.make
               (root / "packages" / OpamPackage.Name.to_string name
                / OpamPackage.to_string nv // "opam"))
            comp_opam;
          OpamFile.Dot_config.write config_f config;
          (* Also export compiler variables as globals *)
          OpamFile.Dot_config.write switch_config_f
            (OpamFile.Dot_config.with_vars
               (OpamFile.Dot_config.bindings switch_config @
                OpamFile.Dot_config.bindings config)
               switch_config);
          { selections with
            sel_installed = OpamPackage.Set.add nv selections.sel_installed;
            sel_compiler = OpamPackage.Set.add nv selections.sel_compiler;
            sel_roots = OpamPackage.Set.add nv selections.sel_roots; }
        else selections
      in
      OpamFile.SwitchSelections.write selections_f selections;
      OpamFilename.remove (OpamFile.filename state_f))
    aliases;
  let conf =
    OpamFile.Config.with_installed_switches (OpamSwitch.Map.keys aliases) conf
  in
  OpamFilename.remove (OpamFile.filename aliases_f);
  conf, gtc_none

let v1_3_dev6 = OpamVersion.of_string "1.3~dev6"

let from_1_3_dev5_to_1_3_dev6 ~on_the_fly:_ root conf =
  log "Upgrade switch state files format to 1.3 step 3";
  (* Move switch internals to [switch/.opam-switch] *)
  List.iter (fun switch ->
      let switch_dir = root / OpamSwitch.to_string switch in
      let meta_dir =  switch_dir / ".opam-switch" in
      OpamFilename.mkdir meta_dir;
      List.iter (fun f ->
          let src = switch_dir // f in
          let dst = meta_dir // f in
          if OpamFilename.exists src then OpamFilename.move ~src ~dst)
        ["lock"; "switch-state"; "reinstall"; "environment"];
      List.iter (fun d ->
          let src = switch_dir / d in
          let dst = meta_dir / d in
          if OpamFilename.exists_dir src then OpamFilename.move_dir ~src ~dst)
        ["backup"; "build"; "install"; "config"; "packages.dev"; "overlay"]
    )
    (OpamFile.Config.installed_switches conf);
  conf, gtc_none

let v1_3_dev7 = OpamVersion.of_string "1.3~dev7"

let from_1_3_dev6_to_1_3_dev7 ~on_the_fly:_ root conf =
  log "Upgrade switch state files format to 1.3 step 4";
  (* Get mirrors of the metadata of all installed packages into
     switch_meta_dir/packages *)
  List.iter (fun switch ->
      let switch_dir = root / OpamSwitch.to_string switch in
      let meta_dir =  switch_dir / ".opam-switch" in
      let installed =
        (OpamFile.SwitchSelections.BestEffort.safe_read
           (OpamFile.make (meta_dir // "switch-state")))
        .sel_installed
      in
      OpamFilename.mkdir (meta_dir / "packages");
      OpamPackage.Set.iter (fun nv ->
          let dstdir =
            meta_dir / "packages" / OpamPackage.to_string nv
          in
          try
            let srcdir =
              List.find (fun d -> OpamFilename.exists (d // "opam")) [
                meta_dir / "overlay" / OpamPackage.Name.to_string nv.name;
                root / "packages" / OpamPackage.Name.to_string nv.name /
                OpamPackage.to_string nv;
              ]
            in
            match OpamFileTools.read_opam srcdir with
            | Some opam ->
              OpamFile.OPAM.write (OpamFile.make (dstdir // "opam")) opam;
              OpamStd.Option.iter (fun src ->
                  OpamFilename.copy_dir ~src ~dst:(dstdir / "files"))
                (OpamFilename.opt_dir (srcdir / "files"))
            | None -> raise Not_found
          with Not_found ->
            OpamFile.OPAM.write (OpamFile.make (dstdir // "opam"))
              (OpamFile.OPAM.create nv)
        )
        installed)
    (OpamFile.Config.installed_switches conf);
  OpamFilename.rmdir (root / "packages");
  OpamFilename.rmdir (root / "packages.dev");
  OpamFilename.rmdir (root / "state.cache");
  conf, gtc_none

let v2_0_alpha = OpamVersion.of_string "2.0~alpha"

let from_1_3_dev7_to_2_0_alpha ~on_the_fly:_ root conf =
  log "Upgrade switch state files format to 2.0~alpha";
  (* leftovers from previous upgrades *)
  OpamFilename.rmdir (root / "compilers");
  OpamFilename.remove (root / "repo" // "package-index");
  OpamFilename.remove (root / "repo" // "compiler-index");
  (* turn repo priorities into an ordered list in ~/.opam/config, repo conf
     files into a single file repo/repos-config *)
  let prio_repositories =
    List.map (fun name ->
        let conf_file =
          OpamFile.make
            (root / "repo" / OpamRepositoryName.to_string name // "config")
        in
        let module RCL = OpamFile.Repo_config_legacy in
        let conf = RCL.read conf_file in
        OpamFilename.remove (OpamFile.filename conf_file);
        conf.RCL.repo_priority, name, conf.RCL.repo_url)
      (OpamFile.Config.repositories conf)
  in
  OpamFile.Repos_config.write (OpamPath.repos_config root)
    (OpamRepositoryName.Map.of_list
       (List.map (fun (_, r, u) -> r, Some (u,None)) prio_repositories));
  let prio_repositories =
    List.stable_sort (fun (prio1, _, _) (prio2, _, _) -> prio2 - prio1)
      prio_repositories
  in
  let repositories_list = List.map (fun (_, r, _) -> r) prio_repositories in
  OpamFile.Config.with_repositories repositories_list conf
  |> OpamFile.Config.with_opam_version v2_0, gtc_none

let v2_0_alpha2 = OpamVersion.of_string "2.0~alpha2"

let from_2_0_alpha_to_2_0_alpha2 ~on_the_fly:_ root conf =
  List.iter (fun switch ->
      let switch_dir = root / OpamSwitch.to_string switch in
      let meta_dir =  switch_dir / ".opam-switch" in

      (* Cleanup exported variables from the switch config (they are now
         defined in wrapper package 'ocaml', and accessed as e.g.
         'ocaml:native-dynlink') *)
      let to_remove_vars = List.map OpamVariable.of_string [
          "ocaml-version";
          "compiler";
          "preinstalled";
          "ocaml-native";
          "ocaml-native-tools";
          "ocaml-native-dynlink";
          "ocaml-stubsdir";
        ] in
      let remove_vars config =
        OpamFile.Dot_config.with_vars
          (List.filter (fun (var, _) -> not (List.mem var to_remove_vars))
             (OpamFile.Dot_config.bindings config))
          config
      in
      let switch_config_f =
        OpamFile.make
          (meta_dir / "config" // "global-config.config")
      in
      let switch_config = OpamFile.Dot_config.safe_read switch_config_f in
      OpamFile.Dot_config.write switch_config_f (remove_vars switch_config);

      (* Rename the 'ocaml' compiler packages to their proper instance (and
         let the wrapper 'ocaml' package be pulled from the repository later
         on to detect and set the 'ocaml:*' variables *)
      let selections_file = OpamFile.make (meta_dir // "switch-state") in
      let selections = OpamFile.SwitchSelections.BestEffort.safe_read selections_file in
      let new_compilers =
        OpamPackage.Set.map (fun nv ->
            if nv.name <> OpamPackage.Name.of_string "ocaml" then nv else
            let config_f nv =
              OpamFile.make (meta_dir / "config" //
                             (OpamPackage.Name.to_string nv.name ^ ".config"))
            in
            let config = OpamFile.Dot_config.safe_read (config_f nv) in
            let ocaml_version_var = OpamVariable.of_string "ocaml-version" in
            let ocaml_version =
              match
                OpamFile.Dot_config.variable switch_config ocaml_version_var
              with
              | Some (S v) -> OpamPackage.Version.of_string v
              | _ ->
                match
                  OpamFile.Dot_config.variable config ocaml_version_var
                with
                | Some (S v) -> OpamPackage.Version.of_string v
                | _ -> nv.version
            in
            let full_version = OpamPackage.Version.to_string nv.version in
            let name, version =
              match OpamStd.String.cut_at full_version '+' with
              | None when full_version = "system" ->
                OpamPackage.Name.of_string "ocaml-system", ocaml_version
              | None ->
                OpamPackage.Name.of_string "ocaml-base-compiler",
                ocaml_version
              | Some (_version, _variant) ->
                OpamPackage.Name.of_string "ocaml-variants",
                OpamPackage.Version.of_string full_version
            in
            let new_nv = OpamPackage.create name version in
            let pkgdir nv = meta_dir / "packages" / OpamPackage.to_string nv in
            if OpamFilename.exists_dir (pkgdir nv) then
              OpamFilename.move_dir ~src:(pkgdir nv) ~dst:(pkgdir new_nv);
            OpamStd.Option.Op.(
              OpamFilename.opt_file (pkgdir new_nv // "opam") >>|
              OpamFile.make >>= fun f ->
              OpamFile.OPAM.read_opt f >>|
              opam_file_from_1_2_to_2_0 ~filename:f >>|
              OpamFile.OPAM.write_with_preserved_format f
            ) |> ignore;
            if OpamFile.exists (config_f nv) then
              (OpamFile.Dot_config.write (config_f new_nv)
                 (remove_vars config);
               OpamFilename.remove (OpamFile.filename (config_f nv)));
            let install_f nv =
              meta_dir / "install" //
              (OpamPackage.Name.to_string nv.name ^ ".install")
            in
            if OpamFilename.exists (install_f nv) then
              OpamFilename.move ~src:(install_f nv) ~dst:(install_f new_nv);
            let changes_f nv =
              meta_dir / "install" //
              (OpamPackage.Name.to_string nv.name ^ ".changes")
            in
            if OpamFilename.exists (changes_f nv) then
              OpamFilename.move ~src:(changes_f nv) ~dst:(changes_f new_nv);
            new_nv
          )
          selections.sel_compiler
      in
      let selections =
        let open OpamPackage.Set.Op in
        { selections with
          sel_installed = selections.sel_installed
                          -- selections.sel_compiler ++ new_compilers;
          sel_roots = selections.sel_roots
                      -- selections.sel_compiler ++ new_compilers;
          sel_compiler = new_compilers }
      in
      OpamFile.SwitchSelections.write selections_file selections;

      (* Update pinned overlay opam files *)
      OpamPackage.Set.iter (fun nv ->
          let pkg_dir =
            meta_dir / "overlay" / OpamPackage.Name.to_string nv.name
          in
          let opamf = pkg_dir // "opam" in
          let opam0 = OpamFile.make opamf in
          OpamStd.Option.iter (fun opam ->
              opam_file_from_1_2_to_2_0 ~filename:opam0 opam
              |> OpamFile.OPAM.write_with_preserved_format opam0;
              OpamFilename.remove (pkg_dir // "descr");
              OpamFilename.remove (pkg_dir // "url")
            ) (OpamFileTools.read_opam pkg_dir)
        ) selections.sel_pinned;
    )
    (OpamFile.Config.installed_switches conf);
  OpamFile.Config.with_eval_variables [
    OpamVariable.of_string "sys-ocaml-version", ["ocamlc"; "-vnum"],
    "OCaml version present on your system independently of opam, if any";
  ] conf, gtc_none

let v2_0_alpha3 = OpamVersion.of_string "2.0~alpha3"

let from_2_0_alpha2_to_2_0_alpha3 ~on_the_fly:_ root conf =
  List.iter (fun switch ->
      let switch_dir = root / OpamSwitch.to_string switch in
      let old_global_config =
        switch_dir / ".opam-switch" / "config" // "global-config.config"
      in
      match OpamFile.Dot_config.read_opt (OpamFile.make old_global_config) with
      | None -> ()
      | Some oldconf ->
        let new_config_file = switch_dir / ".opam-switch" // "switch-config" in
        let opam_root, paths, variables =
          List.fold_left (fun (root, paths, variables) (var, value) ->
              match OpamVariable.to_string var, value with
              | "root", S r ->
                (Some (OpamFilename.Dir.of_string r), paths, variables)
              | stdpath, S d when
                  (try ignore (std_path_of_string stdpath); true
                   with Failure _ -> false) ->
                root, (std_path_of_string stdpath, d) :: paths, variables
              | _, value -> root, paths, (var, value) :: variables)
            (None, [], [])
            (OpamFile.Dot_config.bindings oldconf)
        in
        let new_config =
          { OpamFile.Switch_config.
            opam_version = OpamVersion.nopatch v2_0_alpha3;
            synopsis = "";
            repos = None;
            opam_root; paths; variables; wrappers = OpamFile.Wrappers.empty;
            env = [];
            invariant = None;
            depext_bypass = OpamSysPkg.Set.empty;
          }
        in
        OpamFile.Switch_config.write (OpamFile.make new_config_file) new_config;
        OpamFilename.remove old_global_config
    )
    (OpamFile.Config.installed_switches conf);
  conf, gtc_none

let v2_0_beta = OpamVersion.of_string "2.0~beta"

let from_2_0_alpha3_to_2_0_beta ~on_the_fly:_ root conf =
  List.iter (fun switch ->
      let switch_meta_dir =
        root / OpamSwitch.to_string switch / ".opam-switch"
      in
      let packages_dev_dir = switch_meta_dir / "packages.dev" in (* old *)
      let sources_dir = switch_meta_dir / "sources" in (* new *)
      let state =
        OpamFile.SwitchSelections.BestEffort.safe_read
          (OpamFile.make (switch_meta_dir // "switch-state"))
      in
      OpamFilename.mkdir sources_dir;
      List.iter (fun d ->
          try
            let name =
              OpamPackage.Name.of_string
                OpamFilename.(Base.to_string (basename_dir d))
            in
            let dst =
              if OpamPackage.has_name state.sel_pinned name then
                sources_dir / OpamPackage.Name.to_string name
              else
                let nv = OpamPackage.package_of_name state.sel_installed name in
                sources_dir / OpamPackage.to_string nv
            in
            (* Extract version-pinned archives to source dirs *)
            match OpamFilename.files d with
            | file::[] when OpamFilename.is_archive file ->
              OpamFilename.extract_in file dst;
              OpamFilename.remove file
            | _ -> ()
          with Failure _ | Not_found -> ()
        )
        (OpamFilename.dirs packages_dev_dir);
      OpamFilename.rmdir packages_dev_dir;
    )
    (OpamFile.Config.installed_switches conf);
  (if OpamFile.Config.default_compiler conf <> Empty then conf
   else
     OpamFile.Config.with_default_compiler
       (OpamFormula.ors [
           OpamFormula.Atom (OpamPackage.Name.of_string "ocaml-system",
                             OpamFormula.Atom
                               (`Geq, OpamPackage.Version.of_string "4.02.3"));
           OpamFormula.Atom (OpamPackage.Name.of_string "ocaml-base-compiler",
                             OpamFormula.Empty);
         ])
       conf) |>
  OpamFile.Config.with_eval_variables
    ((OpamVariable.of_string "arch", ["uname"; "-m"],
      "Host architecture, as returned by 'uname -m'")
     :: OpamFile.Config.eval_variables conf), gtc_none

let v2_0_beta5 = OpamVersion.of_string "2.0~beta5"

let from_2_0_beta_to_2_0_beta5 ~on_the_fly:_ root conf =
  List.iter (fun switch ->
      let switch_meta_dir =
        root / OpamSwitch.to_string switch / ".opam-switch"
      in
      let switch_config = OpamFile.make (switch_meta_dir // "switch-config") in
      let module C = OpamFile.Switch_config in
      let config = C.BestEffort.safe_read switch_config in
      let rem_variables = List.map OpamVariable.of_string ["os"; "make"] in
      let config =
        { config with
          C.variables =
            List.filter (fun (var,_) -> not (List.mem var rem_variables))
              config.C.variables;
        }
      in
      OpamFile.Switch_config.write switch_config config;
      let opam_files_dirs =
        OpamFilename.dirs (switch_meta_dir / "packages") @
        OpamFilename.dirs (switch_meta_dir / "overlay")
      in
      List.iter (fun d ->
          let opam = OpamFile.make (d // "opam") in
          try
            OpamFile.OPAM.read opam |> fun o ->
            OpamFile.OPAM.with_depexts
              (upgrade_depexts_to_2_0_beta5 (OpamFile.to_string opam)
                 (OpamFile.OPAM.depexts o))
              o |>
            OpamFile.OPAM.write_with_preserved_format opam
          with e -> OpamStd.Exn.fatal e)
        opam_files_dirs
    )
    (OpamFile.Config.installed_switches conf);
  let rem_eval_variables = List.map OpamVariable.of_string ["arch"] in
  OpamFile.Config.with_eval_variables
    (List.filter (fun (v,_,_) -> not (List.mem v rem_eval_variables))
       (OpamFile.Config.eval_variables conf))
    conf, gtc_none

let from_2_0_beta5_to_2_0 ~on_the_fly:_ _ conf = conf, gtc_none

(* swiitch config with opam-version 2.1 *)
let v2_1_alpha = OpamVersion.of_string "2.1~alpha"
(* config with opam-version 2.1 *)
let v2_1_alpha2 = OpamVersion.of_string "2.1~alpha2"
(* config & sw config downgrade opam-version to 2.0 and add opam root version *)
let v2_1_rc = OpamVersion.of_string "2.1~rc"

let v2_1 = OpamVersion.of_string "2.1"

let from_2_0_to_2_1_alpha ~on_the_fly:_ _ conf = conf, gtc_none

let downgrade_2_1_switches root conf =
  List.iter (fun switch ->
      let f = OpamPath.Switch.switch_config root switch in
      OpamStd.Option.iter (OpamFile.Switch_config.write f)
        (OpamStateConfig.downgrade_2_1_switch f))
    (OpamFile.Config.installed_switches conf);
  conf

let from_2_1_alpha_to_2_1_alpha2 ~on_the_fly:_ root conf =
  downgrade_2_1_switches root conf, gtc_none

let from_2_1_alpha2_to_2_1_rc ~on_the_fly:_ root conf =
  downgrade_2_1_switches root conf, gtc_none

let from_2_1_rc_to_2_1 ~on_the_fly:_ _ conf = conf, gtc_none

let from_2_0_to_2_1 ~on_the_fly _ conf =
  (* In opam < 2.1 "jobs" was set during initialisation
     This creates problems when upgrading from opam 2.0 as it
     sets the job count for good even if the CPU is replaced.
     See https://github.com/ocurrent/ocaml-dockerfile/pull/92 *)
  let info_jobs_changed ~prev_jobs =
    if not on_the_fly then
      OpamConsole.note
        "The 'jobs' option was reset, its value was %d and its new value \
         will vary according to the current number of cores on your machine. \
         You can restore the fixed value using:\n    \
         opam option jobs=%d --global" prev_jobs prev_jobs;
  in
  (* We print a note in case the previous value of 'jobs' does not
     match the default in opam 2.0 as we can't determine if the value
     was modified or if the hardware changed. *)
  (match OpamFile.Config.jobs conf with
   | Some prev_jobs when prev_jobs = max 1 (OpamSysPoll.cores () - 1) -> ()
   | Some prev_jobs -> info_jobs_changed ~prev_jobs
   | None -> info_jobs_changed ~prev_jobs:1);
  OpamFile.Config.with_jobs_opt None conf, gtc_none

let v2_2_alpha = OpamVersion.of_string "2.2~alpha"

let from_2_1_to_2_2_alpha ~on_the_fly:_ _ conf = conf, gtc_none

(* To add an upgrade layer
   * If it is a light upgrade, returns as second element if the repo or switch
     need an light upgrade with `gtc_*` values.
   * If it is an hard upgrade, performs repo & switch upgrade in upgrade
     function.
*)

let latest_version = OpamFile.Config.root_version

let latest_hard_upgrade = (* to *) v2_0_beta5

(* intermediate roots that need a hard upgrade when upgrading from them *)
let v2_1_intermediate_roots = [
  v2_1_alpha; v2_1_alpha2; v2_1_rc
]

let remove_missing_switches root conf =
  let exists, missing =
    List.partition (fun switch ->
        OpamFilename.exists (OpamFile.filename
                               (OpamPath.Switch.switch_config root switch)))
      (OpamFile.Config.installed_switches conf)
  in
  OpamFile.Config.with_installed_switches exists conf, missing

let erase_plugin_links root =
  let plugins_bin = OpamPath.plugins_bin root in
  if OpamFilename.exists_dir plugins_bin then
    List.iter OpamFilename.remove @@ OpamFilename.files_and_links plugins_bin

let flock_root =
  let dontblock =
    let t = lazy (
      (* Deadlock until one is killed in interactive mode, but abort in batch *)
      if OpamStd.Sys.tty_out then None else Some true
    ) in
    fun () -> Lazy.force t
  in
  fun ?global_lock kind root ->
    try
      let global_lock = match global_lock with
        | Some g -> g
        | None -> OpamFilename.flock `Lock_read (OpamPath.lock root)
      in
      OpamFilename.with_flock_upgrade kind ?dontblock:(dontblock ()) global_lock
    with OpamSystem.Locked ->
      OpamConsole.error_and_exit `Locked
        "Could not acquire lock for performing format upgrade."

let as_necessary ?reinit requested_lock global_lock root config =
  let root_version =
    match OpamFile.Config.opam_root_version_opt config with
    | Some v -> v
    | None ->
      let v = OpamFile.Config.opam_version config in
      if OpamVersion.compare v v2_0 <> 0 then v else
      try
        List.iter (fun switch ->
            ignore @@
            OpamFile.Switch_config.read_opt
              (OpamPath.Switch.switch_config root switch))
          (OpamFile.Config.installed_switches config);
        v
      with Sys_error _ | OpamPp.Bad_version _ -> v2_1_alpha
  in
  let cmp = OpamVersion.(compare OpamFile.Config.root_version root_version) in
  if cmp <= 0 then config, gtc_none (* newer or same *) else
  let hard_upg, light_upg =
    let is_2_1_intermediate_root =
      List.exists (OpamVersion.equal root_version) v2_1_intermediate_roots
    in
    let latest_hard_upgrade =
      if is_2_1_intermediate_root then v2_1_rc else latest_hard_upgrade
    in
    (if is_2_1_intermediate_root then [
        v2_1_alpha,  from_2_0_to_2_1_alpha;
        v2_1_alpha2, from_2_1_alpha_to_2_1_alpha2;
        v2_1_rc,     from_2_1_alpha2_to_2_1_rc;
        v2_1,        from_2_1_rc_to_2_1;
      ] else [
       v1_1,        from_1_0_to_1_1;
       v1_2,        from_1_1_to_1_2;
       v1_3_dev2,   from_1_2_to_1_3_dev2;
       v1_3_dev5,   from_1_3_dev2_to_1_3_dev5;
       v1_3_dev6,   from_1_3_dev5_to_1_3_dev6;
       v1_3_dev7,   from_1_3_dev6_to_1_3_dev7;
       v2_0_alpha,  from_1_3_dev7_to_2_0_alpha;
       v2_0_alpha2, from_2_0_alpha_to_2_0_alpha2;
       v2_0_alpha3, from_2_0_alpha2_to_2_0_alpha3;
       v2_0_beta,   from_2_0_alpha3_to_2_0_beta;
       v2_0_beta5,  from_2_0_beta_to_2_0_beta5;
       v2_0,        from_2_0_beta5_to_2_0;
       v2_1,        from_2_0_to_2_1;
     ]) @ [
      v2_2_alpha,  from_2_1_to_2_2_alpha;
    ]
    |> List.filter (fun (v,_) ->
        OpamVersion.compare root_version v < 0)
    |> List.partition (fun (v,_) ->
        OpamVersion.compare v latest_hard_upgrade <= 0)
  in
  let need_hard_upg = hard_upg <> [] in
  let on_the_fly, global_lock_kind =
    if not need_hard_upg && requested_lock <> `Lock_write then
      true, `Lock_read
    else
      false, `Lock_write
  in
  let add_chg changes gtc = {
    gtc_repo = changes.gtc_repo || gtc.gtc_repo;
    gtc_switch = changes.gtc_switch || gtc.gtc_switch;
  } in
  let light config =
    let config, changes =
      List.fold_left (fun (config, changes) (v, from) ->
          let config, change = from ~on_the_fly root config in
          config |> OpamFile.Config.with_opam_root_version v,
          add_chg changes change)
        (config, gtc_none) light_upg
    in
    if not on_the_fly then begin
      OpamFile.Config.write (OpamPath.config root) config;
      erase_plugin_links root;
    end;
    config, if on_the_fly then changes else gtc_none
  in
  let hard config =
    List.fold_left (fun config (v, from) ->
        let config, _change = from ~on_the_fly root config in
        let config = config |> OpamFile.Config.with_opam_root_version v in
        (* save the current version to mitigate damage if the upgrade goes
             wrong afterwards *)
        OpamFile.Config.write (OpamPath.config root) config;
        erase_plugin_links root;
        config)
      config hard_upg
  in
  let config =
    let config, missing_switches = remove_missing_switches root config in
    let global = List.filter (OpamSwitch.is_external @> not) missing_switches in
    if not on_the_fly && global <> [] then
      OpamConsole.warning "Removing global switch%s %s as %s"
        (match global with | [_] -> "" | _ -> "es")
        (OpamStd.Format.pretty_list
           (List.map (OpamSwitch.to_string
                      @> OpamConsole.colorise `bold
                      @> Printf.sprintf "'%s'")
              global))
        (match global with
         | [_] -> "it no longer exists"
         | _ -> "they no longer exist");
    config
  in
  if hard_upg = [] && light_upg = [] then config, gtc_none (* no upgrade to do *) else
  let is_dev = OpamVersion.is_dev_version () in
  log "%s config upgrade, from %s to %s"
    (if on_the_fly then "On-the-fly" else
     if need_hard_upg then "Hard" else "Light")
    (OpamVersion.to_string root_version)
    (OpamVersion.to_string latest_version);
  if not on_the_fly then
    OpamConsole.errmsg "%s\n" @@
    OpamStd.Format.reformat @@
    Printf.sprintf
      "This %sversion of opam requires an update to the layout of %s \
       from version %s to version %s, which can't be reverted.\n\
       You may want to back it up before going further.\n"
      (if is_dev then "development " else "")
      (OpamFilename.Dir.to_string root)
      (OpamVersion.to_string root_version)
      (OpamVersion.to_string latest_version);
  flock_root global_lock_kind ~global_lock root @@ fun _ ->
  if not on_the_fly then
    if need_hard_upg then
      if is_dev &&
         Some "yes" =
         OpamConsole.read "Type \"yes\" to perform the update and continue:"
      || not is_dev &&
         OpamConsole.confirm "Perform the update and continue?"
      then
        let config, _changes = hard config |> light in
        OpamConsole.msg "Format upgrade done.\n";
        (* We need to re run init in case of hard upgrade *)
        raise (Upgrade_done (config, reinit))
      else
        OpamStd.Sys.exit_because `Aborted
    else
    if OpamConsole.confirm "Continue?" then
      (let config, _changes = light config in
       OpamConsole.msg "Format upgrade done.\n";
       config, gtc_none)
    else
      OpamStd.Sys.exit_because `Aborted
  else
    (let config, changes = light config in
     log "Format upgrade done";
     config, changes)

let as_necessary_repo_switch_light_upgrade lock_kind kind gt =
  let { gtc_repo; gtc_switch } = gt.global_state_to_upgrade in
  (* No upgrade to do *)
  if not gtc_repo && not gtc_switch then () else
  let config_f = OpamPath.config gt.root in
  let written_root_version = OpamFile.Config.raw_root_version config_f in
  (* Config already upgraded *)
  if OpamStd.Option.equal OpamVersion.equal
      written_root_version
      (Some OpamFile.Config.root_version) then () else
  match lock_kind, kind with
  (* ro repo & rw switch & only repo changes case *)
  | `Lock_write, `Switch when not gtc_switch && gtc_repo -> ()
  | `Lock_write, _ ->
    let is_dev = OpamVersion.is_dev_version () in
    OpamConsole.errmsg "%s" @@
    OpamStd.Format.reformat @@
    Printf.sprintf
      "This %sversion of opam requires an update to the layout of %s \
       from version %s to version %s, which can't be reverted.\n\
       You may want to back it up before going further.\n"
      (if is_dev then "development " else "")
      (OpamFilename.Dir.to_string gt.root)
      OpamStd.Option.Op.((written_root_version >>| OpamVersion.to_string) +! "2.0")
      (OpamVersion.to_string (OpamFile.Config.opam_root_version gt.config));
    if OpamConsole.confirm "Continue?" then
      flock_root `Lock_write gt.root @@ fun _ ->
      OpamFile.Config.write config_f gt.config;
      erase_plugin_links gt.root
    else
      OpamStd.Sys.exit_because `Aborted
  | _, _ -> ()

let hard_upgrade_from_2_1_intermediates ?reinit ?global_lock root =
  let config_f = OpamPath.config root in
  let opam_root_version = OpamFile.Config.raw_root_version config_f in
  match opam_root_version with
  | Some v when OpamVersion.compare v v2_0 <= 0
             || OpamVersion.compare v2_1 v <= 0 ->
    () (* do nothing, need to reraise parsing exception *)
  | _ ->
    log "Intermediate opam root detected%s, launch hard upgrade"
      (match opam_root_version with
         None -> ""
       | Some v -> "("^(OpamVersion.to_string v)^")");
    let filename = OpamFile.filename config_f in
    let opamfile = OpamParser.FullPos.file (OpamFilename.to_string filename) in
    let opamfile' =
      let open OpamParserTypes.FullPos in
      { opamfile with
        file_contents =
          List.map (fun item ->
              match item.pelem with
              | Variable (({pelem = "opam-version"; _} as opam_v),
                          ({pelem = String "2.1"; _} as v)) ->
                { item with
                  pelem = Variable ({opam_v with pelem = "opam-version"},
                                    {v with pelem = String "2.0"})}
              | _ -> item) opamfile.file_contents}
    in
    log "Downgrade config opam-version to fix up";
    OpamFilename.write filename (OpamPrinter.FullPos.opamfile opamfile');
    let config = OpamFile.Config.read config_f in
    let global_lock = match global_lock with
      | Some g -> g
      | None -> OpamFilename.flock `Lock_read (OpamPath.lock root)
    in
    (* it will trigger only hard upgrades that won't get back *)
    ignore @@ as_necessary `Lock_write global_lock root ?reinit
      (OpamFile.Config.with_opam_root_version v2_1_alpha2 config)

let opam_file ?(quiet=false) ?filename opam =
  let v = OpamFile.OPAM.opam_version opam in
  if OpamVersion.compare v v2_0_alpha3 < 0
  then
    ((match filename with
        | Some f when not quiet ->
          log "Internally converting format of %a from %a to %a"
            (slog OpamFile.to_string) f
            (slog OpamVersion.to_string) v
            (slog OpamVersion.to_string) latest_version
        | _ -> ());
     opam_file_from_1_2_to_2_0 ?filename opam)
  else opam

let opam_file_with_aux ?(quiet=false) ?dir ~files ?filename opam =
  let opam = OpamFileTools.add_aux_files ?dir ~files_subdir_hashes:files opam in
  opam_file ~quiet ?filename opam

let comp_file ?package ?descr comp =
  OpamFile.Comp.to_package ?package comp descr
  |> opam_file_from_1_2_to_2_0
