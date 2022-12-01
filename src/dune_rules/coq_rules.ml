open Import
open Memo.O

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2020                              *)
(* Written by: Emilio Jesús Gallego Arias *)
(* Written by: Rudi Grinberg *)

module Bootstrap = struct
  (* the internal boot flag determines if the Coq "standard library" is being
     built, in case we need to explicitly tell Coq where the build artifacts are
     and add `Init.Prelude.vo` as a dependency; there is a further special case
     when compiling the prelude, in this case we also need to tell Coq not to
     try to load the prelude. *)
  type t =
    | No_boot  (** Coq's stdlib is installed globally *)
    | Bootstrap of Coq_lib.t
        (** Coq's stdlib is in scope of the composed build *)
    | Bootstrap_prelude
        (** We are compiling the prelude itself
            [should be replaced with (per_file ...) flags] *)

  let flags ~coqdoc t : _ Command.Args.t =
    match t with
    | No_boot -> Command.Args.empty
    | Bootstrap lib ->
      if coqdoc then
        S [ A "--coqlib"; Path (Path.build @@ Coq_lib.src_root lib) ]
      else A "-boot"
    | Bootstrap_prelude -> As [ "-boot"; "-noinit" ]
end

let coqc ~loc ~dir ~sctx =
  Super_context.resolve_program sctx "coqc" ~dir ~loc:(Some loc)
    ~hint:"opam install coq"

let select_native_mode ~sctx ~dir (buildable : Coq_stanza.Buildable.t) =
  match buildable.mode with
  | Some x ->
    if
      buildable.coq_lang_version < (0, 7)
      && Profile.is_dev (Super_context.context sctx).profile
    then Memo.return Coq_mode.VoOnly
    else Memo.return x
  | None -> (
    if buildable.coq_lang_version < (0, 3) then Memo.return Coq_mode.Legacy
    else if buildable.coq_lang_version < (0, 7) then Memo.return Coq_mode.VoOnly
    else
      let* coqc = coqc ~sctx ~dir ~loc:buildable.loc in
      let+ config = Coq_config.make_opt ~coqc in
      match config with
      | None -> Coq_mode.VoOnly
      | Some config -> (
        match Coq_config.by_name config "coq_native_compiler_default" with
        | Some (`String "yes") | Some (`String "ondemand") -> Coq_mode.Native
        | _ -> Coq_mode.VoOnly))

(* CR alizter: move this to Lib.DB *)

(** Given a list of library names, we try to resolve them in order, returning
    the first one that exists. *)
let rec resolve_first lib_db = function
  | [] -> Code_error.raise "resolve_first: empty list" []
  | [ n ] -> Lib.DB.resolve lib_db (Loc.none, Lib_name.of_string n)
  | n :: l -> (
    let open Memo.O in
    Lib.DB.resolve_when_exists lib_db (Loc.none, Lib_name.of_string n)
    >>= function
    | Some l -> Resolve.Memo.lift l
    | None -> resolve_first lib_db l)

let coq_flags ~dir ~stanza_flags ~expander ~sctx =
  let open Action_builder.O in
  let* standard = Action_builder.of_memo @@ Super_context.coq ~dir sctx in
  Expander.expand_and_eval_set expander stanza_flags ~standard

let theories_flags ~theories_deps =
  let theory_coqc_flag lib =
    let dir = Coq_lib.src_root lib in
    let binding_flag = if Coq_lib.implicit lib then "-R" else "-Q" in
    Command.Args.S
      [ A binding_flag
      ; Path (Path.build dir)
      ; A (Coq_lib.name lib |> Coq_lib_name.wrapper)
      ]
  in
  Resolve.Memo.args
    (let open Resolve.Memo.O in
    let+ libs = theories_deps in
    Command.Args.S (List.map ~f:theory_coqc_flag libs))

let coqc_file_flags ~dir ~theories_deps ~wrapper_name ~boot_type ~ml_flags :
    _ Command.Args.t list =
  let file_flags : _ Command.Args.t list =
    [ Dyn (Resolve.Memo.read ml_flags)
    ; theories_flags ~theories_deps
    ; A "-R"
    ; Path (Path.build dir)
    ; A wrapper_name
    ]
  in
  [ Bootstrap.flags ~coqdoc:false boot_type; S file_flags ]

let native_includes ~dir =
  let* scope = Scope.DB.find_by_dir dir in
  let lib_db = Scope.libs scope in
  (* We want the cmi files *)
  Resolve.Memo.map ~f:(fun lib ->
      let info = Lib.info lib in
      let obj_dir = Obj_dir.public_cmi_ocaml_dir (Lib_info.obj_dir info) in
      Path.Set.singleton obj_dir)
  @@ resolve_first lib_db [ "coq-core.kernel"; "coq.kernel" ]

let directories_of_lib ~sctx lib =
  let name = Coq_lib.name lib in
  let dir = Coq_lib.src_root lib in
  let* dir_contents = Dir_contents.get sctx ~dir in
  let+ coq_sources = Dir_contents.coq dir_contents in
  Coq_sources.directories coq_sources ~name

let setup_native_theory_includes ~sctx ~theories_deps ~theory_dirs =
  Resolve.Memo.bind theories_deps ~f:(fun theories_deps ->
      let+ l =
        Memo.parallel_map theories_deps ~f:(fun lib ->
            let+ theory_dirs = directories_of_lib ~sctx lib in
            Path.Build.Set.of_list theory_dirs)
      in
      Resolve.return (Path.Build.Set.union_all (theory_dirs :: l)))

let coqc_native_flags ~sctx ~dir ~theories_deps ~theory_dirs
    ~(mode : Coq_mode.t) =
  match mode with
  | Legacy -> Command.Args.empty
  | VoOnly ->
    Command.Args.As
      [ "-w"
      ; "-deprecated-native-compiler-option"
      ; "-w"
      ; "-native-compiler-disabled"
      ; "-native-compiler"
      ; "ondemand"
      ]
  | Native ->
    let args =
      let open Action_builder.O in
      let* native_includes = Resolve.Memo.read @@ native_includes ~dir in
      let+ native_theory_includes =
        Resolve.Memo.read
        @@ setup_native_theory_includes ~sctx ~theories_deps ~theory_dirs
      in
      let include_ dir acc = Command.Args.Path dir :: A "-nI" :: acc in
      let native_include_ml_args =
        Path.Set.fold native_includes ~init:[] ~f:include_
      in
      let native_include_theory_output =
        Path.Build.Set.fold native_theory_includes ~init:[] ~f:(fun dir acc ->
            include_ (Path.build dir) acc)
      in
      (* This dir is relative to the file, by default [.coq-native/] *)
      Command.Args.S
        [ Command.Args.As [ "-w"; "-deprecated-native-compiler-option" ]
        ; As [ "-native-output-dir"; "." ]
        ; As [ "-native-compiler"; "on" ]
        ; S (List.rev native_include_ml_args)
        ; S (List.rev native_include_theory_output)
        ]
    in
    Command.Args.Dyn args

let theories_deps_requires_for_user_written ~dir
    (buildable : Coq_stanza.Buildable.t) =
  let open Memo.O in
  let* scope = Scope.DB.find_by_dir dir in
  let coq_lib_db = Scope.coq_libs scope in
  Coq_lib.DB.requires_for_user_written coq_lib_db buildable.theories
    ~coq_lang_version:buildable.coq_lang_version

(* compute include flags and mlpack rules *)
let plugins_of_buildable ~context ~lib_db ~theories_deps
    (buildable : Coq_stanza.Buildable.t) =
  let res =
    let open Resolve.Memo.O in
    let+ libs =
      Resolve.Memo.List.map buildable.plugins ~f:(fun (loc, name) ->
          let+ lib = Lib.DB.resolve lib_db (loc, name) in
          (loc, lib))
    in
    let coq_lang_version = buildable.coq_lang_version in
    let plugin_loc = List.hd_opt buildable.plugins |> Option.map ~f:fst in
    (* Pair of include flags and paths to mlpack *)
    let libs =
      let* theories = theories_deps in
      let* theories =
        Resolve.Memo.lift
        @@ Resolve.List.concat_map ~f:Coq_lib.libraries theories
      in
      let libs = libs @ theories in
      Lib.closure ~linking:false (List.map ~f:snd libs)
    in
    let flags =
      Resolve.Memo.args
        (Resolve.Memo.map libs ~f:(fun libs ->
             Path.Set.of_list_map libs ~f:(fun t ->
                 let info = Lib.info t in
                 Lib_info.src_dir info)
             |> Lib_flags.L.to_iflags))
    in
    let open Action_builder.O in
    ( flags
    , let* libs = Resolve.Memo.read libs in
      (* coqdep expects an mlpack file next to the sources otherwise it will
         omit the cmxs deps *)
      let ml_pack_files lib =
        let plugins =
          let info = Lib.info lib in
          let plugins = Lib_info.plugins info in
          Mode.Dict.get plugins Native
        in
        let to_mlpack file =
          [ Path.set_extension file ~ext:".mlpack"
          ; Path.set_extension file ~ext:".mllib"
          ]
        in
        List.concat_map plugins ~f:to_mlpack
      in
      let meta_info (lib : Lib.t) =
        let name = Lib.name lib |> Lib_name.to_string in
        match Lib_info.status (Lib.info lib) with
        | Public (_, pkg) ->
          let package = Package.name pkg in
          let meta_i =
            Path.Build.relative
              (Local_install_path.lib_dir ~context ~package)
              "META"
          in
          Some (Path.build meta_i)
        | Installed -> None
        | Installed_private | Private _ ->
          let is_error = coq_lang_version >= (0, 6) in
          let text = if is_error then "not supported" else "deprecated" in
          User_warning.emit ?loc:plugin_loc ~is_error
            [ Pp.textf "Using private library %s as a Coq plugin is %s" name
                text
            ];
          None
      in
      (* If the mlpack files don't exist, don't fail *)
      Action_builder.all_unit
        [ Action_builder.paths (List.filter_map ~f:meta_info libs)
        ; Action_builder.paths_existing (List.concat_map ~f:ml_pack_files libs)
        ] )
  in
  let ml_flags = Resolve.Memo.map res ~f:fst in
  let mlpack_rule =
    let open Action_builder.O in
    let* _, mlpack_rule = Resolve.Memo.read res in
    mlpack_rule
  in
  (ml_flags, mlpack_rule)

let parse_coqdep ~dir ~(boot_type : Bootstrap.t) ~coq_module
    (lines : string list) =
  let source = Coq_module.source coq_module in
  let invalid phase =
    User_error.raise
      [ Pp.textf "coqdep returned invalid output for %s / [phase: %s]"
          (Path.Build.to_string_maybe_quoted source)
          phase
      ; Pp.verbatim (String.concat ~sep:"\n" lines)
      ]
  in
  let line =
    match lines with
    | [] | _ :: _ :: _ :: _ -> invalid "line"
    | [ line ] -> line
    | [ l1; _l2 ] ->
      (* .vo is produced before .vio, this is fragile tho *)
      l1
  in
  match String.lsplit2 line ~on:':' with
  | None -> invalid "split"
  | Some (basename, deps) -> (
    let ff = List.hd @@ String.extract_blank_separated_words basename in
    let depname, _ = Filename.split_extension ff in
    let modname =
      String.concat ~sep:"/"
        Coq_module.(
          prefix coq_module @ [ Coq_module.Name.to_string (name coq_module) ])
    in
    if depname <> modname then invalid "basename";
    let deps = String.extract_blank_separated_words deps in
    (* Add prelude deps for when stdlib is in scope and we are not actually
       compiling the prelude *)
    let deps = List.map ~f:(Path.relative (Path.build dir)) deps in
    match boot_type with
    | No_boot | Bootstrap_prelude -> deps
    | Bootstrap lib ->
      Path.relative (Path.build (Coq_lib.src_root lib)) "Init/Prelude.vo"
      :: deps)

let boot_type ~dir ~use_stdlib ~wrapper_name coq_module =
  let* scope = Scope.DB.find_by_dir dir in
  let+ boot_lib =
    scope |> Scope.coq_libs |> Coq_lib.DB.boot_library |> Resolve.Memo.read_memo
  in
  if use_stdlib then
    match boot_lib with
    | None -> Bootstrap.No_boot
    | Some (_loc, lib) ->
      (* This is here as an optimization, TODO; replace with per_file flags *)
      let init =
        String.equal (Coq_lib_name.wrapper (Coq_lib.name lib)) wrapper_name
        && Option.equal String.equal
             (List.hd_opt (Coq_module.prefix coq_module))
             (Some "Init")
      in
      if init then Bootstrap.Bootstrap_prelude else Bootstrap lib
  else Bootstrap.Bootstrap_prelude

let setup_coqdep_rule ~sctx ~dir ~loc ~theories_deps ~wrapper_name ~use_stdlib
    ~source_rule ~ml_flags ~mlpack_rule coq_module =
  let* boot_type = boot_type ~dir ~use_stdlib ~wrapper_name coq_module in
  (* coqdep needs the full source + plugin's mlpack to be present :( *)
  let source = Coq_module.source coq_module in
  let file_flags =
    [ Command.Args.S
        (coqc_file_flags ~dir ~theories_deps ~wrapper_name ~boot_type ~ml_flags)
    ; As [ "-dyndep"; "opt" ]
    ; Dep (Path.build source)
    ]
  in
  let stdout_to = Coq_module.dep_file ~obj_dir:dir coq_module in
  let* coqdep =
    Super_context.resolve_program sctx "coqdep" ~dir ~loc:(Some loc)
      ~hint:"opam install coq"
  in
  (* Coqdep has to be called in the stanza's directory *)
  Super_context.add_rule ~loc sctx ~dir
    (let open Action_builder.With_targets.O in
    Action_builder.with_no_targets mlpack_rule
    >>> Action_builder.(with_no_targets (goal source_rule))
    >>> Command.run ~dir:(Path.build dir) ~stdout_to coqdep file_flags)

let coqc_rule ~sctx ~theories_deps ~theory_dirs ~dir ~coq_flags ~file_flags
    ~coqc ~coqc_dir ~mode ~wrapper_name coq_module =
  let args =
    [ Command.Args.dyn coq_flags
    ; Command.Args.Hidden_targets
        (Coq_module.obj_files ~wrapper_name ~mode ~obj_dir:dir
           ~obj_files_mode:Coq_module.Build coq_module
        |> List.map ~f:fst)
    ; coqc_native_flags ~sctx ~dir ~theories_deps ~theory_dirs ~mode
    ; S file_flags
    ; Dep (Path.build @@ Coq_module.source coq_module)
    ]
  in
  let open Action_builder.With_targets.O in
  Command.run ~dir:(Path.build coqc_dir) coqc args
  (* The way we handle the transitive dependencies of .vo files is not safe for
     sandboxing *)
  >>| Action.Full.add_sandbox Sandbox_config.no_sandboxing

let deps_of ~dir ~boot_type coq_module =
  let stdout_to = Coq_module.dep_file ~obj_dir:dir coq_module in
  Action_builder.dyn_paths_unit
    (Action_builder.map
       (Action_builder.lines_of (Path.build stdout_to))
       ~f:(parse_coqdep ~dir ~boot_type ~coq_module))

let setup_coqc_rule ~loc ~dir ~sctx ~coqc_dir ~file_targets ~stanza_flags
    ~theories_deps ~mode ~wrapper_name ~use_stdlib ~ml_flags ~theory_dirs
    coq_module =
  (* Process coqdep and generate rules *)
  let* boot_type = boot_type ~dir ~use_stdlib ~wrapper_name coq_module in
  let deps_of = deps_of ~dir ~boot_type coq_module in
  let file_flags =
    coqc_file_flags ~dir ~theories_deps ~wrapper_name ~boot_type ~ml_flags
  in
  let* coqc = coqc ~loc ~dir ~sctx in
  let* expander = Super_context.expander sctx ~dir in
  let coq_flags = coq_flags ~expander ~dir ~stanza_flags ~sctx in
  let open Action_builder.With_targets.O in
  Super_context.add_rule ~loc ~dir sctx
    (Action_builder.with_no_targets deps_of
    >>> Action_builder.With_targets.add ~file_targets
        @@ coqc_rule ~sctx ~dir ~file_flags ~coqc ~coqc_dir ~coq_flags ~mode
             ~wrapper_name ~theories_deps ~theory_dirs coq_module)

let coq_modules_of_theory ~sctx lib =
  Action_builder.of_memo
    (let name = Coq_lib.name lib in
     let dir = Coq_lib.src_root lib in
     let* dir_contents = Dir_contents.get sctx ~dir in
     let+ coq_sources = Dir_contents.coq dir_contents in
     Coq_sources.library coq_sources ~name)

let source_rule ~sctx theories =
  (* sources for depending libraries coqdep requires all the files to be in the
     tree to produce correct dependencies, including those of dependencies *)
  Action_builder.dyn_paths_unit
    (let open Action_builder.O in
    let+ l =
      Action_builder.List.map theories ~f:(coq_modules_of_theory ~sctx)
    in
    List.concat l |> List.rev_map ~f:(fun m -> Path.build (Coq_module.source m)))

let coqdoc_directory ~mode ~obj_dir ~name =
  Path.Build.relative obj_dir
    (Coq_lib_name.to_string name
    ^
    match mode with
    | `Html -> ".html"
    | `Latex -> ".tex")

let coqdoc_directory_targets ~dir:obj_dir (theory : Coq_stanza.Theory.t) =
  let loc = theory.buildable.loc in
  let name = snd theory.name in
  Path.Build.Map.of_list_exn
    [ (coqdoc_directory ~mode:`Html ~obj_dir ~name, loc)
    ; (coqdoc_directory ~mode:`Latex ~obj_dir ~name, loc)
    ]

let setup_coqdoc_rules ~sctx ~dir ~theories_deps ~wrapper_name
    (s : Coq_stanza.Theory.t) coq_modules =
  let loc, name = (s.buildable.loc, snd s.name) in
  let rule =
    let file_flags =
      let file_flags =
        [ theories_flags ~theories_deps
        ; A "-R"
        ; Path (Path.build dir)
        ; A wrapper_name
        ]
      in
      (* BUG: we were passing No_boot before and now we have made it explicit. We
          probably want to do something better here. *)
      [ Bootstrap.flags ~coqdoc:true Bootstrap.No_boot; S file_flags ]
    in
    fun mode ->
      let* () =
        let* coqdoc =
          Super_context.resolve_program sctx "coqdoc" ~dir ~loc:(Some loc)
            ~hint:"opam install coq"
        in
        (let doc_dir = coqdoc_directory ~mode ~obj_dir:dir ~name in
         let file_flags =
           let globs =
             let open Action_builder.O in
             let* theories_deps = Resolve.Memo.read theories_deps in
             Action_builder.of_memo
             @@
             let open Memo.O in
             let+ deps =
               Memo.parallel_map theories_deps ~f:(fun theory ->
                   let+ theory_dirs = directories_of_lib ~sctx theory in
                   Dep.Set.of_list_map theory_dirs ~f:(fun dir ->
                       (* TODO *)
                       Glob.of_string_exn Loc.none "*.glob"
                       |> File_selector.of_glob ~dir:(Path.build dir)
                       |> Dep.file_selector))
             in
             Command.Args.Hidden_deps (Dep.Set.union_all deps)
           in
           let mode_flag =
             match mode with
             | `Html -> "--html"
             | `Latex -> "--latex"
           in
           [ Command.Args.S file_flags
           ; A "--toc"
           ; A mode_flag
           ; A "-d"
           ; Path (Path.build doc_dir)
           ; Deps
               (List.map ~f:Path.build
               @@ List.map ~f:Coq_module.source coq_modules)
           ; Dyn globs
           ; Hidden_deps
               (Dep.Set.of_files @@ List.map ~f:Path.build
               @@ List.map ~f:(Coq_module.glob_file ~obj_dir:dir) coq_modules)
           ]
         in
         Command.run ~sandbox:Sandbox_config.needs_sandboxing
           ~dir:(Path.build dir) coqdoc file_flags
         |> Action_builder.With_targets.map
              ~f:
                (Action.Full.map ~f:(fun coqdoc ->
                     Action.Progn [ Action.mkdir doc_dir; coqdoc ]))
         |> Action_builder.With_targets.add_directories
              ~directory_targets:[ doc_dir ])
        |> Super_context.add_rule ~loc ~dir sctx
      in
      let alias =
        match mode with
        | `Html -> Alias.doc ~dir
        | `Latex -> Alias.make (Alias.Name.of_string "doc-latex") ~dir
      in
      coqdoc_directory ~mode ~obj_dir:dir ~name
      |> Path.build |> Action_builder.path
      |> Rules.Produce.Alias.add_deps alias ~loc
  in
  rule `Html >>> rule `Latex

let setup_theory_rules ~sctx ~dir ~dir_contents (s : Coq_stanza.Theory.t) =
  let context = Super_context.context sctx |> Context.name in
  let* scope = Scope.DB.find_by_dir dir in
  let theory =
    let coq_lib_db = Scope.coq_libs scope in
    Coq_lib.DB.resolve coq_lib_db ~coq_lang_version:s.buildable.coq_lang_version
      s.name
  in
  let theories_deps =
    Resolve.Memo.bind theory ~f:(fun theory ->
        Resolve.Memo.lift @@ Coq_lib.theories_closure theory)
  in
  (* ML-level flags for depending libraries *)
  let ml_flags, mlpack_rule =
    let lib_db = Scope.libs scope in
    plugins_of_buildable ~context ~theories_deps ~lib_db s.buildable
  in
  let wrapper_name = Coq_lib_name.wrapper (snd s.name) in

  let name = snd s.name in
  let* coq_dir_contents = Dir_contents.coq dir_contents in
  let theory_dirs =
    Coq_sources.directories coq_dir_contents ~name |> Path.Build.Set.of_list
  in
  let coq_modules = Coq_sources.library coq_dir_contents ~name in

  let loc = s.buildable.loc in
  let source_rule =
    let theories =
      let open Resolve.Memo.O in
      let+ theory = theory
      and+ theories = theories_deps in
      theory :: theories
    in
    let open Action_builder.O in
    let* theories = Resolve.Memo.read theories in
    source_rule ~sctx theories
  in
  let coqc_dir = (Super_context.context sctx).build_dir in

  let* mode = select_native_mode ~sctx ~dir s.buildable in
  Memo.parallel_iter coq_modules
    ~f:
      (setup_coqdep_rule ~sctx ~loc ~source_rule ~dir ~theories_deps
         ~wrapper_name ~use_stdlib:s.buildable.use_stdlib ~ml_flags ~mlpack_rule)
  >>> Memo.parallel_iter coq_modules
        ~f:
          (setup_coqc_rule ~file_targets:[] ~stanza_flags:s.buildable.flags
             ~sctx ~loc ~coqc_dir ~dir ~theories_deps ~mode ~wrapper_name
             ~use_stdlib:s.buildable.use_stdlib ~ml_flags ~theory_dirs)
  >>> setup_coqdoc_rules ~sctx ~dir ~theories_deps s coq_modules ~wrapper_name

let coqtop_args_theory ~sctx ~dir ~dir_contents (s : Coq_stanza.Theory.t)
    coq_module =
  Action_builder.of_memo_join
  @@
  let theories_deps =
    theories_deps_requires_for_user_written ~dir s.buildable
  in
  let wrapper_name = Coq_lib_name.wrapper (snd s.name) in
  let context = Super_context.context sctx |> Context.name in
  let* scope = Scope.DB.find_by_dir dir in
  let ml_flags, _ =
    let lib_db = Scope.libs scope in
    plugins_of_buildable ~context ~theories_deps ~lib_db s.buildable
  in
  let* expander = Super_context.expander sctx ~dir in
  let* mode = select_native_mode ~sctx ~dir s.buildable in
  let name = snd s.name in
  let* boot_type =
    boot_type ~dir ~use_stdlib:s.buildable.use_stdlib ~wrapper_name coq_module
  in
  let+ coq_dir_contents = Dir_contents.coq dir_contents in
  let theory_dirs =
    Coq_sources.directories coq_dir_contents ~name |> Path.Build.Set.of_list
  in
  let coqc_native_flags =
    coqc_native_flags ~sctx ~dir ~theories_deps ~theory_dirs ~mode
  in
  let open Action_builder.O in
  let+ coq_flags =
    coq_flags ~expander ~dir ~stanza_flags:s.buildable.flags ~sctx
  in
  Command.Args.As coq_flags :: Command.Args.S [ coqc_native_flags ]
  :: coqc_file_flags ~dir ~theories_deps ~wrapper_name ~boot_type ~ml_flags

(******************************************************************************)
(* Install rules *)
(******************************************************************************)

(* This is here for compatibility with Coq < 8.11, which expects plugin files to
   be in the folder containing the `.vo` files *)
let coq_plugins_install_rules ~scope ~package ~dst_dir (s : Coq_stanza.Theory.t)
    =
  let lib_db = Scope.libs scope in
  let+ ml_libs =
    (* get_libraries from Coq's ML dependencies *)
    Resolve.Memo.read_memo
      (Resolve.Memo.List.map ~f:(Lib.DB.resolve lib_db) s.buildable.plugins)
  in
  let rules_for_lib lib =
    let info = Lib.info lib in
    (* Don't install libraries that don't belong to this package *)
    if
      let name = Package.name package in
      Option.equal Package.Name.equal (Lib_info.package info) (Some name)
    then
      let loc = Lib_info.loc info in
      let plugins = Lib_info.plugins info in
      Mode.Dict.get plugins Native
      |> List.map ~f:(fun plugin_file ->
             (* Safe because all coq libraries are local for now *)
             let plugin_file = Path.as_in_build_dir_exn plugin_file in
             let plugin_file_basename = Path.Build.basename plugin_file in
             let dst =
               Path.Local.(to_string (relative dst_dir plugin_file_basename))
             in
             let entry =
               (* TODO this [loc] should come from [s.buildable.libraries] *)
               Install.Entry.make Section.Lib_root ~dst ~kind:`File plugin_file
             in
             Install.Entry.Sourced.create ~loc entry)
    else []
  in
  List.concat_map ~f:rules_for_lib ml_libs

let install_rules ~sctx ~dir s =
  match s with
  | { Coq_stanza.Theory.package = None; _ } -> Memo.return []
  | { Coq_stanza.Theory.package = Some package; buildable; _ } ->
    let loc = s.buildable.loc in
    let* mode = select_native_mode ~sctx ~dir buildable in
    let* scope = Scope.DB.find_by_dir dir in
    let* dir_contents = Dir_contents.get sctx ~dir in
    let name = snd s.name in
    (* This must match the wrapper prefix for now to remain compatible *)
    let dst_suffix = Coq_lib_name.dir name in
    (* These are the rules for now, coq lang 2.0 will make this uniform *)
    let dst_dir =
      if s.boot then
        (* We drop the "Coq" prefix (!) *)
        Path.Local.of_string "coq/theories"
      else
        let coq_root = Path.Local.of_string "coq/user-contrib" in
        Path.Local.relative coq_root dst_suffix
    in
    (* Also, stdlib plugins are handled in a hardcoded way, so no compat install
       is needed *)
    let* coq_plugins_install_rules =
      if s.boot then Memo.return []
      else coq_plugins_install_rules ~scope ~package ~dst_dir s
    in
    let wrapper_name = Coq_lib_name.wrapper name in
    let to_path f = Path.reach ~from:(Path.build dir) (Path.build f) in
    let to_dst f = Path.Local.to_string @@ Path.Local.relative dst_dir f in
    let make_entry (orig_file : Path.Build.t) (dst_file : string) =
      let entry =
        Install.Entry.make Section.Lib_root ~dst:(to_dst dst_file) orig_file
          ~kind:`File
      in
      Install.Entry.Sourced.create ~loc entry
    in
    let+ coq_sources = Dir_contents.coq dir_contents in
    coq_sources |> Coq_sources.library ~name
    |> List.concat_map ~f:(fun (vfile : Coq_module.t) ->
           let obj_files =
             Coq_module.obj_files ~wrapper_name ~mode ~obj_dir:dir
               ~obj_files_mode:Coq_module.Install vfile
             |> List.map
                  ~f:(fun ((vo_file : Path.Build.t), (install_vo_file : string))
                     -> make_entry vo_file install_vo_file)
           in
           let vfile = Coq_module.source vfile in
           let vfile_dst = to_path vfile in
           make_entry vfile vfile_dst :: obj_files)
    |> List.rev_append coq_plugins_install_rules

let setup_coqpp_rules ~sctx ~dir ({ loc; modules } : Coq_stanza.Coqpp.t) =
  let* coqpp =
    Super_context.resolve_program sctx "coqpp" ~dir ~loc:(Some loc)
      ~hint:"opam install coq"
  and* mlg_files = Coq_sources.mlg_files ~sctx ~dir ~modules in
  let mlg_rule m =
    let source = Path.build m in
    let target = Path.Build.set_extension m ~ext:".ml" in
    let args = [ Command.Args.Dep source; Hidden_targets [ target ] ] in
    let build_dir = (Super_context.context sctx).build_dir in
    Command.run ~dir:(Path.build build_dir) coqpp args
  in
  List.rev_map ~f:mlg_rule mlg_files |> Super_context.add_rules ~loc ~dir sctx

let setup_extraction_rules ~sctx ~dir ~dir_contents
    (s : Coq_stanza.Extraction.t) =
  let wrapper_name = "DuneExtraction" in
  let* coq_module =
    let+ coq = Dir_contents.coq dir_contents in
    Coq_sources.extract coq s
  in
  let file_targets =
    Coq_stanza.Extraction.ml_target_fnames s
    |> List.map ~f:(Path.Build.relative dir)
  in
  let theories_deps =
    theories_deps_requires_for_user_written ~dir s.buildable
  in
  let source_rule =
    let open Action_builder.O in
    let* theories_deps = Resolve.Memo.read theories_deps in
    source_rule ~sctx theories_deps
    >>> Action_builder.path (Path.build (Coq_module.source coq_module))
  in
  let context = Super_context.context sctx |> Context.name in
  let* scope = Scope.DB.find_by_dir dir in
  let ml_flags, mlpack_rule =
    let lib_db = Scope.libs scope in
    plugins_of_buildable ~context ~theories_deps ~lib_db s.buildable
  in
  let* mode = select_native_mode ~sctx ~dir s.buildable in
  setup_coqdep_rule ~sctx ~loc:s.buildable.loc ~source_rule ~dir ~theories_deps
    ~wrapper_name ~use_stdlib:s.buildable.use_stdlib ~ml_flags ~mlpack_rule
    coq_module
  >>> setup_coqc_rule ~file_targets ~stanza_flags:s.buildable.flags ~sctx
        ~loc:s.buildable.loc ~coqc_dir:dir coq_module ~dir ~theories_deps ~mode
        ~wrapper_name ~use_stdlib:s.buildable.use_stdlib ~ml_flags
        ~theory_dirs:Path.Build.Set.empty

let coqtop_args_extraction ~sctx ~dir (s : Coq_stanza.Extraction.t) coq_module =
  Action_builder.of_memo_join
  @@
  let theories_deps =
    theories_deps_requires_for_user_written ~dir s.buildable
  in
  let wrapper_name = "DuneExtraction" in
  let context = Super_context.context sctx |> Context.name in
  let* scope = Scope.DB.find_by_dir dir in
  let ml_flags, _ =
    let lib_db = Scope.libs scope in
    plugins_of_buildable ~context ~theories_deps ~lib_db s.buildable
  in
  let* expander = Super_context.expander sctx ~dir in
  let* boot_type =
    boot_type ~dir ~use_stdlib:s.buildable.use_stdlib ~wrapper_name coq_module
  in
  let+ mode = select_native_mode ~sctx ~dir s.buildable in
  let coqc_native_flags =
    coqc_native_flags ~sctx ~dir ~theories_deps
      ~theory_dirs:Path.Build.Set.empty ~mode
  in
  let open Action_builder.O in
  let+ coq_flags =
    coq_flags ~expander ~dir ~stanza_flags:s.buildable.flags ~sctx
  in
  Command.Args.As coq_flags :: Command.Args.S [ coqc_native_flags ]
  :: coqc_file_flags ~dir ~theories_deps ~wrapper_name ~boot_type ~ml_flags
