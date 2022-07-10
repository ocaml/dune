open Import
open Memo.O

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2020                              *)
(* Written by: Emilio Jesús Gallego Arias *)
(* Written by: Rudi Grinberg *)

open Coq_stanza

let deps_kind = `Coqmod

module Require_map_db = struct
  (* merge all the maps *)
  let impl (requires, buildable_map) =
    Memo.return @@ Coq_require_map.merge_all (buildable_map :: requires)

  let memo =
    let module Input = struct
      type t =
        Coq_module.t Coq_require_map.t list * Coq_module.t Coq_require_map.t

      let equal = ( == )

      let hash = Poly.hash

      let to_dyn = Dyn.opaque
    end in
    Memo.create "coq-require-map-db" ~input:(module Input) impl

  let exec ~requires map = Memo.exec memo (requires, map)
end

(* Coqdep / Coq expect the deps to the directory where the plugin cmxs file are.
   This seems to correspond to src_dir. *)
module Util = struct
  let include_paths ts =
    Path.Set.of_list_map ts ~f:(fun t ->
        let info = Lib.info t in
        Lib_info.src_dir info)

  let coq_nativelib_cmi_dirs ts =
    List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
        let info = Lib.info t in
        (* We want the cmi files *)
        let obj_dir = Obj_dir.public_cmi_dir (Lib_info.obj_dir info) in
        Path.Set.add acc obj_dir)

  let include_flags ts = include_paths ts |> Lib_flags.L.to_iflags

  (* coqdep expects an mlpack file next to the sources otherwise it
   * will omit the cmxs deps *)
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

  let theory_coqc_flag lib =
    let dir = Coq_lib.src_root lib in
    let binding_flag = if Coq_lib.implicit lib then "-R" else "-Q" in
    Command.Args.S
      [ A binding_flag
      ; Path (Path.build dir)
      ; A (Coq_lib.name lib |> Coq_lib_name.wrapper)
      ]
end

let resolve_program sctx ~loc ~dir ?(hint = "opam install coq") prog =
  Super_context.resolve_program ~dir sctx prog ~loc:(Some loc) ~hint

module Coq_plugin = struct
  let meta_info ~coq_lang_version ~plugin_loc ~context (lib : Lib.t) =
    let debug = false in
    let name = Lib.name lib |> Lib_name.to_string in
    if debug then Format.eprintf "Meta info for %s@\n" name;
    match Lib_info.status (Lib.info lib) with
    | Public (_, pkg) ->
      let package = Package.name pkg in
      let meta_i =
        Path.Build.relative
          (Local_install_path.lib_dir ~context ~package)
          "META"
      in
      if debug then
        Format.eprintf "Meta for %s: %s@\n" name (Path.Build.to_string meta_i);
      Some (Path.build meta_i)
    | Installed -> None
    | Installed_private | Private _ ->
      let is_error = coq_lang_version >= (0, 6) in
      let text = if is_error then "not supported" else "deprecated" in
      User_warning.emit ?loc:plugin_loc ~is_error
        [ Pp.textf "Using private library %s as a Coq plugin is %s" name text ];
      None

  (* compute include flags and mlpack rules *)
  let setup_ml_deps ~coq_lang_version ~context ~plugin_loc libs theories =
    (* Pair of include flags and paths to mlpack *)
    let libs =
      let open Resolve.Memo.O in
      let* theories = theories in
      let* theories =
        Resolve.Memo.lift
        @@ Resolve.List.concat_map ~f:Coq_lib.libraries theories
      in
      let libs = libs @ theories in
      Lib.closure ~linking:false (List.map ~f:snd libs)
    in
    let flags =
      Resolve.Memo.args (Resolve.Memo.map libs ~f:Util.include_flags)
    in
    let open Action_builder.O in
    ( flags
    , let* libs = Resolve.Memo.read libs in
      (* If the mlpack files don't exist, don't fail *)
      Action_builder.all_unit
        [ Action_builder.paths
            (List.filter_map
               ~f:(meta_info ~plugin_loc ~coq_lang_version ~context)
               libs)
        ; Action_builder.paths_existing
            (List.concat_map ~f:Util.ml_pack_files libs)
        ] )

  let of_buildable ~context ~lib_db ~theories_deps
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
      setup_ml_deps ~plugin_loc ~coq_lang_version ~context libs theories_deps
    in
    let ml_flags = Resolve.Memo.map res ~f:fst in
    let mlpack_rule =
      let open Action_builder.O in
      let* _, mlpack_rule = Resolve.Memo.read res in
      mlpack_rule
    in
    (ml_flags, mlpack_rule)
end

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

  let get_for_module ~boot_lib ~wrapper_name coq_module =
    match boot_lib with
    | None -> No_boot
    | Some (_loc, lib) -> (
      (* This is here as an optimization, TODO; replace with per_file flags *)
      let init =
        let open Coq_module in
        String.equal (Coq_lib_name.wrapper (Coq_lib.name lib)) wrapper_name
        && Path.is_prefix (prefix coq_module)
             ~prefix:(Path.of_string_list [ "Init" ])
      in
      match init with
      | false -> Bootstrap lib
      | true -> Bootstrap_prelude)

  let get ~use_stdlib ~boot_lib ~wrapper_name coq_module =
    if not use_stdlib then Bootstrap_prelude
    else get_for_module ~boot_lib ~wrapper_name coq_module

  let boot_lib_flags ~coqdoc lib : _ Command.Args.t =
    let dir = Coq_lib.src_root lib in
    S
      (if coqdoc then [ A "--coqlib"; Path (Path.build dir) ]
      else [ A "-boot" ])

  let flags ~coqdoc t : _ Command.Args.t =
    match t with
    | No_boot -> As []
    | Bootstrap lib -> boot_lib_flags ~coqdoc lib
    | Bootstrap_prelude -> As [ "-boot"; "-noinit" ]
end

(* get_libraries from Coq's ML dependencies *)
let libs_of_coq_deps ~lib_db = Resolve.Memo.List.map ~f:(Lib.DB.resolve lib_db)

let select_native_mode ~sctx ~(buildable : Buildable.t) : Coq_mode.t =
  let profile = (Super_context.context sctx).profile in
  if Profile.is_dev profile then VoOnly else snd buildable.mode

let rec resolve_first lib_db = function
  | [] -> assert false
  | [ n ] -> Lib.DB.resolve lib_db (Loc.none, Lib_name.of_string n)
  | n :: l -> (
    let open Memo.O in
    Lib.DB.resolve_when_exists lib_db (Loc.none, Lib_name.of_string n)
    >>= function
    | Some l -> Resolve.Memo.lift l
    | None -> resolve_first lib_db l)

module Context = struct
  type 'a t =
    { deps : [ `Coqdep | `Coqmod of Coq_module.t Coq_require_map.t ]
    ; coqc_dir : Path.Build.t
    ; wrapper_name : string
    ; dir : Path.Build.t
    ; expander : Expander.t
    ; buildable : Buildable.t
    ; theories_deps : Coq_lib.t list Resolve.Memo.t
    ; mlpack_rule : unit Action_builder.t
    ; ml_flags : 'a Command.Args.t Resolve.Memo.t
    ; scope : Scope.t
    ; boot_type : Bootstrap.t Resolve.Memo.t
    ; use_stdlib : bool
    ; profile_flags : string list Action_builder.t
    ; mode : Coq_mode.t
    ; native_includes : Path.Set.t Resolve.t
    ; native_theory_includes : Path.Build.Set.t Resolve.t
    }

  let resolve_program_general ~sctx ~name t =
    resolve_program sctx ~dir:t.dir ~loc:t.buildable.loc name

  let coqdep ~sctx t = resolve_program_general ~sctx ~name:"coqdep" t

  let coqc ~sctx t = resolve_program_general ~sctx ~name:"coqc" t

  let coqdoc ~sctx t = resolve_program_general ~sctx ~name:"coqdoc" t

  let coq_flags t =
    let standard = t.profile_flags in
    Expander.expand_and_eval_set t.expander t.buildable.flags ~standard

  let theories_flags t =
    Resolve.Memo.args
      (let open Resolve.Memo.O in
      let+ libs = t.theories_deps in
      Command.Args.S (List.map ~f:Util.theory_coqc_flag libs))

  let coqc_file_flags cctx =
    let file_flags : _ Command.Args.t list =
      [ Dyn (Resolve.Memo.read cctx.ml_flags)
      ; theories_flags cctx
      ; A "-R"
      ; Path (Path.build cctx.dir)
      ; A cctx.wrapper_name
      ]
    in
    ([ Dyn
         (Resolve.Memo.map
            ~f:(fun b -> Bootstrap.flags ~coqdoc:false b)
            cctx.boot_type
         |> Resolve.Memo.read)
     ; S file_flags
     ]
      : _ Command.Args.t list)

  let coqc_native_flags cctx : _ Command.Args.t =
    match cctx.mode with
    | Legacy -> As []
    | VoOnly ->
      As
        [ "-w"
        ; "-deprecated-native-compiler-option"
        ; "-w"
        ; "-native-compiler-disabled"
        ; "-native-compiler"
        ; "ondemand"
        ]
    | Native ->
      let args =
        let open Resolve.O in
        let* native_includes = cctx.native_includes in
        let include_ dir acc = Command.Args.Path dir :: A "-nI" :: acc in
        let native_include_ml_args =
          Path.Set.fold native_includes ~init:[] ~f:include_
        in
        let+ native_theory_includes = cctx.native_theory_includes in
        let native_include_theory_output =
          Path.Build.Set.fold native_theory_includes ~init:[] ~f:(fun dir acc ->
              include_ (Path.build dir) acc)
        in
        (* This dir is relative to the file, by default [.coq-native/] *)
        Command.Args.S
          [ As [ "-w"; "-deprecated-native-compiler-option" ]
          ; As [ "-native-output-dir"; "." ]
          ; As [ "-native-compiler"; "on" ]
          ; S (List.rev native_include_ml_args)
          ; S (List.rev native_include_theory_output)
          ]
      in
      Resolve.args args

  let coqdoc_file_flags cctx =
    let file_flags =
      [ theories_flags cctx
      ; A "-R"
      ; Path (Path.build cctx.dir)
      ; A cctx.wrapper_name
      ]
    in
    [ Command.Args.Dyn
        (Resolve.Memo.map
           ~f:(fun b -> Bootstrap.flags ~coqdoc:true b)
           cctx.boot_type
        |> Resolve.Memo.read)
    ; S file_flags
    ]

  let directories_of_lib ~sctx lib =
    let name = Coq_lib.name lib in
    let dir = Coq_lib.src_root lib in
    let* dir_contents = Dir_contents.get sctx ~dir in
    let+ coq_sources = Dir_contents.coq dir_contents in
    Coq_sources.directories coq_sources ~name

  let setup_native_theory_includes ~sctx ~mode ~theories_deps ~theory_dirs =
    match (mode : Coq_mode.t) with
    | VoOnly | Legacy -> Memo.return (Resolve.return Path.Build.Set.empty)
    | Native ->
      Resolve.Memo.bind theories_deps ~f:(fun theories_deps ->
          let+ l =
            Memo.parallel_map theories_deps ~f:(fun lib ->
                let+ theory_dirs = directories_of_lib ~sctx lib in
                Path.Build.Set.of_list theory_dirs)
          in
          Resolve.return (Path.Build.Set.union_all (theory_dirs :: l)))

  let create ~deps_kind ~coqc_dir sctx ~dir ~wrapper_name ~theories_deps
      ~theory_dirs stanza =
    let buildable =
      match stanza with
      | `Extraction (e : Extraction.t) -> e.buildable
      | `Theory (t : Theory.t) -> t.buildable
    in
    let use_stdlib = buildable.use_stdlib in
    let context = Super_context.context sctx |> Context.name in
    let* expander = Super_context.expander sctx ~dir in
    let* scope = Scope.DB.find_by_dir dir in
    let lib_db = Scope.libs scope in
    (* ML-level flags for depending libraries *)
    let ml_flags, mlpack_rule =
      Coq_plugin.of_buildable ~context ~theories_deps ~lib_db buildable
    in
    let mode = select_native_mode ~sctx ~buildable in
    let* native_includes =
      let open Resolve.Memo.O in
      resolve_first lib_db [ "coq-core.kernel"; "coq.kernel" ] >>| fun lib ->
      Util.coq_nativelib_cmi_dirs [ lib ]
    in
    let+ native_theory_includes =
      setup_native_theory_includes ~sctx ~mode ~theories_deps ~theory_dirs
    and+ deps =
      match deps_kind with
      | `Coqdep -> Memo.return `Coqdep
      | `Coqmod ->
        let* dir_contents = Dir_contents.get sctx ~dir in
        let+ coq_sources = Dir_contents.coq dir_contents in
        let what =
          match stanza with
          | `Extraction e -> `Extraction e
          | `Theory (t : Theory.t) -> `Theory (snd t.name)
        in
        `Coqmod (Coq_sources.require_map coq_sources what)
    and+ profile_flags = Super_context.coq sctx ~dir in
    { deps
    ; coqc_dir
    ; wrapper_name
    ; dir
    ; expander
    ; buildable
    ; theories_deps
    ; mlpack_rule
    ; ml_flags
    ; scope
    ; boot_type = Resolve.Memo.return Bootstrap.No_boot
    ; use_stdlib
    ; profile_flags
    ; mode
    ; native_includes
    ; native_theory_includes
    }

  let for_module t coq_module =
    let boot_type =
      let open Resolve.Memo.O in
      let+ boot_lib = t.scope |> Scope.coq_libs |> Coq_lib.DB.boot_library in
      Bootstrap.get ~use_stdlib:t.use_stdlib ~boot_lib
        ~wrapper_name:t.wrapper_name coq_module
    in
    { t with boot_type }
end

let parse_coqdep ~dir ~(boot_type : Bootstrap.t) ~coq_module contents =
  let source = Coq_module.source coq_module in
  let err_coqdep err =
    User_error.raise
      [ Pp.textf "coqdep returned invalid output for %s"
          (Path.Build.to_string_maybe_quoted source)
      ; Pp.textf "error: %s" err
      ; Pp.verbatim (String.concat ~sep:"\n" contents)
      ]
  in
  let deps =
    List.map contents ~f:(fun line ->
        match String.lsplit2 line ~on:':' with
        | None -> err_coqdep "unable to parse targets"
        | Some (targets, deps) ->
          ( String.extract_blank_separated_words targets
          , String.extract_blank_separated_words deps ))
  in
  let for_ ext =
    match
      List.find_map deps ~f:(fun (targets, deps) ->
          List.find_map targets ~f:(fun t ->
              if Filename.check_suffix t ext then Some (t, deps) else None))
    with
    | None -> err_coqdep (ext ^ " not found")
    | Some (basename, deps) -> (
      let ff = List.hd @@ String.extract_blank_separated_words basename in
      let depname, _ = Filename.split_extension ff in
      let modname =
        let name = Coq_module.name coq_module in
        let prefix = Coq_module.prefix coq_module in
        let path = Coq_module.Path.append_name prefix name in
        String.concat ~sep:"/" (Coq_module.Path.to_string_list path)
      in
      if depname <> modname then err_coqdep "basename is invalid";
      (* Add prelude deps for when stdlib is in scope and we are not actually
         compiling the prelude *)
      let deps = List.map ~f:(Path.relative (Path.build dir)) deps in
      match boot_type with
      | No_boot | Bootstrap_prelude -> deps
      | Bootstrap lib ->
        Path.relative (Path.build (Coq_lib.src_root lib)) ("Init/Prelude" ^ ext)
        :: deps)
  in
  (for_ ".vo", for_ ".vos")

let err_from_not_found ~loc from source =
  User_error.raise ~loc
  @@ (match Coqmod.From.prefix from with
     | Some prefix ->
       [ Pp.textf "could not find module %S with prefix %S"
           (Coqmod.From.require from |> Coqmod.Module.name)
           (prefix |> Coqmod.Module.name)
       ]
     | None ->
       [ Pp.textf "could not find module %S."
           (Coqmod.From.require from |> Coqmod.Module.name)
       ])
  @ [ Pp.textf "%s" @@ Dyn.to_string @@ Coq_require_map.to_dyn source ]

let err_from_ambiguous ~loc _from source =
  User_error.raise ~loc
  @@ [ Pp.textf "TODO ambiguous paths"
     ; Pp.textf "%s" @@ Dyn.to_string @@ Coq_require_map.to_dyn source
     ]

let err_undeclared_plugin ~loc libname =
  User_error.raise ~loc
    Pp.[ textf "TODO undelcared plugin %S" (Lib_name.to_string libname) ]

let coq_require_map_of_theory ~sctx lib =
  let name = Coq_lib.name lib in
  let dir = Coq_lib.src_root lib in
  let* dir_contents = Dir_contents.get sctx ~dir in
  let+ coq_sources = Dir_contents.coq dir_contents in
  Coq_sources.require_map coq_sources (`Theory name)

module Deps = struct
  let loc from_ coq_module =
    let fname = Coq_module.source coq_module |> Path.Build.to_string in
    Coqmod.From.require from_ |> Coqmod.Module.loc |> Coqmod.Loc.to_loc ~fname

  let froms ~theories ~theory_rms ~sources ~coq_module t =
    let f (from_ : Coqmod.From.t) =
      let loc = loc from_ coq_module in
      let prefix =
        Option.map (Coqmod.From.prefix from_) ~f:(fun p ->
            Coq_module.Path.of_string @@ Coqmod.Module.name p)
      in
      let suffix =
        Coqmod.From.require from_ |> Coqmod.Module.name
        |> Coq_module.Path.of_string
      in
      let open Memo.O in
      let+ require_map =
        let theories =
          match prefix with
          | Some prefix ->
            List.filter theories ~f:(fun theory ->
                Coq_module.Path.is_prefix ~prefix @@ Coq_lib.root_path theory)
          | None ->
            (* TODO this is incorrect, needs to include only current theory
               and boot library if present *)
            theories
        in
        let requires = List.map theories ~f:(Coq_lib.Map.find_exn theory_rms) in
        Require_map_db.exec ~requires sources
      in
      let matches =
        match prefix with
        | Some prefix -> Coq_require_map.find_all ~prefix ~suffix require_map
        | None ->
          Coq_require_map.find_all ~prefix:Coq_module.Path.empty ~suffix
            require_map
      in
      match matches with
      | [] -> err_from_not_found ~loc from_ require_map
      | [ m ] -> Path.build (Coq_module.vo_file m)
      | _ -> err_from_ambiguous ~loc from_ require_map
    in
    Coqmod.froms t |> Memo.parallel_map ~f |> Action_builder.of_memo

  let boot_deps ~boot_type =
    let open Bootstrap in
    let open Action_builder.O in
    let+ boot_type = Resolve.Memo.read boot_type in
    match boot_type with
    | No_boot | Bootstrap_prelude -> []
    | Bootstrap lib ->
      [ Path.relative (Path.build (Coq_lib.src_root lib)) "Init/Prelude.vo" ]

  let loads ~dir t =
    Coqmod.loads t
    |> List.rev_map ~f:(fun file ->
           let fname = Coqmod.Load.path file in
           Path.build (Path.Build.relative dir fname))

  let extradeps t =
    Coqmod.extradeps t
    |> List.rev_map ~f:(fun file ->
           let fname = Coqmod.ExtraDep.file file in
           let path =
             Coqmod.ExtraDep.from file |> Coqmod.Module.name
             |> Dune_re.replace_string Re.(compile @@ char '.') ~by:"/"
             |> Path.Local.of_string |> Path.Build.of_local
           in
           Path.build (Path.Build.relative path fname))

  let coqmod_deps ~theories ~sources ~dir ~theory_rms ~boot_type coq_module =
    let open Action_builder.O in
    let* t = Coqmod_rules.deps_of coq_module in
    (* convert [Coqmod.t] to a list of paths repping the deps *)
    let+ froms = froms ~theories ~theory_rms ~sources ~coq_module t
    and+ boot_deps = boot_deps ~boot_type in
    (* Add prelude deps for when stdlib is in scope and we are not actually
       compiling the prelude *)
    (* TODO: plugin deps *)
    ignore err_undeclared_plugin;
    List.concat [ froms; loads ~dir t; extradeps t; boot_deps ]

  let of_ ~sctx ~theories ~kind ~dir ~boot_type coq_module =
    match kind with
    | `Coqmod sources ->
      let+ theory_rms =
        Memo.parallel_map theories ~f:(fun theory ->
            let+ require_map = coq_require_map_of_theory ~sctx theory in
            (theory, require_map))
        |> Memo.map ~f:Coq_lib.Map.of_list_exn
      in
      ( coqmod_deps ~theories ~sources ~dir ~theory_rms ~boot_type coq_module
        |> Action_builder.dyn_paths_unit
      , Action_builder.dyn_paths_unit (Action_builder.return []) )
    | `Coqdep ->
      let stdout_to = Coq_module.dep_file coq_module in
      let deps =
        let open Action_builder.O in
        let* boot_type = Resolve.Memo.read boot_type in
        Action_builder.memoize
          (Path.Build.to_string stdout_to)
          (Action_builder.map
             (Action_builder.lines_of (Path.build stdout_to))
             ~f:(parse_coqdep ~dir ~boot_type ~coq_module))
      in
      let deps f =
        Action_builder.map deps ~f |> Action_builder.dyn_paths_unit
      in
      Memo.return @@ (deps fst, deps snd)
end

let coqdep_rule (cctx : _ Context.t) ~coqdep ~source_rule coq_module =
  (* coqdep needs the full source + plugin's mlpack to be present :( *)
  let source = Coq_module.source coq_module in
  let file_flags =
    let file_flags = Context.coqc_file_flags cctx in
    [ Command.Args.S file_flags (* TODO guard behind version of coq *)
    ; As [ "-dyndep"; "opt"; "-vos" ]
    ; Dep (Path.build source)
    ]
  in
  let stdout_to = Coq_module.dep_file coq_module in
  (* Coqdep has to be called in the stanza's directory *)
  let open Action_builder.With_targets.O in
  Action_builder.with_no_targets cctx.mlpack_rule
  >>> Action_builder.(with_no_targets (goal source_rule))
  >>> Command.run ~dir:(Path.build cctx.dir) ~stdout_to coqdep file_flags

let coqc_rule (cctx : _ Context.t) ~file_flags ~coqc ~obj_files_mode coq_module
    =
  let source = Coq_module.source coq_module in
  let file_flags =
    let wrapper_name, mode = (cctx.wrapper_name, cctx.mode) in
    let objects_to =
      (* Coq_module.obj_files ~wrapper_name ~mode ~obj_files_mode:Coq_module.Build *)
      Coq_module.obj_files ~wrapper_name ~mode ~obj_files_mode coq_module
      |> List.map ~f:fst
    in
    let native_flags = Context.coqc_native_flags cctx in
    [ Command.Args.Hidden_targets objects_to
    ; native_flags
    ; S file_flags
    ; Dep (Path.build source)
    ]
  in
  let coq_flags = Context.coq_flags cctx in
  let dir = Path.build cctx.coqc_dir in
  Command.run ~dir coqc (Command.Args.dyn coq_flags :: file_flags)

let coqc_rules (cctx : _ Context.t) ~deps_of ~file_flags ~coqc ~file_targets
    coq_module =
  let vo_deps, vos_deps = deps_of in
  [ ( vo_deps
    , coqc_rule cctx ~file_flags ~coqc ~obj_files_mode:(Build Vo) coq_module )
  ; (let file_flags = Command.Args.A "-vos" :: file_flags in
     ( vos_deps
     , coqc_rule cctx ~file_flags ~coqc ~obj_files_mode:(Build Vos) coq_module
     ))
  ]
  |> List.map ~f:(fun (deps, rule) ->
         let open Action_builder.With_targets.O in
         Action_builder.with_no_targets deps
         >>> Action_builder.With_targets.add ~file_targets rule
         (* The way we handle the transitive dependencies of .vo files is not
            safe for sandboxing *)
         >>| Action.Full.add_sandbox Sandbox_config.no_sandboxing)

module Coqdoc_mode = struct
  type t =
    | Html
    | Latex

  let flag = function
    | Html -> "--html"
    | Latex -> "--latex"

  let directory t obj_dir (theory : Coq_lib_name.t) =
    Path.Build.relative obj_dir
      (Coq_lib_name.to_string theory
      ^
      match t with
      | Html -> ".html"
      | Latex -> ".tex")

  let alias t ~dir =
    match t with
    | Html -> Alias.doc ~dir
    | Latex -> Alias.make (Alias.Name.of_string "doc-latex") ~dir
end

let coqdoc_directory_targets ~dir:obj_dir (theory : Coq_stanza.Theory.t) =
  let loc = theory.buildable.loc in
  let name = snd theory.name in
  Path.Build.Map.of_list_exn
    [ (Coqdoc_mode.directory Html obj_dir name, loc)
    ; (Coqdoc_mode.directory Latex obj_dir name, loc)
    ]

let coqdoc_rule (cctx : _ Context.t) ~sctx ~name ~coqdoc ~file_flags ~mode
    ~theories_deps coq_modules =
  let obj_dir = cctx.dir in
  let doc_dir = Coqdoc_mode.directory mode obj_dir name in
  let file_flags =
    let globs =
      let open Action_builder.O in
      let* theories_deps = Resolve.Memo.read theories_deps in
      Action_builder.of_memo
      @@
      let open Memo.O in
      let+ deps =
        Memo.parallel_map theories_deps ~f:(fun theory ->
            let+ theory_dirs = Context.directories_of_lib ~sctx theory in
            Dep.Set.of_list_map theory_dirs ~f:(fun dir ->
                (* TODO *)
                Glob.of_string_exn Loc.none "*.glob"
                |> File_selector.of_glob ~dir:(Path.build dir)
                |> Dep.file_selector))
      in
      Command.Args.Hidden_deps (Dep.Set.union_all deps)
    in
    [ Command.Args.S file_flags
    ; A "--toc"
    ; A Coqdoc_mode.(flag mode)
    ; A "-d"
    ; Path (Path.build doc_dir)
    ; Deps (List.map ~f:Path.build @@ List.map ~f:Coq_module.source coq_modules)
    ; Dyn globs
    ; Hidden_deps
        (Dep.Set.of_files @@ List.map ~f:Path.build
        @@ List.map ~f:Coq_module.glob_file coq_modules)
    ]
  in
  Command.run ~sandbox:Sandbox_config.needs_sandboxing
    ~dir:(Path.build cctx.dir) coqdoc file_flags
  |> Action_builder.With_targets.map
       ~f:
         (Action.Full.map ~f:(fun coqdoc ->
              Action.Progn [ Action.mkdir (Path.build doc_dir); coqdoc ]))
  |> Action_builder.With_targets.add_directories ~directory_targets:[ doc_dir ]

let setup_rule cctx ~sctx ~loc ~dir ~source_rule ~ml_targets coq_module =
  let file_flags = Context.coqc_file_flags cctx in
  let* () =
    match cctx.deps with
    | `Coqmod _ -> Coqmod_rules.add_rule sctx coq_module
    | `Coqdep ->
      let* coqdep = Context.coqdep ~sctx cctx in
      let rule = coqdep_rule cctx ~coqdep ~source_rule coq_module in
      Super_context.add_rule ~loc ~dir sctx rule
  in
  (* Process coqdep and generate rules *)
  let* deps_of =
    let* theories = cctx.theories_deps in
    let* theories = Resolve.read_memo theories in
    Deps.of_ ~sctx ~theories ~kind:cctx.deps ~dir:cctx.dir
      ~boot_type:cctx.boot_type coq_module
  in
  let* coqc = Context.coqc ~sctx cctx in
  Super_context.add_rules ~loc ~dir sctx
    (coqc_rules cctx ~file_flags ~coqc ~file_targets:ml_targets ~deps_of
       coq_module)

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
    let* theories = Resolve.Memo.read theories in
    let+ l =
      Action_builder.List.map theories ~f:(coq_modules_of_theory ~sctx)
    in
    List.concat l |> List.rev_map ~f:(fun m -> Path.build (Coq_module.source m)))

let setup_cctx_and_modules ~sctx ~dir ~dir_contents (s : Theory.t) theory =
  let name = snd s.name in
  let wrapper_name = Coq_lib_name.wrapper name in
  let theories_deps =
    Resolve.Memo.bind theory ~f:(fun theory ->
        Resolve.Memo.lift @@ Coq_lib.theories_closure theory)
  in
  let* coq_dir_contents = Dir_contents.coq dir_contents in
  let theory_dirs =
    Coq_sources.directories coq_dir_contents ~name |> Path.Build.Set.of_list
  in
  let coqc_dir = (Super_context.context sctx).build_dir in
  let+ cctx =
    Context.create sctx ~deps_kind ~coqc_dir ~dir ~wrapper_name ~theories_deps
      ~theory_dirs (`Theory s)
  in
  let coq_modules = Coq_sources.library coq_dir_contents ~name in
  (cctx, coq_modules)

let setup_vo_rules ~sctx ~dir ~(cctx : _ Context.t) (s : Theory.t) theory
    coq_modules =
  let loc = s.buildable.loc in
  let source_rule =
    let theories =
      let open Resolve.Memo.O in
      let* theory = theory in
      let+ theories = cctx.theories_deps in
      theory :: theories
    in
    source_rule ~sctx theories
  in
  Memo.parallel_iter coq_modules ~f:(fun m ->
      let cctx = Context.for_module cctx m in
      setup_rule cctx ~sctx ~loc ~dir ~source_rule ~ml_targets:[] m)

let setup_coqdoc_rules ~sctx ~dir ~cctx (s : Theory.t) coq_modules =
  let loc, name = (s.buildable.loc, snd s.name) in
  let rule =
    let file_flags = Context.coqdoc_file_flags cctx in
    fun mode ->
      let* () =
        let* coqdoc = Context.coqdoc ~sctx cctx in
        coqdoc_rule cctx ~sctx ~mode ~theories_deps:cctx.theories_deps ~name
          ~file_flags ~coqdoc coq_modules
        |> Super_context.add_rule ~loc ~dir sctx
      in
      Coqdoc_mode.directory mode cctx.dir name
      |> Path.build |> Action_builder.path
      |> Rules.Produce.Alias.add_deps (Coqdoc_mode.alias mode ~dir) ~loc
  in
  rule Html >>> rule Latex

let setup_rules ~sctx ~dir ~dir_contents (s : Theory.t) =
  let theory =
    let* scope = Scope.DB.find_by_dir dir in
    let coq_lib_db = Scope.coq_libs scope in
    Coq_lib.DB.resolve coq_lib_db ~coq_lang_version:s.buildable.coq_lang_version
      s.name
  in
  let* cctx, coq_module_sources =
    setup_cctx_and_modules ~sctx ~dir ~dir_contents s theory
  in
  setup_vo_rules ~sctx ~dir ~cctx s theory coq_module_sources
  >>> setup_coqdoc_rules ~sctx ~dir ~cctx s coq_module_sources

let coqtop_args_theory ~sctx ~dir ~dir_contents (s : Theory.t) coq_module_source
    =
  let name = s.name in
  let* scope = Scope.DB.find_by_dir dir in
  let coq_lib_db = Scope.coq_libs scope in
  let theory =
    Coq_lib.DB.resolve coq_lib_db name
      ~coq_lang_version:s.buildable.coq_lang_version
  in
  let* cctx, _ = setup_cctx_and_modules ~sctx ~dir ~dir_contents s theory in
  let cctx = Context.for_module cctx coq_module_source in
  let+ boot_type = Resolve.Memo.read_memo cctx.boot_type in
  (Context.coqc_file_flags cctx, boot_type)

(******************************************************************************)
(* Install rules *)
(******************************************************************************)

(* This is here for compatibility with Coq < 8.11, which expects plugin files to
   be in the folder containing the `.vo` files *)
let coq_plugins_install_rules ~scope ~package ~dst_dir (s : Theory.t) =
  let lib_db = Scope.libs scope in
  let+ ml_libs =
    Resolve.Memo.read_memo (libs_of_coq_deps ~lib_db s.buildable.plugins)
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
  | { Theory.package = None; _ } -> Memo.return []
  | { Theory.package = Some package; buildable; _ } ->
    let mode = select_native_mode ~sctx ~buildable in
    let loc = s.buildable.loc in
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
             Coq_module.obj_files ~wrapper_name ~mode
               ~obj_files_mode:Coq_module.Install vfile
             |> List.map
                  ~f:(fun ((vo_file : Path.Build.t), (install_vo_file : string))
                     -> make_entry vo_file install_vo_file)
           in
           let vfile = Coq_module.source vfile in
           let vfile_dst = to_path vfile in
           make_entry vfile vfile_dst :: obj_files)
    |> List.rev_append coq_plugins_install_rules

let setup_coqpp_rules ~sctx ~dir ({ loc; modules } : Coqpp.t) =
  let* coqpp = resolve_program sctx ~dir ~loc "coqpp"
  and* mlg_files = Coq_sources.mlg_files ~sctx ~dir ~modules in
  let mlg_rule m =
    let source = Path.build m in
    let target = Path.Build.set_extension m ~ext:".ml" in
    let args = [ Command.Args.Dep source; Hidden_targets [ target ] ] in
    let build_dir = (Super_context.context sctx).build_dir in
    Command.run ~dir:(Path.build build_dir) coqpp args
  in
  List.rev_map ~f:mlg_rule mlg_files |> Super_context.add_rules ~loc ~dir sctx

let setup_extraction_cctx_and_modules ~sctx ~dir ~dir_contents
    (s : Extraction.t) =
  let+ cctx =
    let wrapper_name = "DuneExtraction" in
    let* theories_deps =
      let* scope = Scope.DB.find_by_dir dir in
      let coq_lib_db = Scope.coq_libs scope in
      Coq_lib.DB.requires_for_user_written coq_lib_db s.buildable.theories
        ~coq_lang_version:s.buildable.coq_lang_version
    in
    let theory_dirs = Path.Build.Set.empty in
    let theories_deps = Resolve.Memo.lift theories_deps in
    Context.create sctx ~deps_kind ~coqc_dir:dir ~dir ~wrapper_name
      ~theories_deps ~theory_dirs (`Extraction s)
  and+ coq = Dir_contents.coq dir_contents in
  (cctx, Coq_sources.extract coq s)

let setup_extraction_rules ~sctx ~dir ~dir_contents (s : Extraction.t) =
  let* cctx, coq_module =
    setup_extraction_cctx_and_modules ~sctx ~dir ~dir_contents s
  in
  let ml_targets =
    Extraction.ml_target_fnames s |> List.map ~f:(Path.Build.relative dir)
  in
  let source_rule =
    let theories = source_rule ~sctx cctx.theories_deps in
    let open Action_builder.O in
    theories >>> Action_builder.path (Path.build (Coq_module.source coq_module))
  in
  setup_rule cctx ~sctx ~loc:s.buildable.loc ~dir ~source_rule coq_module
    ~ml_targets

let setup_ffi_rules ~sctx ~dir ({ loc; modules; library } : Ffi.t) =
  let* coqffi =
    resolve_program sctx ~dir ~loc "coqffi" ~hint:"opam install coq-ffi"
  in
  let coqffi_rule m =
    let module_ lib_info =
      let+ modules =
        match Lib_info.modules lib_info with
        | External (Some m) -> Memo.return m
        | External None ->
          User_error.raise ~loc
            [ Pp.textf
                "Library %S was not installed using Dune and therefore not \
                 supported by coq.ffi."
                (Lib_name.to_string (snd library))
            ]
        | Local ->
          Dir_contents.get sctx ~dir >>= Dir_contents.ocaml
          >>| Ml_sources.modules_and_obj_dir ~for_:(Library (snd library))
          >>| fst
      in
      match Modules.find modules (Module_name.of_string m) with
      | Some m -> m
      | None ->
        User_error.raise ~loc
          [ Pp.textf "Module %S was not found in library %S." m
              (Lib_name.to_string (snd library))
          ]
    in
    let cmi =
      Resolve.Memo.read
      @@
      let open Resolve.Memo.O in
      let* lib =
        let* scope = Scope.DB.find_by_dir dir |> Resolve.Memo.lift_memo in
        let lib_db = Scope.libs scope in
        Lib.DB.resolve lib_db library
      in
      let info = Lib.info lib in
      let obj_dir = Lib_info.obj_dir info in
      let* module_ = module_ info |> Resolve.Memo.lift_memo in
      let cmi = Obj_dir.Module.cm_file_exn obj_dir module_ ~kind:(Ocaml Cmi) in
      Resolve.Memo.return @@ Command.Args.Dep cmi
    in
    let target = Path.Build.relative dir (m ^ ".v") in
    let args = [ Command.Args.Dyn cmi; A "-o"; Target target ] in
    Command.run ~dir:(Path.build dir) coqffi args
  in
  List.rev_map ~f:coqffi_rule modules |> Super_context.add_rules ~loc ~dir sctx

let setup_of_ocaml_rules ~sctx ~dir ({ loc; modules } : Of_ocaml.t) =
  let* coq_of_ocaml =
    resolve_program sctx ~dir ~loc "coq-of-ocaml"
      ~hint:"opam install coq-of-ocaml"
  in
  let coq_of_ocaml_rule m =
    let source = Path.build (Path.Build.relative dir (m ^ ".ml")) in
    let target = Path.Build.relative dir (m ^ ".v") in
    let args = [ Command.Args.Dep source; A "-output"; Target target ] in
    Command.run ~dir:(Path.build dir) coq_of_ocaml args
  in
  List.rev_map ~f:coq_of_ocaml_rule modules
  |> Super_context.add_rules ~loc ~dir sctx

let coqtop_args_extraction ~sctx ~dir ~dir_contents (s : Extraction.t) =
  let* cctx, coq_module =
    setup_extraction_cctx_and_modules ~sctx ~dir ~dir_contents s
  in
  let cctx = Context.for_module cctx coq_module in
  let+ boot_type = Resolve.Memo.read_memo cctx.boot_type in
  (Context.coqc_file_flags cctx, boot_type)

let deps_of ~dir ~boot_type mod_ =
  (* TODO fix *)
  let kind = failwith "TODO dune coq top unsupported" in
  let sctx = failwith "" in
  ignore kind;
  ignore sctx;
  ignore dir;
  ignore boot_type;
  ignore mod_;
  assert false
(* Action_builder.of_memo_join
   @@ Deps.of_ ~sctx ~theories:[] ~kind ~dir ~boot_type mod_ *)
