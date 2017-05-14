open Import
open Jbuild_types

module Dir_with_jbuild = struct
  type t =
    { src_dir : Path.t
    ; ctx_dir : Path.t
    ; stanzas : Stanzas.t
    ; pkgs    : Pkgs.t
    }
end

type t =
  { context                                 : Context.t
  ; libs                                    : Lib_db.t
  ; stanzas                                 : Dir_with_jbuild.t list
  ; packages                                : Package.t String_map.t
  ; aliases                                 : Alias.Store.t
  ; file_tree                               : File_tree.t
  ; artifacts                               : Artifacts.t
  ; mutable rules                           : Build_interpret.Rule.t list
  ; stanzas_to_consider_for_install         : (Path.t * Stanza.t) list
  ; mutable known_targets_by_src_dir_so_far : String_set.t Path.Map.t
  ; cxx_flags                               : string list
  ; vars                                    : string String_map.t
  ; ppx_dir                                 : Path.t
  ; ppx_drivers                             : (string, Path.t) Hashtbl.t
  }

let context t = t.context
let aliases t = t.aliases
let stanzas t = t.stanzas
let packages t = t.packages
let artifacts t = t.artifacts
let file_tree t = t.file_tree
let rules t = t.rules
let stanzas_to_consider_for_install t = t.stanzas_to_consider_for_install
let cxx_flags t = t.cxx_flags

let expand_var_no_root t var = String_map.find var t.vars

let expand_vars t ~dir s =
  String_with_vars.expand s ~f:(function
  | "ROOT" -> Some (Path.reach ~from:dir t.context.build_dir)
  | var -> String_map.find var t.vars)

let resolve_program t ?hint ?(in_the_tree=true) bin =
  match Artifacts.binary t.artifacts ?hint ~in_the_tree bin with
  | Error fail -> Build.Prog_spec.Dyn (fun _ -> fail.fail ())
  | Ok    path -> Build.Prog_spec.Dep path

let create
      ~(context:Context.t)
      ~aliases
      ~dirs_with_dot_opam_files
      ~file_tree
      ~packages
      ~stanzas
      ~filter_out_optional_stanzas_with_missing_deps
  =
  let stanzas =
    List.map stanzas
      ~f:(fun (dir, pkgs, stanzas) ->
        { Dir_with_jbuild.
          src_dir = dir
        ; ctx_dir = Path.append context.build_dir dir
        ; stanzas
        ; pkgs
        })
  in
  let internal_libraries =
    List.concat_map stanzas ~f:(fun { ctx_dir;  stanzas; _ } ->
      List.filter_map stanzas ~f:(fun stanza ->
        match (stanza : Stanza.t) with
        | Library lib -> Some (ctx_dir, lib)
        | _ -> None))
  in
  let dirs_with_dot_opam_files =
    Path.Set.elements dirs_with_dot_opam_files
    |> List.map ~f:(Path.append context.build_dir)
    |> Path.Set.of_list
  in
  let libs =
    Lib_db.create context.findlib internal_libraries
      ~dirs_with_dot_opam_files
  in
  let stanzas_to_consider_for_install =
    if filter_out_optional_stanzas_with_missing_deps then
      List.concat_map stanzas ~f:(fun { ctx_dir; stanzas; _ } ->
        List.filter_map stanzas ~f:(function
          | Library _ -> None
          | stanza    -> Some (ctx_dir, stanza)))
      @ List.map
          (Lib_db.internal_libs_without_non_installable_optional_ones libs)
          ~f:(fun (dir, lib) -> (dir, Stanza.Library lib))
    else
      List.concat_map stanzas ~f:(fun { ctx_dir; stanzas; _ } ->
        List.map stanzas ~f:(fun s -> (ctx_dir, s)))
  in
  let artifacts =
    Artifacts.create context (List.map stanzas ~f:(fun (d : Dir_with_jbuild.t) ->
      (d.ctx_dir, d.stanzas)))
  in
  let cxx_flags =
    String.extract_blank_separated_words context.ocamlc_cflags
    |> List.filter ~f:(fun s -> not (String.is_prefix s ~prefix:"-std="))
  in
  let vars =
    let ocamlopt =
      match context.ocamlopt with
      | None -> Path.relative context.ocaml_bin "ocamlopt"
      | Some p -> p
    in
    let make =
      match Bin.make with
      | None   -> "make"
      | Some p -> Path.to_string p
    in
    [ "-verbose"       , "" (*"-verbose";*)
    ; "CPP"            , sprintf "%s %s -E" context.c_compiler context.ocamlc_cflags
    ; "PA_CPP"         , sprintf "%s %s -undef -traditional -x c -E" context.c_compiler
                           context.ocamlc_cflags
    ; "CC"             , sprintf "%s %s" context.c_compiler context.ocamlc_cflags
    ; "CXX"            , String.concat ~sep:" " (context.c_compiler :: cxx_flags)
    ; "ocaml_bin"      , Path.to_string context.ocaml_bin
    ; "OCAML"          , Path.to_string context.ocaml
    ; "OCAMLC"         , Path.to_string context.ocamlc
    ; "OCAMLOPT"       , Path.to_string ocamlopt
    ; "ocaml_version"  , context.version
    ; "ocaml_where"    , Path.to_string context.stdlib_dir
    ; "ARCH_SIXTYFOUR" , string_of_bool context.arch_sixtyfour
    ; "MAKE"           , make
    ; "null"           , Path.to_string Config.dev_null
    ]
    |> String_map.of_alist
    |> function
    | Ok x -> x
    | Error _ -> assert false
  in
  { context
  ; libs
  ; stanzas
  ; packages
  ; aliases
  ; file_tree
  ; rules = []
  ; stanzas_to_consider_for_install
  ; known_targets_by_src_dir_so_far = Path.Map.empty
  ; artifacts
  ; cxx_flags
  ; vars
  ; ppx_drivers = Hashtbl.create 32
  ; ppx_dir = Path.of_string (sprintf "_build/.ppx/%s" context.name)
  }

let add_rule t ?sandbox build =
  let rule = Build_interpret.Rule.make ?sandbox build in
  t.rules <- rule :: t.rules;
  t.known_targets_by_src_dir_so_far <-
    Path.Set.fold rule.targets ~init:t.known_targets_by_src_dir_so_far
      ~f:(fun path acc ->
        match Path.extract_build_context path with
        | None -> acc
        | Some (_, path) ->
          let dir = Path.parent path in
          let fn = Path.basename path in
          let files =
            match Path.Map.find dir acc with
            | None -> String_set.singleton fn
            | Some set -> String_set.add fn set
          in
          Path.Map.add acc ~key:dir ~data:files)

let add_rules t ?sandbox builds =
  List.iter builds ~f:(add_rule t ?sandbox)

let sources_and_targets_known_so_far t ~src_path =
  let sources =
    match File_tree.find_dir t.file_tree src_path with
    | None -> String_set.empty
    | Some dir -> File_tree.Dir.files dir
  in
  match Path.Map.find src_path t.known_targets_by_src_dir_so_far with
  | None -> sources
  | Some set -> String_set.union sources set


module Libs = struct
  open Build.O
  open Lib_db

  let find t ~from name = find t.libs ~from name

  let requires_file ~dir ~item =
    Path.relative dir (item ^ ".requires.sexp")

  let load_deps t ~dir fn =
    Build.read_sexp fn (fun sexp ->
      Sexp.Of_sexp.(list string) sexp
      |> List.map ~f:(fun name -> Lib_db.find_exn t.libs ~from:dir name))

  let load_requires t ~dir ~item =
    load_deps t ~dir (requires_file ~dir ~item)

  let runtime_deps_file ~dir ~item =
    Path.relative dir (item ^ ".runtime-deps.sexp")

  let load_runtime_deps t ~dir ~item =
    load_deps t ~dir (runtime_deps_file ~dir ~item)

  let with_fail ~fail build =
    match fail with
    | None -> build
    | Some f -> Build.fail f >>> build

  let closure t ~dir ~dep_kind lib_deps =
    let internals, externals, fail = Lib_db.interpret_lib_deps t.libs ~dir lib_deps in
    with_fail ~fail
      (Build.record_lib_deps ~dir ~kind:dep_kind lib_deps
       >>>
       Build.all
         (List.map internals ~f:(fun ((dir, lib) : Lib.Internal.t) ->
            load_requires t ~dir ~item:lib.name))
       >>^ (fun internal_deps ->
         let externals =
           Findlib.closure externals
             ~required_by:dir
             ~local_public_libs:(local_public_libs t.libs)
           |> List.map ~f:(fun pkg -> Lib.External pkg)
         in
         Lib.remove_dups_preserve_order
           (List.concat (externals :: internal_deps) @
            List.map internals ~f:(fun x -> Lib.Internal x))))

  let closed_ppx_runtime_deps_of t ~dir ~dep_kind lib_deps =
    let internals, externals, fail = Lib_db.interpret_lib_deps t.libs ~dir lib_deps in
    with_fail ~fail
      (Build.record_lib_deps ~dir ~kind:dep_kind lib_deps
       >>>
       Build.all
         (List.map internals ~f:(fun ((dir, lib) : Lib.Internal.t) ->
            load_runtime_deps t ~dir ~item:lib.name))
       >>^ (fun libs ->
         let externals =
           Findlib.closed_ppx_runtime_deps_of externals
             ~required_by:dir
             ~local_public_libs:(local_public_libs t.libs)
           |> List.map ~f:(fun pkg -> Lib.External pkg)
         in
         Lib.remove_dups_preserve_order (List.concat (externals :: libs))))

  let lib_is_available t ~from name = lib_is_available t.libs ~from name

  let add_select_rules t ~dir lib_deps =
    List.iter (Lib_db.resolve_selects t.libs ~from:dir lib_deps) ~f:(fun { dst_fn; src_fn } ->
      let src = Path.relative dir src_fn in
      let dst = Path.relative dir dst_fn in
      add_rule t
        (Build.path src
         >>>
         Build.action_context_independent ~targets:[dst]
           (Copy_and_add_line_directive (src, dst))))

  let write_deps fn =
    Build.write_sexp fn (fun l -> Sexp.To_sexp.(list string) (List.map l ~f:Lib.best_name))

  let real_requires t ~dir ~dep_kind ~item ~libraries ~preprocess ~virtual_deps =
    let all_pps =
      List.map (Preprocess_map.pps preprocess) ~f:Pp.to_string
    in
    let requires_file = requires_file ~dir ~item in
    add_rule t
      (Build.record_lib_deps ~dir ~kind:dep_kind (List.map virtual_deps ~f:Lib_dep.direct)
       >>>
       Build.fanout
         (closure t ~dir ~dep_kind libraries)
         (closed_ppx_runtime_deps_of t ~dir ~dep_kind
            (List.map all_pps ~f:Lib_dep.direct))
       >>>
       Build.arr (fun (libs, rt_deps) ->
         Lib.remove_dups_preserve_order (libs @ rt_deps))
       >>>
      write_deps requires_file);
    load_deps t ~dir requires_file

  let requires t ~dir ~dep_kind ~item ~libraries ~preprocess ~virtual_deps =
    let real_requires =
      real_requires t ~dir ~dep_kind ~item ~libraries ~preprocess ~virtual_deps
    in
    let requires =
      if t.context.merlin then
        (* We don't depend on the dot_merlin directly, otherwise everytime it changes we
           would have to rebuild everything.

           .merlin-exists depends on the .merlin and is an empty file. Depending on it
           forces the generation of the .merlin but not recompilation when it
           changes. Maybe one day we should add [Build.path_exists] to do the same in
           general. *)
        Build.path (Path.relative dir ".merlin-exists")
        >>>
        real_requires
      else
        real_requires
    in
    (requires, real_requires)

  let setup_runtime_deps t ~dir ~dep_kind ~item ~libraries ~ppx_runtime_libraries =
    let runtime_deps_file = runtime_deps_file ~dir ~item in
    add_rule t
      (Build.fanout
         (closure t ~dir ~dep_kind (List.map ppx_runtime_libraries ~f:Lib_dep.direct))
         (closed_ppx_runtime_deps_of t ~dir ~dep_kind libraries)
       >>>
       Build.arr (fun (rt_deps, rt_deps_of_deps) ->
         Lib.remove_dups_preserve_order (rt_deps @ rt_deps_of_deps))
       >>>
       write_deps runtime_deps_file)
end

module Deps = struct
  open Build.O
  open Dep_conf

  let dep t ~dir = function
    | File  s -> Build.path (Path.relative dir (expand_vars t ~dir s))
    | Alias s -> Build.path (Alias.file (Alias.make ~dir (expand_vars t ~dir s)))
    | Glob_files s -> begin
        let path = Path.relative dir (expand_vars t ~dir s) in
        let dir = Path.parent path in
        let s = Path.basename path in
        match Glob_lexer.parse_string s with
        | Ok re ->
          Build.paths_glob ~dir (Re.compile re)
        | Error (_pos, msg) ->
          die "invalid glob in %s/jbuild: %s" (Path.to_string dir) msg
      end
    | Files_recursively_in s ->
      let path = Path.relative dir (expand_vars t ~dir s) in
      Build.files_recursively_in ~dir:path ~file_tree:t.file_tree

  let interpret t ~dir l =
    let rec loop acc = function
      | [] -> acc
      | d :: l ->
        loop (acc >>> dep t ~dir d) l
    in
    loop (Build.return ()) l

  let only_plain_file t ~dir = function
    | File s -> Some (Path.relative dir (expand_vars t ~dir s))
    | Alias _ -> None
    | Glob_files _ -> None
    | Files_recursively_in _ -> None

  let only_plain_files t ~dir l = List.map l ~f:(only_plain_file t ~dir)
end

module Pkg_version = struct
  open Build.O

  let spec_file sctx (p : Package.t) =
    Path.relative (Path.append sctx.context.build_dir p.path)
      (sprintf "%s.version.sexp" p.name)

  let read sctx p = Build.read_sexp (spec_file sctx p) Sexp.Of_sexp.(option string)

  let set sctx p get =
    let fn = spec_file sctx p in
    add_rule sctx (get >>> Build.write_sexp fn Sexp.To_sexp.(option string));
    Build.read_sexp fn Sexp.Of_sexp.(option string)
end

module Action = struct
  open Build.O
  module U = Action.Mini_shexp.Unexpanded

  type resolved_forms =
    { (* Mapping from ${...} forms to their resolutions *)
      artifacts : Action.var_expansion String_map.t
    ; (* Failed resolutions *)
      failures  : fail list
    ; (* All "name" for ${lib:name:...}/${lib-available:name} forms *)
      lib_deps  : Build.lib_deps
    ; vdeps     : (unit, Action.var_expansion) Build.t String_map.t
    }

  let add_artifact ?lib_dep acc ~var result =
    let lib_deps =
      match lib_dep with
      | None -> acc.lib_deps
      | Some (lib, kind) -> String_map.add acc.lib_deps ~key:lib ~data:kind
    in
    match result with
    | Ok path ->
      { acc with
        artifacts = String_map.add acc.artifacts ~key:var ~data:path
      ; lib_deps
      }
    | Error fail ->
      { acc with
        failures = fail :: acc.failures
      ; lib_deps
      }

  let map_result = function
    | Ok x -> Ok (Action.Path x)
    | Error _ as e -> e

  let extract_artifacts sctx ~dir ~dep_kind ~package_context t =
    let init =
      { artifacts = String_map.empty
      ; failures  = []
      ; lib_deps  = String_map.empty
      ; vdeps     = String_map.empty
      }
    in
    U.fold_vars t ~init ~f:(fun acc loc var ->
      let module A = Artifacts in
      match String.lsplit2 var ~on:':' with
      | Some ("exe"     , s) -> add_artifact acc ~var (Ok (Path (Path.relative dir s)))
      | Some ("path"    , s) -> add_artifact acc ~var (Ok (Path (Path.relative dir s)))
      | Some ("bin"     , s) ->
        add_artifact acc ~var (A.binary (artifacts sctx) s |> map_result)
      | Some ("lib"     , s)
      | Some ("libexec" , s) ->
        let lib_dep, res = A.file_of_lib (artifacts sctx) ~from:dir s in
        add_artifact acc ~var ~lib_dep:(lib_dep, dep_kind) (map_result res)
      | Some ("lib-available", lib) ->
        add_artifact acc ~var ~lib_dep:(lib, Optional)
          (Ok (Str (string_of_bool (Libs.lib_is_available sctx ~from:dir lib))))
      (* CR-someday jdimino: allow this only for (jbuild_version jane_street) *)
      | Some ("findlib" , s) ->
        let lib_dep, res =
          A.file_of_lib (artifacts sctx) ~from:dir s ~use_provides:true
        in
        add_artifact acc ~var ~lib_dep:(lib_dep, Required) (map_result res)
      | Some ("version", s) -> begin
          match Pkgs.resolve package_context s with
          | Ok p ->
            let x =
              Pkg_version.read sctx p >>^ function
              | None -> Action.Str ""
              | Some s -> Str s
            in
            { acc with vdeps = String_map.add acc.vdeps ~key:var ~data:x }
          | Error s ->
            { acc with failures = { fail = fun () -> Loc.fail loc "%s" s } :: acc.failures }
        end
      | _ -> acc)

  let expand_var =
    let dep_exn name = function
      | Some dep -> dep
      | None -> die "cannot use ${%s} with files_recursively_in" name
    in
    fun sctx ~artifacts ~targets ~deps var_name ->
      match String_map.find var_name artifacts with
      | Some exp -> exp
      | None ->
        match var_name with
        | "@" -> Action.Paths targets
        | "<" -> (match deps with
          | []        -> Str ""
          | dep1 :: _ -> Path (dep_exn var_name dep1))
        | "^" ->
          Paths (List.map deps ~f:(dep_exn var_name))
        | "ROOT" -> Path sctx.context.build_dir
        | var ->
          match expand_var_no_root sctx var with
          | Some s -> Str s
          | None -> Not_found

  let run sctx t ~dir ~dep_kind ~targets ~deps ~package_context =
    let forms = extract_artifacts sctx ~dir ~dep_kind ~package_context t in
    let build =
      Build.record_lib_deps_simple ~dir forms.lib_deps
      >>>
      Build.path_set
        (String_map.fold forms.artifacts ~init:Path.Set.empty
           ~f:(fun ~key:_ ~data:exp acc ->
             match exp with
             | Action.Path p -> Path.Set.add p acc
             | Paths ps -> Path.Set.union acc (Path.Set.of_list ps)
             | Not_found | Str _ -> acc))
      >>>
      let vdeps = String_map.bindings forms.vdeps in
      Build.all (List.map vdeps ~f:snd)
      >>^ (fun vals ->
        let artifacts =
          List.fold_left2 vdeps vals ~init:forms.artifacts ~f:(fun acc (var, _) value ->
            String_map.add acc ~key:var ~data:value)
        in
        U.expand sctx.context dir t
          ~f:(expand_var sctx ~artifacts ~targets ~deps))
      >>>
      Build.action_dyn () ~context:sctx.context ~dir ~targets
    in
    match forms.failures with
    | [] -> build
    | fail :: _ -> Build.fail fail >>> build
end

module PP = struct
  open Build.O

  let pp_fname fn =
    let fn, ext = Filename.split_extension fn in
    (* We need to to put the .pp before the .ml so that the compiler realises that
       [foo.pp.mli] is the interface for [foo.pp.ml] *)
    fn ^ ".pp" ^ ext

  let pped_module ~dir (m : Module.t) ~f =
    let ml_pp_fname = pp_fname m.impl.name in
    f Ml_kind.Impl (Path.relative dir m.impl.name) (Path.relative dir ml_pp_fname);
    let intf =
      Option.map m.intf ~f:(fun intf ->
        let pp_fname = pp_fname intf.name in
        f Intf (Path.relative dir intf.name) (Path.relative dir pp_fname);
        {intf with name = pp_fname})
    in
    { m with
      impl = { m.impl with name = ml_pp_fname }
    ; intf
    }

  let ppx_drivers = Hashtbl.create 32

  let migrate_driver_main = "ocaml-migrate-parsetree.driver-main"

  let build_ppx_driver sctx ~dir ~dep_kind ~target pp_names ~driver =
    let ctx = sctx.context in
    let mode = Context.best_mode ctx in
    let compiler = Option.value_exn (Context.compiler ctx mode) in
    let pp_names = pp_names @ [migrate_driver_main] in
    let libs =
      Libs.closure sctx ~dir ~dep_kind (List.map pp_names ~f:Lib_dep.direct)
    in
    let libs =
      (* Put the driver back at the end, just before migrate_driver_main *)
      match driver with
      | None -> libs
      | Some driver ->
        libs >>^ fun libs ->
        let is_driver name = name = driver || name = migrate_driver_main in
        let libs, drivers =
          List.partition_map libs ~f:(fun lib ->
            if (match lib with
              | External pkg -> is_driver pkg.name
              | Internal (_, lib) ->
                is_driver lib.name ||
                match lib.public with
                | None -> false
                | Some { name; _ } -> is_driver name)
            then
              Inr lib
            else
              Inl lib)
        in
        let user_driver, migrate_driver =
          List.partition_map drivers ~f:(fun lib ->
            if Lib.best_name lib = migrate_driver_main then
              Inr lib
            else
              Inl lib)
        in
        libs @ user_driver @ migrate_driver
    in
    (* Provide a better error for migrate_driver_main given that this is an implicit
       dependency *)
    let libs =
      match Libs.find sctx ~from:dir migrate_driver_main with
      | None ->
        Build.fail { fail = fun () ->
          die "@{<error>Error@}: I couldn't find '%s'.\n\
               I need this library in order to use ppx rewriters.\n\
               See the manual for details.\n\
               Hint: opam install ocaml-migrate-parsetree"
            migrate_driver_main
        }
        >>>
        libs
      | Some _ ->
        libs
    in
    add_rule sctx
      (libs
       >>>
       Build.dyn_paths (Build.arr (Lib.archive_files ~mode ~ext_lib:ctx.ext_lib))
       >>>
       Build.run ~context:ctx (Dep compiler)
         [ A "-o" ; Target target
         ; Dyn (Lib.link_flags ~mode)
         ])

  let get_ppx_driver sctx pps ~dir ~dep_kind =
    let driver, names =
      match List.rev_map pps ~f:Pp.to_string with
      | [] -> (None, [])
      | driver :: rest ->
        (Some driver, List.sort rest ~cmp:String.compare @ [driver])
    in
    let key =
      match names with
      | [] -> "+none+"
      | _  -> String.concat names ~sep:"+"
    in
    match Hashtbl.find ppx_drivers key with
    | Some x -> x
    | None ->
      let ppx_dir = Path.relative sctx.ppx_dir key in
      let exe = Path.relative ppx_dir "ppx.exe" in
      build_ppx_driver sctx names ~dir ~dep_kind ~target:exe ~driver;
      Hashtbl.add ppx_drivers ~key ~data:exe;
      exe

  let target_var = String_with_vars.of_string "${@}" ~loc:Loc.none
  let root_var   = String_with_vars.of_string "${ROOT}" ~loc:Loc.none

  let cookie_library_name lib_name =
    match lib_name with
    | None -> []
    | Some name -> ["--cookie"; sprintf "library-name=%S" name]

  (* Generate rules for the reason modules in [modules] and return a
     a new module with only OCaml sources *)
  let setup_reason_rules sctx ~dir (m : Module.t) =
    let ctx = sctx.context in
    let refmt = resolve_program sctx "refmt" ~hint:"opam install reason" in
    let rule src target =
      let src_path = Path.relative dir src in
      Build.run ~context:ctx refmt
        [ A "--print"
        ; A "binary"
        ; Dep src_path ]
        ~stdout_to:(Path.relative dir target) in
    let impl =
      match m.impl.syntax with
      | OCaml -> m.impl
      | Reason ->
        let ml = Module.File.to_ocaml m.impl in
        add_rule sctx (rule m.impl.name ml.name);
        ml in
    let intf =
      Option.map m.intf ~f:(fun f ->
        match f.syntax with
        | OCaml -> f
        | Reason ->
          let mli = Module.File.to_ocaml f in
          add_rule sctx (rule f.name mli.name);
          mli) in
    { m with impl ; intf }

  (* Generate rules to build the .pp files and return a new module map where all filenames
     point to the .pp files *)
  let pped_modules sctx ~dir ~dep_kind ~modules ~preprocess ~preprocessor_deps ~lib_name
        ~package_context =
    let preprocessor_deps = Deps.interpret sctx ~dir preprocessor_deps in
    String_map.map modules ~f:(fun (m : Module.t) ->
      let m = setup_reason_rules sctx ~dir m in
      match Preprocess_map.find m.name preprocess with
      | No_preprocessing -> m
      | Action action ->
        pped_module m ~dir ~f:(fun _kind src dst ->
          add_rule sctx
            (preprocessor_deps
             >>>
             Build.path src
             >>>
             Action.run sctx
               (Redirect
                  (Stdout,
                   target_var,
                   Chdir (root_var,
                          action)))
               ~dir
               ~dep_kind
               ~targets:[dst]
               ~deps:[Some src]
               ~package_context))
      | Pps { pps; flags } ->
        let ppx_exe = get_ppx_driver sctx pps ~dir ~dep_kind in
        pped_module m ~dir ~f:(fun kind src dst ->
          add_rule sctx
            (preprocessor_deps
             >>>
             Build.run ~context:sctx.context
               (Dep ppx_exe)
               [ As flags
               ; A "--dump-ast"
               ; As (cookie_library_name lib_name)
               ; A "-o"; Target dst
               ; Ml_kind.ppx_driver_flag kind; Dep src
               ])
        )
    )
end

let expand_and_eval_set ~dir set ~standard =
  let open Build.O in
  match Ordered_set_lang.Unexpanded.files set |> String_set.elements with
  | [] ->
    let set = Ordered_set_lang.Unexpanded.expand set ~files_contents:String_map.empty in
    Build.return (Ordered_set_lang.eval_with_standard set ~standard)
  | files ->
    let paths = List.map files ~f:(Path.relative dir) in
    Build.paths paths
    >>>
    Build.arr (fun () ->
      let files_contents =
        List.map2 files paths ~f:(fun fn path ->
          (fn, Sexp_load.single (Path.to_string path)))
        |> String_map.of_alist_exn
      in
      let set = Ordered_set_lang.Unexpanded.expand set ~files_contents in
      Ordered_set_lang.eval_with_standard set ~standard)
