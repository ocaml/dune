open Import
open Memo.O

module Source_tree_map_reduce =
  Source_tree.Dir.Make_map_reduce
    (Action_builder)
    (Monoid.Appendable_list (struct
         type t = Command.Args.without_targets Command.Args.t
       end))

let default_context_flags (ctx : Build_context.t) ocaml_config ~project =
  let cflags = Ocaml_config.ocamlc_cflags ocaml_config in
  let c, cxx =
    let cxxflags =
      List.filter cflags ~f:(fun s -> not (String.is_prefix s ~prefix:"-std="))
    in
    match Dune_project.use_standard_c_and_cxx_flags project with
    | None | Some false -> Action_builder.(return cflags, return cxxflags)
    | Some true ->
      let fdiagnostics_color =
        Cxx_flags.ccomp_type ctx |> Action_builder.map ~f:Cxx_flags.fdiagnostics_color
      in
      let open Action_builder.O in
      let c =
        let+ fdiagnostics_color = fdiagnostics_color in
        List.concat
          [ cflags; Ocaml_config.ocamlc_cppflags ocaml_config; fdiagnostics_color ]
      in
      let cxx =
        let+ fdiagnostics_color = fdiagnostics_color
        and+ db_flags = Cxx_flags.get_flags ~for_:Compile ctx in
        List.concat [ db_flags; cxxflags; fdiagnostics_color ]
      in
      c, cxx
  in
  Foreign_language.Dict.make ~c ~cxx
;;

let foreign_flags_env =
  let f =
    Env_stanza_db_flags.flags
      ~name:"foreign_flags_env"
      ~root:(fun ctx project ->
        let* context = Context.DB.get ctx in
        let+ ocaml = Context.ocaml context in
        default_context_flags (Context.build_context context) ocaml.ocaml_config ~project)
      ~f:(fun ~parent expander (env : Dune_env.config) ->
        let open Memo.O in
        let+ parent = parent in
        let foreign_flags lang =
          Foreign_language.Dict.get env.foreign_flags lang
          |> Expander.expand_and_eval_set
               expander
               ~standard:(Foreign_language.Dict.get parent lang)
        in
        Foreign_language.Dict.make ~c:(foreign_flags C) ~cxx:(foreign_flags Cxx))
  in
  fun ~dir ->
    let* () = Memo.return () in
    (Staged.unstage f) dir
;;

let () = Fdecl.set Expander.foreign_flags foreign_flags_env

let default_foreign_flags ~dir ~language =
  foreign_flags_env ~dir
  >>| (fun dict -> Foreign_language.Dict.get dict language)
  |> Action_builder.of_memo_join
;;

let foreign_flags t ~dir ~expander ~flags ~language =
  let context = Super_context.context t in
  let default = default_foreign_flags ~dir ~language in
  let open Action_builder.O in
  let name = Foreign_language.proper_name language in
  let flags =
    let* ccg =
      Action_builder.of_memo
        (let open Memo.O in
         let+ ocaml = Context.ocaml context in
         Lib_config.cc_g ocaml.lib_config)
    in
    let+ l = Expander.expand_and_eval_set expander flags ~standard:default in
    l @ ccg
  in
  Action_builder.memoize ~cutoff:(List.equal String.equal) (sprintf "%s flags" name) flags
;;

(* Compute command line flags for the [include_dirs] field of [Foreign.Stubs.t]
   and track all files in specified directories as [Hidden_deps]
   dependencies. *)
let include_dir_flags ~expander ~dir ~include_dirs =
  let lib_dir =
    let scope = Scope.DB.find_by_dir dir in
    fun loc lib_name ->
      let open Resolve.Memo.O in
      let* libs = Memo.map scope ~f:Scope.libs |> Resolve.Memo.lift_memo in
      let+ lib = Lib.DB.resolve libs (loc, lib_name) in
      Lib_info.src_dir (Lib.info lib)
  in
  let args_of_include_dir include_dir =
    Resolve.Memo.args
    @@
    let open Resolve.Memo.O in
    let+ loc, include_dir =
      match (include_dir : Foreign.Stubs.Include_dir.Without_include.t) with
      | Dir dir ->
        Resolve.Memo.return (String_with_vars.loc dir, Expander.expand_path expander dir)
      | Lib (loc, lib_name) ->
        let+ lib_dir = lib_dir loc lib_name in
        loc, Action_builder.return lib_dir
    in
    Command.Args.Dyn
      (let open Action_builder.O in
       let* include_dir = include_dir in
       let+ dep_args =
         match Path.extract_build_context_dir include_dir with
         | None ->
           (* This branch corresponds to an external directory. The
              current implementation tracks its contents
              NON-recursively. *)
           (* TODO: Track the contents recursively. One way to implement
              this is to change [Build_system.Loaded.Non_build] so that it
              contains not only files but also directories and traverse
              them recursively in [Build_system.Exported.Pred]. *)
           let+ () =
             let error msg =
               User_error.raise
                 ~loc
                 [ Pp.textf "Unable to read the include directory."
                 ; Pp.textf "Reason: %s." msg
                 ]
             in
             Action_builder.of_memo
             @@ Fs_memo.is_directory (Path.as_outside_build_dir_exn include_dir)
             >>| function
             | Error msg -> error (Unix_error.Detailed.to_string_hum msg)
             | Ok true -> ()
             | Ok false ->
               error (sprintf "%S is not a directory" (Path.to_string include_dir))
           in
           let deps =
             File_selector.of_predicate_lang ~dir:include_dir Predicate_lang.true_
             |> Dep.file_selector
             |> Dep.Set.singleton
           in
           Command.Args.Hidden_deps deps
         | Some (build_dir, source_dir) ->
           Action_builder.return
           @@ Command.Args.Dyn
                ((* This branch corresponds to a source directory. We
                    track its contents recursively. *)
                 Action_builder.of_memo (Source_tree.find_dir source_dir)
                 >>= function
                 | None ->
                   User_error.raise
                     ~loc
                     [ Pp.textf
                         "Include directory %S does not exist."
                         (Path.reach ~from:(Path.build dir) include_dir)
                     ]
                 | Some dir ->
                   let+ l =
                     Source_tree_map_reduce.map_reduce
                       dir
                       ~traverse:Source_dir_status.Set.all
                       ~f:(fun t ->
                         let deps =
                           let dir =
                             Path.append_source build_dir (Source_tree.Dir.path t)
                           in
                           File_selector.of_predicate_lang ~dir Predicate_lang.true_
                           |> Dep.file_selector
                           |> Dep.Set.singleton
                         in
                         Command.Args.Hidden_deps deps
                         |> Appendable_list.singleton
                         |> Action_builder.return)
                   in
                   Command.Args.S (Appendable_list.to_list l))
       in
       Command.Args.S [ A "-I"; Path include_dir; dep_args ])
  in
  Command.Args.Dyn
    (let open Action_builder.O in
     let+ include_dirs_expanded =
       let expand = Expander.No_deps.expand expander ~mode:Single in
       Memo.List.concat_map
         include_dirs
         ~f:(Foreign.Stubs.Include_dir.expand_include ~expand ~dir:(Path.build dir))
       |> Action_builder.of_memo
     in
     Command.Args.S (List.map include_dirs_expanded ~f:args_of_include_dir))
;;

let build_c
  ~(kind : Foreign_language.t)
  ~sctx
  ~dir
  ~expander
  ~include_flags
  (loc, (src : Foreign.Source.t), dst)
  =
  let ctx = Super_context.context sctx in
  let* project = Dune_load.find_project ~dir in
  let use_standard_flags = Dune_project.use_standard_c_and_cxx_flags project in
  let* ocaml = Context.ocaml ctx in
  let base_flags =
    match kind with
    | Cxx -> Fdo.cxx_flags ctx
    | C ->
      (match use_standard_flags with
       | Some true -> Fdo.c_flags ctx
       | None | Some false ->
         (* In dune < 2.8 flags from ocamlc_config are always added *)
         let cfg = ocaml.ocaml_config in
         List.concat
           [ Ocaml_config.ocamlc_cflags cfg
           ; Ocaml_config.ocamlc_cppflags cfg
           ; Fdo.c_flags ctx
           ])
  in
  let* with_user_and_std_flags =
    Memo.map ~f:(Action_builder.map ~f:(List.append base_flags))
    @@
    match src.kind with
    | Ctypes field ->
      Memo.return
      @@
        (match field.build_flags_resolver with
        | Vendored { c_flags; c_library_flags = _ } ->
          foreign_flags sctx ~dir ~expander ~flags:c_flags ~language:C
        | Pkg_config ->
          let open Action_builder.O in
          let+ default_flags =
            let dir = Path.Build.parent_exn dst in
            default_foreign_flags ~dir ~language:C
          and+ pkg_config_flags =
            let lib = External_lib_name.to_string field.external_library_name in
            Pkg_config.Query.read ~dir (Cflags lib) sctx
          in
          default_flags @ pkg_config_flags)
    | Stubs { Foreign.Stubs.flags; _ } ->
      (* DUNE3 will have [use_standard_c_and_cxx_flags] enabled by default. To
         guide users toward this change we emit a warning when dune_lang is >=
         1.8, [use_standard_c_and_cxx_flags] is not specified in the
         [dune-project] file (thus defaulting to [true]), the [:standard] set of
         flags has been overridden and we are not in a vendored project *)
      let has_standard = Ordered_set_lang.Unexpanded.has_standard flags in
      let+ is_vendored =
        match Path.Build.drop_build_context dir with
        | Some src_dir -> Source_tree.is_vendored src_dir
        | None -> Memo.return false
      in
      if Dune_project.dune_version project >= (2, 8)
         && Option.is_none use_standard_flags
         && (not is_vendored)
         && not has_standard
      then
        User_warning.emit
          ~loc
          [ Pp.text
              "The flag set for these foreign sources overrides the `:standard` set of \
               flags. However the flags in this standard set are still added to the \
               compiler arguments by Dune. This might cause unexpected issues. You can \
               disable this warning by defining the option \
               `(use_standard_c_and_cxx_flags <bool>)` in your `dune-project` file. \
               Setting this option to `true` will effectively prevent Dune from silently \
               adding c-flags to the compiler arguments which is the new recommended \
               behaviour."
          ];
      foreign_flags sctx ~dir ~expander ~flags ~language:kind
  in
  let output_param =
    match ocaml.lib_config.ccomp_type with
    | Msvc -> [ Command.Args.Concat ("", [ A "/Fo"; Target dst ]) ]
    | Other _ -> [ A "-o"; Target dst ]
  in
  Super_context.add_rule
    sctx
    ~loc
    ~dir
    (let open Action_builder.With_targets.O in
     let src = Path.build (Foreign.Source.path src) in
     (* We have to execute the rule in the library directory as the .o is
        produced in the current directory *)
     let c_compiler =
       Super_context.resolve_program
         ~loc:None
         ~dir
         sctx
         (Ocaml_config.c_compiler ocaml.ocaml_config)
     in
     Command.run_dyn_prog
       ~dir:(Path.build dir)
       c_compiler
       ([ Command.Args.dyn with_user_and_std_flags
        ; S [ A "-I"; Path ocaml.lib_config.stdlib_dir ]
        ; include_flags
        ]
        @ output_param
        @ [ A "-c"; Dep src ])
     (* With sandboxing we get errors like: bar.c:2:19: fatal error: foo.cxx:
        No such file or directory #include "foo.cxx". (These errors happen only
        when compiling c files.) *)
     >>| Action.Full.add_sandbox Sandbox_config.no_sandboxing)
;;

(* TODO: [requires] is a confusing name, probably because it's too general: it
   looks like it's a list of libraries we depend on. *)
let build_o_files
  ~sctx
  ~foreign_sources
  ~(dir : Path.Build.t)
  ~expander
  ~requires
  ~dir_contents
  =
  let includes =
    let h_files =
      Dir_contents.dirs dir_contents
      |> List.fold_left ~init:[] ~f:(fun acc dc ->
        Dir_contents.text_files dc
        |> Filename.Set.fold ~init:acc ~f:(fun fn acc ->
          if String.is_suffix fn ~suffix:Foreign_language.header_extension
          then Path.relative (Path.build (Dir_contents.dir dc)) fn :: acc
          else acc))
    in
    Command.Args.S
      [ Hidden_deps (Dep.Set.of_files h_files)
      ; Resolve.args
          (let open Resolve.O in
           let+ libs = requires in
           Command.Args.S
             [ Lib_flags.L.c_include_flags libs sctx
             ; Hidden_deps (Lib_file_deps.deps libs ~groups:[ Header ])
             ])
      ]
  in
  let ctx = Super_context.context sctx in
  let* ocaml = Context.ocaml ctx in
  String.Map.to_list_map foreign_sources ~f:(fun obj (loc, (src : Foreign.Source.t)) ->
    let+ build_file =
      let include_flags =
        let extra_deps =
          let extra_deps, sandbox =
            match src.kind with
            | Stubs stubs -> Dep_conf_eval.unnamed stubs.extra_deps ~expander
            | Ctypes _ -> Action_builder.return (), Sandbox_config.default
          in
          (* We don't sandbox the C compiler, see comment in [build_file] about
             this. *)
          ignore sandbox;
          Action_builder.map extra_deps ~f:(fun () -> Command.Args.empty)
        in
        let extra_flags =
          include_dir_flags
            ~expander
            ~dir
            ~include_dirs:
              (match src.kind with
               | Stubs stubs -> stubs.include_dirs
               | Ctypes _ -> [])
        in
        Command.Args.S [ includes; extra_flags; Dyn extra_deps ]
      in
      let dst = Path.Build.relative dir (obj ^ ocaml.lib_config.ext_obj) in
      let+ () =
        build_c
          ~kind:(Foreign.Source.language src)
          ~sctx
          ~dir
          ~expander
          ~include_flags
          (loc, src, dst)
      in
      dst
    in
    Foreign.Source.mode src, Path.build build_file)
  |> Memo.all_concurrently
  >>| List.fold_left ~init:Mode.Map.empty ~f:(fun tbl (for_mode, file) ->
    Mode.Map.Multi.cons tbl for_mode file)
;;
