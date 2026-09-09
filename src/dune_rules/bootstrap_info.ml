open Import

let def name dyn =
  let open Pp.O in
  Pp.box ~indent:2 (Pp.textf "let %s = " name ++ Dyn.pp dyn)
;;

module Root_module_data = struct
  type t =
    { name : Module_name.t
    ; entries : Module_name.t list
    }

  let to_dyn { name; entries } =
    let open Dyn in
    record
      [ "name", string (Module_name.to_string name)
      ; "entries", list Module_name.to_dyn entries
      ]
  ;;
end

module Include_subdirs = struct
  type t =
    | Unqualified
    | Qualified of (string list * string list) list
    | No

  let to_dyn = function
    | Unqualified -> Dyn.variant "Unqualified" []
    | Qualified dirs ->
      Dyn.variant "Qualified" [ Dyn.(list (pair (list string) (list string))) dirs ]
    | No -> Dyn.variant "No" []
  ;;
end

let local_library
      ~root_module
      ~special_builtin_support
      ~include_subdirs
      ~main_module_name
      ~dir
  =
  Dyn.record
    [ "path", Path.Source.to_local dir |> Path.Local.to_dyn
    ; "main_module_name", Dyn.option Module_name.to_dyn main_module_name
    ; "include_subdirs", Include_subdirs.to_dyn include_subdirs
    ; "special_builtin_support", Dyn.option Module_name.to_dyn special_builtin_support
    ; "root_module", Dyn.(option Root_module_data.to_dyn) root_module
    ]
;;

let for_ = Compilation_mode.Ocaml

let include_subdirs sctx dir_contents =
  let open Memo.O in
  Dir_contents.ml dir_contents ~for_
  >>| Ml_sources.include_subdirs
  >>= function
  | Import.Include_subdirs.No -> Memo.return Include_subdirs.No
  | Include Unqualified -> Memo.return Include_subdirs.Unqualified
  | Include (Qualified { dirs = [] }) -> Memo.return (Include_subdirs.Qualified [])
  | Include (Qualified { dirs }) ->
    let dir = Dir_contents.dir dir_contents in
    let* expander = Super_context.expander sctx ~dir in
    let+ dirs =
      Memo.parallel_map dirs ~f:(fun binding ->
        File_binding_expand.expand binding ~dir ~f:(fun sw ->
          Action_builder.evaluate_and_collect_facts (Expander.expand_str expander sw)
          >>| fst))
    in
    let root = Path.Build.local dir in
    let components path =
      Path.Local.descendant path ~of_:root
      |> Option.value_exn
      |> Path.Local.explode
      |> Filename.L.to_string
    in
    let dirs =
      List.filter_map dirs ~f:(fun binding ->
        Option.map (File_binding.Expanded.dst binding) ~f:(fun dst ->
          ( components (File_binding.Expanded.src binding |> Path.Build.local)
          , components (Path.Local.relative root dst) )))
    in
    Include_subdirs.Qualified dirs
;;

let make_root_module sctx ~name compile_info =
  let open Action_builder.O in
  let+ entries =
    let requires_compile = Lib.Compile.user_written_requires_no_loc compile_info ~for_ in
    Root_module.entries sctx ~requires_compile ~for_
  in
  { Root_module_data.name; entries }
;;

let rule sctx ~requires_link ~main =
  let open Action_builder.O in
  let* () = Action_builder.return () in
  let* locals =
    Memo.Lazy.force requires_link
    |> Resolve.Memo.read
    >>| List.filter_map ~f:Lib.Local.of_lib
  in
  let+ locals =
    Action_builder.List.map locals ~f:(fun x ->
      let info = Lib.Local.info x in
      let dir = Lib_info.src_dir info in
      let special_builtin_support =
        match Lib_info.special_builtin_support info with
        | Some (_loc, Build_info { data_module; _ }) -> Some data_module
        | _ -> None
      in
      let open Action_builder.O in
      let* include_subdirs =
        Action_builder.of_memo
          (let open Memo.O in
           Dir_contents.get sctx ~dir >>= include_subdirs sctx)
      in
      let+ root_module =
        match Lib_info.root_module info with
        | None -> Action_builder.return None
        | Some name ->
          let* compile_info =
            (let open Memo.O in
             let+ db = Scope.DB.find_by_dir dir >>| Scope.libs in
             Lib.Compile.for_lib ~allow_overlaps:false db (Lib.Local.to_lib x))
            |> Action_builder.of_memo
          in
          make_root_module sctx ~name compile_info >>| Option.some
      in
      local_library
        ~root_module
        ~special_builtin_support
        ~dir:(Path.Build.drop_build_context_exn dir)
        ~include_subdirs
        ~main_module_name:
          (match Lib_info.main_module_name info with
           | From _ -> None
           | This x -> x))
  in
  Format.asprintf
    "%a@."
    Pp.to_fmt
    (Pp.concat
       ~sep:Pp.cut
       [ Pp.verbatim "open Types"
       ; Pp.nop
       ; def "local_libraries" (List locals)
       ; Pp.nop
       ; def "main" main
       ; Pp.nop
       ]
     |> Pp.vbox)
;;

let make_main sctx ~root_module compile_info dir_contents =
  let open Action_builder.O in
  let* () = Action_builder.return () in
  let* root_module =
    match root_module with
    | None -> Action_builder.return None
    | Some name -> make_root_module sctx ~name compile_info >>| Option.some
  in
  let+ include_subdirs = include_subdirs sctx dir_contents |> Action_builder.of_memo in
  local_library
    ~root_module
    ~special_builtin_support:None
    ~dir:(Dir_contents.source_dir dir_contents |> Option.value_exn |> Source_tree.Dir.path)
    ~include_subdirs
    ~main_module_name:None
;;

let gen_rules sctx (exes : Executables.t) ~dir compile_info dir_contents =
  Memo.Option.iter exes.bootstrap_info ~f:(fun fname ->
    let requires_link = Lib.Compile.requires_link compile_info ~for_ in
    Action_builder.write_file_dyn
      (Path.Build.relative dir fname)
      (let open Action_builder.O in
       let* main =
         let root_module = Option.map exes.buildable.modules.root_module ~f:snd in
         make_main sctx ~root_module compile_info dir_contents
       in
       rule sctx ~requires_link ~main)
    |> Super_context.add_rule sctx ~loc:exes.buildable.loc ~dir)
;;
