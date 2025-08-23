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

let local_library
      ~root_module
      ~special_builtin_support
      ~is_multi_dir
      ~main_module_name
      ~dir
  =
  Dyn.record
    [ "path", Path.Source.to_local dir |> Path.Local.to_dyn
    ; "main_module_name", Dyn.option Module_name.to_dyn main_module_name
    ; "include_subdirs_unqualified", Dyn.Bool is_multi_dir
    ; "special_builtin_support", Dyn.option Module_name.to_dyn special_builtin_support
    ; "root_module", Dyn.(option Root_module_data.to_dyn) root_module
    ]
;;

let is_multi_dir dir_contents =
  match Dir_contents.dirs dir_contents with
  | _ :: _ :: _ -> true
  | _ -> false
;;

let rule sctx ~requires_link ~main =
  let open Action_builder.O in
  let* () = Action_builder.return () in
  let* locals, externals =
    Memo.Lazy.force requires_link
    |> Resolve.Memo.read
    >>| List.partition_map ~f:(fun lib ->
      match Lib.Local.of_lib lib with
      | Some x -> Left x
      | None -> Right lib)
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
      let* is_multi_dir =
        Action_builder.of_memo
          (let open Memo.O in
           Dir_contents.get sctx ~dir >>| is_multi_dir)
      in
      let+ root_module =
        match Lib_info.root_module info with
        | None -> Action_builder.return None
        | Some name ->
          let+ entries =
            let requires_compile =
              let open Memo.O in
              let* db = Scope.DB.find_by_dir dir >>| Scope.libs in
              Lib.Compile.for_lib ~allow_overlaps:false db (Lib.Local.to_lib x)
              |> Lib.Compile.direct_requires
            in
            Root_module.entries sctx ~requires_compile
          in
          Some { Root_module_data.name; entries }
      in
      local_library
        ~root_module
        ~special_builtin_support
        ~dir:(Path.Build.drop_build_context_exn dir)
        ~is_multi_dir
        ~main_module_name:
          (match Lib_info.main_module_name info with
           | From _ -> None
           | This x -> x))
  in
  let externals =
    let available =
      [ "threads.posix"; "re"; "seq" ] |> List.rev_map ~f:Lib_name.of_string
    in
    List.filter_map externals ~f:(fun lib ->
      let name = Lib.name lib in
      if List.mem available ~equal:Lib_name.equal name then None else Some name)
  in
  Format.asprintf
    "%a@."
    Pp.to_fmt
    (Pp.concat
       ~sep:Pp.cut
       [ Pp.verbatim "open Types"
       ; def "external_libraries" (Dyn.list Lib_name.to_dyn externals)
       ; Pp.nop
       ; def "local_libraries" (List locals)
       ; Pp.nop
       ; def "main" main
       ; Pp.nop
       ]
     |> Pp.vbox)
;;

let make_main dir_contents =
  let open Action_builder.O in
  let+ () = Action_builder.return () in
  local_library
    ~root_module:None
    ~special_builtin_support:None
    ~dir:(Dir_contents.source_dir dir_contents |> Option.value_exn |> Source_tree.Dir.path)
    ~is_multi_dir:(is_multi_dir dir_contents)
    ~main_module_name:None
;;

let gen_rules sctx (exes : Executables.t) ~dir ~requires_link dir_contents =
  Memo.Option.iter exes.bootstrap_info ~f:(fun fname ->
    Action_builder.write_file_dyn
      (Path.Build.relative dir fname)
      (let open Action_builder.O in
       let* main = make_main dir_contents in
       rule sctx ~requires_link ~main)
    |> Super_context.add_rule sctx ~loc:exes.buildable.loc ~dir)
;;
