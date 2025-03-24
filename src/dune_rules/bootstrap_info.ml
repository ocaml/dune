open Import

let def name dyn =
  let open Pp.O in
  Pp.box ~indent:2 (Pp.textf "let %s = " name ++ Dyn.pp dyn)
;;

let flags flags =
  let open Dyn in
  list (pair (list string) (list string)) flags
;;

let rule sctx ~requires_link =
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
  let windows_system_values = [ "win32"; "win64"; "mingw"; "mingw64" ] in
  let build_flags =
    [ windows_system_values, [ "-ccopt"; "-D_UNICODE"; "-ccopt"; "-DUNICODE" ] ]
  in
  let link_flags =
    (* additional link flags keyed by the platform *)
    [ ( [ "macosx" ]
      , [ "-cclib"; "-framework CoreFoundation"; "-cclib"; "-framework CoreServices" ] )
    ; ( windows_system_values
      , [ "-cclib"; "-lshell32"; "-cclib"; "-lole32"; "-cclib"; "-luuid" ] )
    ; [ "beos" ], [ "-cclib"; "-lbsd" ] (* flags for Haiku *)
    ]
  in
  let+ locals =
    Memo.parallel_map locals ~f:(fun x ->
      let info = Lib.Local.info x in
      let dir = Lib_info.src_dir info in
      let special_builtin_support =
        match Lib_info.special_builtin_support info with
        | Some (_loc, Build_info { data_module; _ }) -> Some data_module
        | _ -> None
      in
      let open Memo.O in
      let+ is_multi_dir =
        let+ dc = Dir_contents.get sctx ~dir in
        match Dir_contents.dirs dc with
        | _ :: _ :: _ -> true
        | _ -> false
      in
      Dyn.Tuple
        [ Path.Build.drop_build_context_exn dir
          |> Path.Source.to_local
          |> Path.Local.to_dyn
        ; Dyn.option
            Module_name.to_dyn
            (match Lib_info.main_module_name info with
             | From _ -> None
             | This x -> x)
        ; Dyn.Bool is_multi_dir
        ; Dyn.option Module_name.to_dyn special_builtin_support
        ])
    |> Action_builder.of_memo
  in
  let externals =
    List.filter_map externals ~f:(fun lib ->
      let name = Lib.name lib in
      Option.some_if (Lib_name.equal name (Lib_name.of_string "threads.posix")) name)
  in
  Format.asprintf
    "%a@."
    Pp.to_fmt
    (Pp.concat
       ~sep:Pp.cut
       [ def "external_libraries" (Dyn.list Lib_name.to_dyn externals)
       ; Pp.nop
       ; def "local_libraries" (List locals)
       ; Pp.nop
       ; def "build_flags" (flags build_flags)
       ; Pp.nop
       ; def "link_flags" (flags link_flags)
       ]
     |> Pp.vbox)
;;

let gen_rules sctx (exes : Executables.t) ~dir ~requires_link =
  Memo.Option.iter exes.bootstrap_info ~f:(fun fname ->
    Action_builder.write_file_dyn
      (Path.Build.relative dir fname)
      (rule sctx ~requires_link)
    |> Super_context.add_rule sctx ~loc:exes.buildable.loc ~dir)
;;
