open Import
open Memo.O

let def name dyn =
  let open Pp.O in
  Pp.box ~indent:2 (Pp.textf "let %s = " name ++ Dyn.pp dyn)

let rule sctx compile (exes : Dune_file.Executables.t) () =
  let* locals, externals =
    let+ libs =
      Resolve.Memo.read_memo
        (Memo.Lazy.force (Lib.Compile.requires_link compile))
    in
    List.partition_map libs ~f:(fun lib ->
        match Lib.Local.of_lib lib with
        | Some x -> Left x
        | None -> Right lib)
  in
  let link_flags =
    let win_link_flags =
      [ "-cclib"; "-lshell32"; "-cclib"; "-lole32"; "-cclib"; "-luuid" ]
    in
    (* additional link flags keyed by the platform *)
    [ ( "macosx"
      , [ "-cclib"
        ; "-framework Foundation"
        ; "-cclib"
        ; "-framework CoreServices"
        ] )
    ; ("win32", win_link_flags)
    ; ("win64", win_link_flags)
    ; ("mingw", win_link_flags)
    ; ("mingw64", win_link_flags)
    ]
  in
  let+ locals =
    Memo.parallel_map locals ~f:(fun x ->
        let info = Lib.Local.info x in
        let dir = Lib_info.src_dir info in
        let special_builtin_support =
          match Lib_info.special_builtin_support info with
          | Some (Build_info { data_module; _ }) -> Some data_module
          | _ -> None
        in
        let+ is_multi_dir =
          let+ dc = Dir_contents.get sctx ~dir in
          match Dir_contents.dirs dc with
          | _ :: _ :: _ -> true
          | _ -> false
        in
        Dyn.Tuple
          [ Path.Build.drop_build_context_exn dir
            |> Path.Source.to_local |> Path.Local.to_dyn
          ; Dyn.option Module_name.to_dyn
              (match Lib_info.main_module_name info with
              | From _ -> None
              | This x -> x)
          ; Dyn.Bool is_multi_dir
          ; Dyn.option Module_name.to_dyn special_builtin_support
          ])
  in
  Format.asprintf "%a@." Pp.to_fmt
    (Pp.vbox
       (Pp.concat ~sep:Pp.cut
          [ def "executables"
              (List
                 (* @@DRA Want to be using the public_name here, not the
                    internal name *)
                 (List.map ~f:(fun (_, x) -> Dyn.String x) exes.names))
          ; Pp.nop
          ; def "external_libraries"
              (List
                 (List.map externals ~f:(fun x -> Lib.name x |> Lib_name.to_dyn)))
          ; Pp.nop
          ; def "local_libraries" (List locals)
          ; Pp.nop
          ; def "link_flags"
              (let open Dyn in
              list (pair string (list string)) link_flags)
          ]))

let gen_rules sctx (exes : Dune_file.Executables.t) ~dir compile =
  Memo.Option.iter exes.bootstrap_info ~f:(fun fname ->
      Super_context.add_rule sctx ~loc:exes.buildable.loc ~dir
        (Action_builder.write_file_dyn
           (Path.Build.relative dir fname)
           (Action_builder.of_memo (Memo.return () >>= rule sctx compile exes))))
