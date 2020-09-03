open! Dune_engine
open Import
open! No_io

let rule sctx compile (exes : Dune_file.Executables.t) () =
  let libs = Result.ok_exn (Lazy.force (Lib.Compile.requires_link compile)) in
  let locals, externals =
    List.partition_map libs ~f:(fun lib ->
        match Lib.Local.of_lib lib with
        | Some x -> Left x
        | None -> Right lib)
  in
  let open Pp.O in
  let def name dyn =
    Pp.box ~indent:2 (Pp.textf "let %s = " name ++ Dyn.pp dyn)
  in
  Format.asprintf "%a@." Pp.render_ignore_tags
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
          ; def "local_libraries"
              (List
                 (List.map locals ~f:(fun x ->
                      let info = Lib.Local.info x in
                      let dir = Lib_info.src_dir info in
                      let special_builton_support =
                        match Lib_info.special_builtin_support info with
                        | Some (Build_info { data_module; _ }) ->
                          Some data_module
                        | _ -> None
                      in
                      Dyn.Tuple
                        [ Path.Source.to_dyn
                            (Path.Build.drop_build_context_exn dir)
                        ; Dyn.Encoder.option Module_name.to_dyn
                            ( match Lib_info.main_module_name info with
                            | From _ -> None
                            | This x -> x )
                        ; ( match
                              Dir_contents.get sctx ~dir |> Dir_contents.dirs
                            with
                          | _ :: _ :: _ -> Dyn.Bool true
                          | _ -> Dyn.Bool false )
                        ; Dyn.Encoder.(option string) special_builton_support
                        ])))
          ]))

let gen_rules sctx (exes : Dune_file.Executables.t) ~dir compile =
  Option.iter exes.bootstrap_info ~f:(fun fname ->
      Super_context.add_rule sctx ~loc:exes.buildable.loc ~dir
        (Build.write_file_dyn
           (Path.Build.relative dir fname)
           (Build.delayed (rule sctx compile exes))))
