open Import

let term =
  let+ builder = Common.Builder.term
  and+ format = Describe_format.arg
  and+ _ = Describe_lang_compat.arg
  and+ filenames_only =
    let doc =
      "Print opam filenames, each on their own line. This ignores the --format argument."
    in
    Arg.(value & flag & info [ "files" ] ~doc:(Some doc))
  in
  let common, config = Common.init builder in
  Scheduler_setup.go_with_rpc_server ~common ~config
  @@ fun () ->
  Build_system.run_exn
  @@ fun () ->
  let open Memo.O in
  let* project = Source_tree.root () >>| Source_tree.Dir.project in
  let packages = Dune_project.packages project |> Package.Name.Map.values in
  let opam_file_to_dyn pkg =
    let opam_file = Path.source (Package.opam_file pkg) in
    let+ contents =
      if Dune_project.generate_opam_files project
      then
        let+ doc = Dune_rules.Packages.odoc_enabled pkg in
        let template_file = Dune_rules.Opam_create.template_file opam_file in
        let template =
          if Fpath.exists (Path.to_string template_file)
          then Some (template_file, Io.read_file_exn template_file)
          else None
        in
        Dune_rules.Opam_create.generate project pkg ~doc ~template
      else Memo.return (Io.read_file_exn opam_file)
    in
    Dyn.Tuple [ String (Path.to_string opam_file); String contents ]
  in
  if filenames_only
  then
    Memo.return
      (Console.print
         [ Pp.vbox
             (Pp.concat_map ~sep:Pp.cut packages ~f:(fun pkg ->
                Package.opam_file pkg |> Path.source |> Path.pp))
         ])
  else
    let+ opam_files = Memo.List.map packages ~f:opam_file_to_dyn in
    Describe_format.print_dyn format (Dyn.list Fun.id opam_files)
;;

let command =
  let doc = "Print information about the opam files that have been discovered." in
  let info = Cmd.info ~doc "opam-files" in
  Cmd.v info term
;;
