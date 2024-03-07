open Import

let term =
  let+ builder = Common.Builder.term
  and+ format = Describe_format.arg
  and+ _ = Describe_lang_compat.arg in
  let common, config = Common.init builder in
  Scheduler.go ~common ~config
  @@ fun () ->
  Build_system.run_exn
  @@ fun () ->
  let open Memo.O in
  let+ project = Source_tree.root () >>| Source_tree.Dir.project in
  let packages = Dune_project.packages project |> Package.Name.Map.values in
  let opam_file_to_dyn pkg =
    let opam_file = Path.source (Package.opam_file pkg) in
    let contents =
      if Dune_project.generate_opam_files project
      then (
        let template_file = Dune_rules.Opam_create.template_file opam_file in
        let template =
          if Path.exists template_file
          then Some (template_file, Io.read_file template_file)
          else None
        in
        Dune_rules.Opam_create.generate project pkg ~template)
      else Io.read_file opam_file
    in
    Dyn.Tuple [ String (Path.to_string opam_file); String contents ]
  in
  packages |> Dyn.list opam_file_to_dyn |> Describe_format.print_dyn format
;;

let command =
  let doc = "Print information about the opam files that have been discovered." in
  let info = Cmd.info ~doc "opam-files" in
  Cmd.v info term
;;
