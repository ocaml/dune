open Import
open Stdune

let term =
  let+ common = Common.term
  and+ format = Describe_common.Format.arg in
  let config = Common.init common in
  Scheduler.go ~common ~config @@ fun () ->
  let open Fiber.O in
  let+ res =
    Build_system.run_exn @@ fun () ->
    let open Memo.O in
    let+ project = Source_tree.root () >>| Source_tree.Dir.project in
    let packages = Dune_project.packages project |> Package.Name.Map.values in
    Dyn.List
      (List.map packages ~f:(fun pkg ->
           let opam_file = Path.source (Package.opam_file pkg) in
           let contents =
             if not (Dune_project.generate_opam_files project) then
               Io.read_file opam_file
             else
               let template_file =
                 Dune_rules.Opam_create.template_file opam_file
               in
               let template =
                 if Path.exists template_file then
                   Some (template_file, Io.read_file template_file)
                 else None
               in
               Dune_rules.Opam_create.generate project pkg ~template
           in
           Dyn.Tuple [ String (Path.to_string opam_file); String contents ]))
  in
  Describe_common.Format.print_dyn format res

let command =
  let doc =
    "Print information about the opam files that have been discovered."
  in
  let info = Cmd.info ~doc "opam-files" in
  Cmd.v info term
