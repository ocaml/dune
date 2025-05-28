open! Import
module Pkg_dev_tool = Dune_rules.Pkg_dev_tool

let ocamllsp_exe_path = Path.build @@ Pkg_dev_tool.exe_path Ocamllsp
let ocamllsp_exe_name = Pkg_dev_tool.exe_name Ocamllsp

let is_in_dune_project builder =
  Workspace_root.create
    ~default_is_cwd:(Common.Builder.default_root_is_cwd builder)
    ~specified_by_user:(Common.Builder.root builder)
  |> Result.is_ok
;;

module Exec = struct
  let term =
    let+ builder = Common.Builder.term
    and+ args = Arg.(value & pos_all string [] (info [] ~docv:"ARGS")) in
    match is_in_dune_project builder with
    | false ->
      User_error.raise
        [ Pp.textf
            "Unable to run %s as a dev-tool because you don't appear to be inside a dune \
             project."
            ocamllsp_exe_name
        ]
    | true ->
      let common, config = Common.init builder in
      Tools_common.lock_build_and_run_dev_tool ~common ~config Ocamllsp ~args
  ;;

  let info =
    let doc = "Run ocamllsp, installing it as a dev tool if necessary." in
    Cmd.info "ocamllsp" ~doc
  ;;

  let command = Cmd.v info term
end

module Which = struct
  let term =
    let+ builder = Common.Builder.term in
    let _ : Common.t * Dune_config_file.Dune_config.t = Common.init builder in
    print_endline (Path.to_string ocamllsp_exe_path)
  ;;

  let info =
    let doc = "Prints the path to the ocamllsp binary." in
    Cmd.info "ocamllsp" ~doc
  ;;

  let command = Cmd.v info term
end
