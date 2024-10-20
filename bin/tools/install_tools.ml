open! Import
module Pkg_dev_tool = Dune_rules.Pkg_dev_tool

let build_dev_tools dev_tools common =
  let open Fiber.O in
  let+ result =
    let dev_tools_path =
      List.map dev_tools ~f:(fun path -> Pkg_dev_tool.exe_path path |> Path.build)
    in
    Build_cmd.run_build_system ~common ~request:(fun _build_system ->
      List.map dev_tools_path ~f:Action_builder.path |> Action_builder.all_unit)
  in
  match result with
  | Error `Already_reported -> raise Dune_util.Report_error.Already_reported
  | Ok () -> ()
;;

let term =
  let+ builder = Common.Builder.term in
  let common, config = Common.init builder in
  let all_dev_tool = Dune_pkg.Dev_tool.all in
  Scheduler.go ~common ~config (fun () ->
    let open Fiber.O in
    if Lazy.force Lock_dev_tool.is_enabled
    then
      let* () =
        Dune_pkg.Dev_tool.all
        |> Lock_dev_tool.lock_tools
        |> List.map ~f:Memo.run
        |> Fiber.all_concurrently_unit
      in
      build_dev_tools all_dev_tool common
    else Fiber.return ())
;;

let info =
  let doc = "Install all the dev-tools that are available" in
  Cmd.info "install" ~doc
;;

let command = Cmd.v info term
