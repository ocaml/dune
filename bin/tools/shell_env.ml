open! Import
module Pkg_dev_tool = Dune_rules.Pkg_dev_tool

let all_tools = List.map ~f:Pkg_dev_tool.exe_name [ Ocamlformat; Ocamllsp ]
let bin_path () = Path.build (Pkg_dev_tool.bin_path ())

let shell_script = {|#!/bin/sh
dune tools exec $(basename $0) -- "$@"
|}

let setup path =
  if not (Path.exists path) then Path.mkdir_p path;
  List.iter all_tools ~f:(fun tool ->
    let tool_path = Path.relative path tool in
    if not (Path.exists tool_path) then Io.write_file ~perm:0o777 tool_path shell_script)
;;

let add_path env =
  let dir = bin_path () in
  setup dir;
  Env_path.cons env ~dir
;;

let term =
  let+ builder = Common.Builder.term in
  let common, config = Common.init builder in
  Scheduler.go ~common ~config (fun () ->
    let env = add_path Env.initial in
    Format.printf
      "%s=%s@."
      Env_path.var
      (Option.value ~default:"" (Env.get env Env_path.var));
    Fiber.return ())
;;

let info =
  let doc = "Configure shell environment" in
  Cmd.info "env" ~doc
;;

let command = Cmd.v info term
