open! Import
module Pkg_dev_tool = Dune_rules.Pkg_dev_tool

module Exec = struct
  let term =
    let+ builder = Common.Builder.term
    and+ args = Arg.(value & pos_all string [] (info [] ~docv:"ARGS")) in
    let common, config = Common.init builder in
    Tools_common.lock_build_and_run_dev_tool ~common ~config Ocamlformat ~args
  ;;

  let info =
    let doc =
      {|Wrapper for running ocamlformat intended to be run automatically
       by a text editor. All positional arguments will be passed to the
       ocamlformat executable (pass flags to ocamlformat after the '--'
       argument, such as 'dune ocamlformat -- --help').|}
    in
    Cmd.info "ocamlformat" ~doc
  ;;

  let command = Cmd.v info term
end
