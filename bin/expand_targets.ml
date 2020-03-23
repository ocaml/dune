open! Stdune
open Import

let doc = "Expand dune targets to their path in the _build directory"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|Expands the given targets, one per line. It supports
        dune language variables such as $(b,%{ext:path}).|}
  ]

let info = Term.info "expand-targets" ~doc ~man

let pp_target fmt = function
  | Target.File path -> Path.pp fmt path
  | Alias alias -> Pp.render_ignore_tags fmt (Alias.pp alias)

let term =
  let+ common = Common.term
  and+ targets =
    let docv = "TARGET" in
    let doc = "Path to the .ml file to print the interface of" in
    Arg.(non_empty & pos_all dep [] & info [] ~docv ~doc)
  in
  Common.set_common common ~targets:[];
  let print_target () =
    let open Fiber.O in
    let+ setup = Import.Main.setup common in
    let targets = Target.resolve_targets_exn common setup targets in
    List.iter targets ~f:(Format.printf "%a\n" pp_target)
  in
  Scheduler.go ~common print_target

let command = (term, info)
