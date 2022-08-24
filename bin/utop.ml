open Stdune
open Import
module Utop = Dune_rules.Utop

let doc = "Load library in utop"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune utop DIR) build and run utop toplevel with libraries defined in DIR|}
  ; `Blocks Common.help_secs
  ]

let info = Term.info "utop" ~doc ~man

let term =
  let+ common = Common.term
  and+ dir = Arg.(value & pos 0 string "" & Arg.info [] ~docv:"DIR")
  and+ ctx_name =
    Common.context_arg ~doc:{|Select context where to build/run utop.|}
  and+ args = Arg.(value & pos_right 0 string [] (Arg.info [] ~docv:"ARGS")) in
  let config = Common.init common in
  let dir = Common.prefix_target common dir in
  if not (Path.is_directory (Path.of_string dir)) then
    User_error.raise
      [ Pp.textf "cannot find directory: %s" (String.maybe_quoted dir) ];
  let sctx, utop_path =
    Scheduler.go ~common ~config (fun () ->
        let open Fiber.O in
        let* setup = Import.Main.setup () in
        Build_system.run_exn (fun () ->
            let open Memo.O in
            let* setup = setup in
            let utop_target =
              let context = Import.Main.find_context_exn setup ~name:ctx_name in
              let utop_target = Filename.concat dir Utop.utop_exe in
              Path.build (Path.Build.relative context.build_dir utop_target)
            in
            Build_system.file_exists utop_target >>= function
            | false ->
              User_error.raise
                [ Pp.textf "no library is defined in %s"
                    (String.maybe_quoted dir)
                ]
            | true ->
              let+ () = Build_system.build_file utop_target in
              let sctx = Import.Main.find_scontext_exn setup ~name:ctx_name in
              (sctx, Path.to_string utop_target)))
  in
  Hooks.End_of_build.run ();
  restore_cwd_and_execve common utop_path (utop_path :: args)
    (Super_context.context_env sctx)

let command = (term, info)
