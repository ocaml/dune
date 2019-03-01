open Stdune
open Import
open Fiber.O

module Utop = Dune.Utop

let doc = "Load library in utop"

let man =
  [ `S "DESCRIPTION"
  ; `P {|$(b,dune utop DIR) build and run utop toplevel with libraries defined in DIR|}
  ; `Blocks Common.help_secs
  ]

 let info = Term.info "utop" ~doc ~man

let term =
  let+ common = Common.term
  and+ dir = Arg.(value & pos 0 string "" & Arg.info [] ~docv:"DIR")
  and+ ctx_name =
    Common.context_arg ~doc:{|Select context where to build/run utop.|}
  and+ args = Arg.(value & pos_right 0 string [] (Arg.info [] ~docv:"ARGS"))
  in
  Common.set_dirs common;
  if not (Path.is_directory
            (Path.of_string (Common.prefix_target common dir))) then
    die "cannot find directory: %s" (String.maybe_quoted dir);
  let utop_target = Filename.concat dir Utop.utop_exe in
  Common.set_common_other common ~targets:[utop_target];
  let log = Log.create common in
  let (context, utop_path) =
    Scheduler.go ~log ~common (fun () ->
      Import.Main.setup ~log common >>= fun setup ->
      let context =
        Import.Main.find_context_exn setup.workspace ~name:ctx_name
      in
      let setup =
        { setup with
          workspace = { setup.workspace with contexts = [context] }
        }
      in
      let target =
        match Target.resolve_target common ~setup utop_target with
        | Error _ ->
          die "no library is defined in %s" (String.maybe_quoted dir)
        | Ok [File target] -> target
        | Ok _ -> assert false
      in
      do_build setup [File target] >>| fun () ->
      (context, Path.to_string target))
  in
  Hooks.End_of_build.run ();
  restore_cwd_and_execve common utop_path (utop_path :: args)
    context.env

let command = term, info
