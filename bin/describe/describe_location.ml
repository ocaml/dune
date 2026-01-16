open Import

let doc =
  "Print the path to the executable using the same resolution logic as [dune exec]."
;;

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune describe location NAME) prints the path to the executable NAME using the same logic as:
          |}
  ; `Pre "$ dune exec NAME"
  ; `P
      "Dune will first try to resolve the executable within the public executables in \
       the current project, then inside the \"bin\" directory of each package among the \
       project's  dependencies (when using dune package management), and finally within \
       the  directories listed in the $PATH environment variable."
  ]
;;

let info = Cmd.info "location" ~doc ~man

let term : unit Term.t =
  let+ builder = Common.Builder.term
  and+ context = Common.context_arg ~doc:(Some {|Run the command in this build context.|})
  and+ prog =
    (* CR-someday Alizter: document this option *)
    Arg.(
      required & pos 0 (some Exec.Cmd_arg.conv) None (Arg.info [] ~docv:"PROG" ~doc:None))
  in
  let common, config = Common.init builder in
  Scheduler_setup.go_with_rpc_server ~common ~config
  @@ fun () ->
  let open Fiber.O in
  let* setup = Import.Main.setup () in
  build_exn
  @@ fun () ->
  let open Memo.O in
  let* sctx = setup >>| Import.Main.find_scontext_exn ~name:context in
  let* prog = Exec.Cmd_arg.expand ~root:(Common.root common) ~sctx prog in
  let+ path = Exec.get_path common sctx ~prog >>| Path.to_string in
  Dune_console.printf "%s" path
;;

let command = Cmd.v info term
