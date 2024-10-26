open Import

let doc = "Format source code."

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune fmt) runs the formatter on the source code. The formatter is
        automatically selected. ocamlformat is used to format OCaml source code
        (*.ml and *.mli files) and refmt is used to format Reason source code
        (*.re and *.rei files).|}
  ; `Blocks Common.help_secs
  ]
;;

let command =
  let term =
    let+ builder = Common.Builder.term
    and+ no_promote =
      Arg.(
        value
        & flag
        & info
            [ "preview" ]
            ~doc:
              "Just print the changes that would be made without actually applying them. \
               This takes precedence over auto-promote as that flag is assumed for this \
               command.")
    in
    let builder =
      Common.Builder.set_promote builder (if no_promote then Never else Automatically)
    in
    let common, config = Common.init builder in
    let request (setup : Import.Main.build_system) =
      let open Action_builder.O in
      let* () =
        if Lazy.force Lock_dev_tool.is_enabled
        then
          (* Note that generating the ocamlformat lockdir here means
             that it will be created when a user runs `dune fmt` but not
             when a user runs `dune build @fmt`. It's important that
             this logic remain outside of `dune build`, as `dune
             build` is intended to only build targets, and generating
             a lockdir is not building a target. *)
          Action_builder.of_memo (Lock_dev_tool.lock_ocamlformat ())
        else Action_builder.return ()
      in
      let dir = Path.(relative root) (Common.prefix_target common ".") in
      Alias.in_dir ~name:Dune_rules.Alias.fmt ~recursive:true ~contexts:setup.contexts dir
      |> Alias.request
    in
    Build_cmd.run_build_command ~common ~config ~request
  in
  Cmd.v (Cmd.info "fmt" ~doc ~man ~envs:Common.envs) term
;;
