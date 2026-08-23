open Import

let doc = "Generate an inferred interface for an OCaml module."

let man =
  [ `S Cmdliner.Manpage.s_description
  ; `P
      {|Run the OCaml compiler to infer the interface of MODULE using its build
        configuration. The generated .mli is registered for promotion; run
        $(b,dune promote) to copy it into the source tree.|}
  ; `Blocks Common.help_secs
  ]
;;

let info = Cmd.info "inferred-mli" ~doc ~man

let term =
  let+ builder = Common.Builder.term
  and+ module_path =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"MODULE" ~doc:(Some "Path to an OCaml implementation."))
  and+ context_name = Common.context_arg ~doc:(Some "Build context to use.") in
  let common, config = Common.init builder in
  let source = Common.source_path common module_path in
  if
    not
      (Filename.Extension.Or_empty.check
         (Path.Source.extension source)
         Filename.Extension.ml)
  then User_error.raise [ Pp.text "Module path must have a .ml extension." ];
  Scheduler_setup.go_with_rpc_server ~common ~config (fun () ->
    Build.build_memo_exn (fun () ->
      let open Memo.O in
      let* sctx =
        let+ setup = Util.setup () in
        Dune_rules.Main.find_scontext_exn setup ~name:context_name
      in
      Dune_rules.Top_module.find_module sctx source
      >>= function
      | None ->
        let source = Path.Source.to_string_maybe_quoted source in
        User_error.raise [ Pp.textf "No module found for %s." source ]
      | Some (_, _, _, Melange _) ->
        User_error.raise
          [ Pp.text "Modules belonging to `melange.emit' are not supported." ]
      | Some (module_, cctx, _, _) ->
        if not (Dune_rules.Module.has module_ ~ml_kind:Impl)
        then (
          let name = Dune_rules.Module.name module_ |> Dune_lang.Module_name.to_string in
          User_error.raise [ Pp.textf "Module %s has no implementation." name ]);
        Dune_rules.Module_compilation.infer_interface cctx module_))
;;

let command = Cmd.v info term
