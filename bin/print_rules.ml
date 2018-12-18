open Stdune
open Import
open Fiber.O

let doc = "Dump internal rules."

let man =
  [ `S "DESCRIPTION"
  ; `P {|Dump Dune internal rules for the given targets.
           If no targets are given, dump all the internal rules.|}
  ; `P {|By default the output is a list of S-expressions,
           one S-expression per rule. Each S-expression is of the form:|}
  ; `Pre "  ((deps    (<dependencies>))\n\
         \   (targets (<targets>))\n\
         \   (context <context-name>)\n\
         \   (action  <action>))"
  ; `P {|$(b,<context-name>) is the context is which the action is executed.
           It is omitted if the action is independent from the context.|}
  ; `P {|$(b,<action>) is the action following the same syntax as user actions,
           as described in the manual.|}
  ; `Blocks Common.help_secs
  ]

let info = Term.info "rules" ~doc ~man

let term =
  let%map common = Common.term
  and out =
    Arg.(value
         & opt (some string) None
         & info ["o"] ~docv:"FILE"
             ~doc:"Output to a file instead of stdout.")
  and recursive =
    Arg.(value
         & flag
         & info ["r"; "recursive"]
             ~doc:"Print all rules needed to build the transitive \
                   dependencies of the given targets.")
  and makefile_syntax =
    Arg.(value
         & flag
         & info ["m"; "makefile"]
             ~doc:"Output the rules in Makefile syntax.")
  and targets =
    Arg.(value
         & pos_all string []
         & Arg.info [] ~docv:"TARGET")
  in
  let out = Option.map ~f:Path.of_string out in
  Common.set_common common ~targets;
  let log = Log.create common in
  Scheduler.go ~log ~common (fun () ->
    Import.Main.setup ~log common ~external_lib_deps_mode:true
    >>= fun setup ->
    let request =
      match targets with
      | [] -> Build.paths (Build_system.all_targets setup.build_system)
      | _  ->
        Target.resolve_targets_exn ~log common setup targets
        |> Target.request setup
    in
    Build_system.build_rules setup.build_system ~request ~recursive >>= fun rules ->
    let sexp_of_action action =
      Action.for_shell action |> Action.For_shell.encode
    in
    let print oc =
      let ppf = Format.formatter_of_out_channel oc in
      Dune_lang.prepare_formatter ppf;
      Format.pp_open_vbox ppf 0;
      if makefile_syntax then begin
        List.iter rules ~f:(fun (rule : Build_system.Rule.t) ->
          let action =
            Action.For_shell.Progn
              [ Mkdir (Path.to_string rule.dir)
              ; Action.for_shell rule.action
              ]
          in
          Format.fprintf ppf
            "@[<hov 2>@{<makefile-stuff>%a:%t@}@]@,\
             @<0>\t@{<makefile-action>%a@}@,@,"
            (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf p ->
               Format.pp_print_string ppf (Path.to_string p)))
            (Path.Set.to_list rule.targets)
            (fun ppf ->
               Path.Set.iter (Deps.paths rule.deps) ~f:(fun dep ->
                 Format.fprintf ppf "@ %s" (Path.to_string dep)))
            Pp.pp
            (Action_to_sh.pp action))
      end else begin
        List.iter rules ~f:(fun (rule : Build_system.Rule.t) ->
          let sexp =
            let paths ps =
              Dune_lang.Encoder.list Path_dune_lang.encode (Path.Set.to_list ps)
            in
            Dune_lang.Encoder.record (
              List.concat
                [ [ "deps"   , Deps.to_sexp rule.deps
                  ; "targets", paths rule.targets ]
                ; (match rule.context with
                   | None -> []
                   | Some c -> ["context",
                                Dune_lang.atom_or_quoted_string c.name])
                ; [ "action" , sexp_of_action rule.action ]
                ])
          in
          Format.fprintf ppf "%a@," Dune_lang.pp_split_strings sexp)
      end;
      Format.pp_print_flush ppf ();
      Fiber.return ()
    in
    match out with
    | None -> print stdout
    | Some fn -> Io.with_file_out fn ~f:print)

let command = term, info
