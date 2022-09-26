open Stdune
open Import

let doc = "Dump internal rules."

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|Dump Dune internal rules for the given targets.
           If no targets are given, dump all the internal rules.|}
  ; `P
      {|By default the output is a list of S-expressions,
           one S-expression per rule. Each S-expression is of the form:|}
  ; `Pre
      "  ((deps    (<dependencies>))\n\
      \   (targets (<targets>))\n\
      \   (context <context-name>)\n\
      \   (action  <action>))"
  ; `P
      {|$(b,<context-name>) is the context is which the action is executed.
           It is omitted if the action is independent from the context.|}
  ; `P
      {|$(b,<action>) is the action following the same syntax as user actions,
           as described in the manual.|}
  ; `Blocks Common.help_secs
  ]

let info = Cmd.info "rules" ~doc ~man

let print_rule_makefile ppf (rule : Dune_engine.Reflection.Rule.t) =
  let action =
    Action.For_shell.Progn
      [ Mkdir (Path.to_string (Path.build rule.dir))
      ; Action.for_shell rule.action
      ]
  in
  (* Makefiles seem to allow directory targets, so we include them. *)
  let targets = Path.Build.Set.union rule.targets.files rule.targets.dirs in
  Format.fprintf ppf
    "@[<hov 2>@{<makefile-stuff>%a:%t@}@]@,@<0>\t@{<makefile-action>%a@}\n"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf p ->
         Format.pp_print_string ppf (Path.to_string p)))
    (List.map ~f:Path.build (Path.Build.Set.to_list targets))
    (fun ppf ->
      Path.Set.iter rule.expanded_deps ~f:(fun dep ->
          Format.fprintf ppf "@ %s" (Path.to_string dep)))
    Pp.to_fmt (Action_to_sh.pp action)

let print_rule_sexp ppf (rule : Dune_engine.Reflection.Rule.t) =
  let sexp_of_action action =
    Action.for_shell action |> Action.For_shell.encode
  in
  let paths ps =
    Dune_lang.Encoder.list Dpath.Build.encode (Path.Build.Set.to_list ps)
  in
  let sexp =
    Dune_lang.Encoder.record
      (List.concat
         [ [ ("deps", Dep.Set.encode rule.deps)
           ; ( "targets"
             , Dune_lang.Encoder.record
                 [ ("files", paths rule.targets.files)
                 ; ("directories", paths rule.targets.dirs)
                 ] )
           ]
         ; (match rule.context with
           | None -> []
           | Some c -> [ ("context", Dune_engine.Context_name.encode c.name) ])
         ; [ ("action", sexp_of_action rule.action) ]
         ])
  in
  Format.fprintf ppf "%a@," Dune_lang.Deprecated.pp_split_strings sexp

module Syntax = struct
  type t =
    | Makefile
    | Sexp

  let term =
    let doc = "Output the rules in Makefile syntax." in
    let+ makefile = Arg.(value & flag & info [ "m"; "makefile" ] ~doc) in
    if makefile then Makefile else Sexp

  let print_rule = function
    | Makefile -> print_rule_makefile
    | Sexp -> print_rule_sexp

  let print_rules syntax ppf rules =
    Dune_lang.Deprecated.prepare_formatter ppf;
    Format.pp_open_vbox ppf 0;
    Format.pp_print_list (print_rule syntax) ppf rules;
    Format.pp_print_flush ppf ()
end

let term =
  let+ common = Common.term
  and+ out =
    Arg.(
      value
      & opt (some string) None
      & info [ "o" ] ~docv:"FILE" ~doc:"Output to a file instead of stdout.")
  and+ recursive =
    Arg.(
      value & flag
      & info [ "r"; "recursive" ]
          ~doc:
            "Print all rules needed to build the transitive dependencies of \
             the given targets.")
  and+ syntax = Syntax.term
  and+ targets = Arg.(value & pos_all dep [] & Arg.info [] ~docv:"TARGET") in
  let config = Common.init common in
  let out = Option.map ~f:Path.of_string out in
  Scheduler.go ~common ~config (fun () ->
      let open Fiber.O in
      let* setup = Import.Main.setup () in
      Build_system.run_exn (fun () ->
          let open Memo.O in
          let* setup = setup in
          let* request =
            match targets with
            | [] ->
              Load_rules.all_direct_targets ()
              >>| Path.Build.Map.foldi ~init:[] ~f:(fun p _ acc ->
                      Path.build p :: acc)
              >>| Action_builder.paths
            | _ ->
              Memo.return
                (Target.interpret_targets (Common.root common) config setup
                   targets)
          in
          let+ rules = Dune_engine.Reflection.eval ~request ~recursive in
          let print oc =
            let ppf = Format.formatter_of_out_channel oc in
            Syntax.print_rules syntax ppf rules
          in
          match out with
          | None -> print stdout
          | Some fn -> Io.with_file_out fn ~f:print))

let command = Cmd.v info term
