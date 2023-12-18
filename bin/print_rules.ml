open Import

let doc = "Dump rules."

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|Dump Dune rules for the given targets.
           If no targets are given, dump all the rules.|}
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
;;

let info = Cmd.info "rules" ~doc ~man

let print_rule_makefile ppf (rule : Dune_engine.Reflection.Rule.t) =
  let action =
    Action.For_shell.Progn
      [ Mkdir (Path.to_string (Path.build rule.targets.root))
      ; Action.for_shell rule.action
      ]
  in
  (* Makefiles seem to allow directory targets, so we include them. *)
  let targets =
    Filename.Set.union rule.targets.files rule.targets.dirs
    |> Filename.Set.to_list_map ~f:(fun basename ->
      Path.Build.relative rule.targets.root basename |> Path.build)
  in
  Format.fprintf
    ppf
    "@[<hov 2>@{<makefile-stuff>%a:%t@}@]@,@<0>\t@{<makefile-action>%a@}\n"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf p ->
       Format.pp_print_string ppf (Path.to_string p)))
    targets
    (fun ppf ->
      Path.Set.iter rule.expanded_deps ~f:(fun dep ->
        Format.fprintf ppf "@ %s" (Path.to_string dep)))
    Pp.to_fmt
    (Action_to_sh.pp action)
;;

let rec encode : Action.For_shell.t -> Dune_lang.t =
  let module Outputs = Dune_lang.Action.Outputs in
  let module File_perm = Dune_lang.Action.File_perm in
  let module Inputs = Dune_lang.Action.Inputs in
  let open Dune_lang in
  let program = Encoder.string in
  let string = Encoder.string in
  let path = Encoder.string in
  let target = Encoder.string in
  function
  | Run (a, xs) ->
    List (atom "run" :: program a :: List.map (Array.Immutable.to_list xs) ~f:string)
  | With_accepted_exit_codes (pred, t) ->
    List
      [ atom "with-accepted-exit-codes"
      ; Predicate_lang.encode Dune_lang.Encoder.int pred
      ; encode t
      ]
  | Dynamic_run (a, xs) -> List (atom "run_dynamic" :: program a :: List.map xs ~f:string)
  | Chdir (a, r) -> List [ atom "chdir"; path a; encode r ]
  | Setenv (k, v, r) -> List [ atom "setenv"; string k; string v; encode r ]
  | Redirect_out (outputs, fn, perm, r) ->
    List
      [ atom (sprintf "with-%s-to%s" (Outputs.to_string outputs) (File_perm.suffix perm))
      ; target fn
      ; encode r
      ]
  | Redirect_in (inputs, fn, r) ->
    List [ atom (sprintf "with-%s-from" (Inputs.to_string inputs)); path fn; encode r ]
  | Ignore (outputs, r) ->
    List [ atom (sprintf "ignore-%s" (Outputs.to_string outputs)); encode r ]
  | Progn l -> List (atom "progn" :: List.map l ~f:encode)
  | Concurrent l -> List (atom "concurrent" :: List.map l ~f:encode)
  | Echo xs -> List (atom "echo" :: List.map xs ~f:string)
  | Cat xs -> List (atom "cat" :: List.map xs ~f:path)
  | Copy (x, y) -> List [ atom "copy"; path x; target y ]
  | Symlink (x, y) -> List [ atom "symlink"; path x; target y ]
  | Hardlink (x, y) -> List [ atom "hardlink"; path x; target y ]
  | System x -> List [ atom "system"; string x ]
  | Bash x -> List [ atom "bash"; string x ]
  | Write_file (x, perm, y) ->
    List [ atom ("write-file" ^ File_perm.suffix perm); target x; string y ]
  | Rename (x, y) -> List [ atom "rename"; target x; target y ]
  | Remove_tree x -> List [ atom "remove-tree"; target x ]
  | Mkdir x -> List [ atom "mkdir"; target x ]
  | Diff { optional; file1; file2; mode = Binary } ->
    assert (not optional);
    List [ atom "cmp"; path file1; target file2 ]
  | Diff { optional = false; file1; file2; mode = _ } ->
    List [ atom "diff"; path file1; target file2 ]
  | Diff { optional = true; file1; file2; mode = _ } ->
    List [ atom "diff?"; path file1; target file2 ]
  | Merge_files_into (srcs, extras, into) ->
    List
      [ atom "merge-files-into"
      ; List (List.map ~f:path srcs)
      ; List (List.map ~f:string extras)
      ; target into
      ]
  | Pipe (outputs, l) ->
    List (atom (sprintf "pipe-%s" (Outputs.to_string outputs)) :: List.map l ~f:encode)
  | Extension ext -> List [ atom "ext"; ext ]
;;

let print_rule_sexp ppf (rule : Dune_engine.Reflection.Rule.t) =
  let sexp_of_action action = Action.for_shell action |> encode in
  let paths ps =
    Dune_lang.Encoder.list
      (fun p ->
        Path.Build.relative rule.targets.root p
        |> Path.Build.to_string
        |> Dune_sexp.atom_or_quoted_string)
      (Filename.Set.to_list ps)
  in
  let sexp =
    Dune_lang.Encoder.record
      (List.concat
         [ [ "deps", Dep.Set.encode rule.deps
           ; ( "targets"
             , Dune_lang.Encoder.record
                 [ "files", paths rule.targets.files
                 ; "directories", paths rule.targets.dirs
                 ] )
           ]
         ; (match Path.Build.extract_build_context rule.targets.root with
            | None -> []
            | Some (c, _) -> [ "context", Dune_sexp.atom_or_quoted_string c ])
         ; [ "action", sexp_of_action rule.action ]
         ])
  in
  Format.fprintf ppf "%a@," Dune_lang.Deprecated.pp_split_strings sexp
;;

module Syntax = struct
  type t =
    | Makefile
    | Sexp

  let term =
    let doc = "Output the rules in Makefile syntax." in
    let+ makefile = Arg.(value & flag & info [ "m"; "makefile" ] ~doc) in
    if makefile then Makefile else Sexp
  ;;

  let print_rule = function
    | Makefile -> print_rule_makefile
    | Sexp -> print_rule_sexp
  ;;

  let print_rules syntax ppf rules =
    Dune_lang.Deprecated.prepare_formatter ppf;
    Format.pp_open_vbox ppf 0;
    Format.pp_print_list (print_rule syntax) ppf rules;
    Format.pp_print_flush ppf ()
  ;;
end

let term =
  let+ builder = Common.Builder.term
  and+ out =
    Arg.(
      value
      & opt (some string) None
      & info [ "o" ] ~docv:"FILE" ~doc:"Output to a file instead of stdout.")
  and+ recursive =
    Arg.(
      value
      & flag
      & info
          [ "r"; "recursive" ]
          ~doc:
            "Print all rules needed to build the transitive dependencies of the given \
             targets.")
  and+ syntax = Syntax.term
  and+ targets = Arg.(value & pos_all dep [] & Arg.info [] ~docv:"TARGET") in
  let common, config = Common.init builder in
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
          Target.all_direct_targets None
          >>| Path.Build.Map.foldi ~init:[] ~f:(fun p _ acc -> Path.build p :: acc)
          >>| Action_builder.paths
        | _ ->
          Memo.return (Target.interpret_targets (Common.root common) config setup targets)
      in
      let+ rules = Dune_engine.Reflection.eval ~request ~recursive in
      let print oc =
        let ppf = Format.formatter_of_out_channel oc in
        Syntax.print_rules syntax ppf rules
      in
      match out with
      | None -> print stdout
      | Some fn -> Io.with_file_out fn ~f:print))
;;

let command = Cmd.v info term
