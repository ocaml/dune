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
      ; Predicate_lang.encode Dune_sexp.Encoder.int pred
      ; encode t
      ]
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
  | Bash x -> List [ atom "bash"; string x ]
  | Write_file (x, perm, y) ->
    List [ atom ("write-file" ^ File_perm.suffix perm); target x; string y ]
  | Rename (x, y) -> List [ atom "rename"; target x; target y ]
  | Remove_tree x -> List [ atom "remove-tree"; target x ]
  | Mkdir x -> List [ atom "mkdir"; target x ]
  | Pipe (outputs, l) ->
    List (atom (sprintf "pipe-%s" (Outputs.to_string outputs)) :: List.map l ~f:encode)
  | Extension ext -> List [ atom "ext"; Dune_sexp.Quoted_string (Sexp.to_string ext) ]
;;

let encode_path p =
  let make constr arg =
    Dune_sexp.List [ Dune_sexp.atom constr; Dune_sexp.atom_or_quoted_string arg ]
  in
  let open Path in
  match p with
  | In_build_dir p -> make "In_build_dir" (Path.Build.to_string p)
  | In_source_tree p -> make "In_source_tree" (Path.Source.to_string p)
  | External p -> make "External" (Path.External.to_string p)
;;

let encode_file_selector file_selector =
  let open Dune_sexp.Encoder in
  let module File_selector = Dune_engine.File_selector in
  let dir = File_selector.dir file_selector in
  let predicate = File_selector.predicate file_selector in
  let only_generated_files = File_selector.only_generated_files file_selector in
  record
    [ "dir", encode_path dir
    ; "predicate", Predicate_lang.Glob.encode predicate
    ; "only_generated_files", bool only_generated_files
    ]
;;

let encode_alias alias =
  let open Dune_sexp.Encoder in
  let dir = Dune_engine.Alias.dir alias in
  let name = Dune_engine.Alias.name alias in
  record
    [ "dir", encode_path (Path.build dir)
    ; "name", Dune_sexp.atom_or_quoted_string (Dune_util.Alias_name.to_string name)
    ]
;;

let encode_dep_set deps =
  Dune_sexp.List
    (Dep.Set.to_list_map
       deps
       ~f:
         (let open Dune_sexp.Encoder in
          function
          | File_selector g -> pair string encode_file_selector ("glob", g)
          | Env e -> pair string string ("Env", e)
          | File f -> pair string encode_path ("File", f)
          | Alias a -> pair string encode_alias ("Alias", a)
          | Universe -> string "Universe"))
;;

let print_rule_sexp ppf (rule : Dune_engine.Reflection.Rule.t) =
  let sexp_of_action action = Action.for_shell action |> encode in
  let paths ps =
    Dune_sexp.Encoder.list
      (fun p ->
         Path.Build.relative rule.targets.root p
         |> Path.Build.to_string
         |> Dune_sexp.atom_or_quoted_string)
      (Filename.Set.to_list ps)
  in
  let sexp =
    Dune_sexp.Encoder.record
      (List.concat
         [ [ "deps", encode_dep_set rule.deps
           ; ( "targets"
             , Dune_sexp.Encoder.record
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
  Format.fprintf ppf "%a@," Pp.to_fmt (Dune_lang.pp sexp)
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

  type formatter_state =
    | In_atom
    | In_makefile_action
    | In_makefile_stuff

  let prepare_formatter ppf =
    let state = ref [] in
    Format.pp_set_mark_tags ppf true;
    let ofuncs = Format.pp_get_formatter_out_functions ppf () in
    let tfuncs = Format.pp_get_formatter_stag_functions ppf () in
    Format.pp_set_formatter_stag_functions
      ppf
      { tfuncs with
        mark_open_stag =
          (function
            | Format.String_tag "atom" ->
              state := In_atom :: !state;
              ""
            | Format.String_tag "makefile-action" ->
              state := In_makefile_action :: !state;
              ""
            | Format.String_tag "makefile-stuff" ->
              state := In_makefile_stuff :: !state;
              ""
            | s -> tfuncs.mark_open_stag s)
      ; mark_close_stag =
          (function
            | Format.String_tag "atom"
            | Format.String_tag "makefile-action"
            | Format.String_tag "makefile-stuff" ->
              state := List.tl !state;
              ""
            | s -> tfuncs.mark_close_stag s)
      };
    Format.pp_set_formatter_out_functions
      ppf
      { ofuncs with
        out_newline =
          (fun () ->
            match !state with
            | [ In_atom; In_makefile_action ] -> ofuncs.out_string "\\\n\t" 0 3
            | [ In_atom ] -> ofuncs.out_string "\\\n" 0 2
            | [ In_makefile_action ] -> ofuncs.out_string " \\\n\t" 0 4
            | [ In_makefile_stuff ] -> ofuncs.out_string " \\\n" 0 3
            | [] -> ofuncs.out_string "\n" 0 1
            | _ -> assert false)
      ; out_spaces =
          (fun n ->
            ofuncs.out_spaces
              (match !state with
               | In_atom :: _ -> max 0 (n - 2)
               | _ -> n))
      }
  ;;

  let print_rules syntax ppf rules =
    prepare_formatter ppf;
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
  Scheduler.go_with_rpc_server ~common ~config (fun () ->
    let open Fiber.O in
    let* setup = Import.Main.setup () in
    build_exn (fun () ->
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
