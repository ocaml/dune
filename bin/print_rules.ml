open Import

let doc = "Dump rules."

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|Dump Dune rules for the given targets.
           If no targets are given, dump all the rules.|}
  ; `P {|With $(b,--deps), only print the dependencies of the matching rules.|}
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
  ; `P {|Use $(b,--format=json) to dump the same data as JSON.|}
  ; `Blocks Common.help_secs
  ]
;;

let info = Cmd.info "rules" ~doc ~man

module Output_format = struct
  type t =
    | Sexp
    | Json

  let all = [ "sexp", Sexp; "json", Json ]
end

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

module Syntax = struct
  type formatter_state = In_atom

  (* CR-someday rgrinberg: what is all of this? consider deleting *)
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
            | s -> tfuncs.mark_open_stag s)
      ; mark_close_stag =
          (function
            | Format.String_tag "atom" ->
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
            | [ In_atom ] -> ofuncs.out_string "\\\n" 0 2
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
end

let syntax_repr = Repr.abstract Dune_sexp.to_dyn
let path_repr = Repr.view syntax_repr ~to_:encode_path
let predicate_repr = Repr.view syntax_repr ~to_:Predicate_lang.Glob.encode

let alias_repr =
  Repr.record
    "alias"
    [ Repr.field "dir" path_repr ~get:(fun alias ->
        Path.build (Dune_engine.Alias.dir alias))
    ; Repr.field "name" Repr.string ~get:(fun alias ->
        Alias_name.to_string (Dune_engine.Alias.name alias))
    ]
;;

let file_selector_repr =
  Repr.record
    "file-selector"
    [ Repr.field "dir" path_repr ~get:Dune_engine.File_selector.dir
    ; Repr.field "predicate" predicate_repr ~get:Dune_engine.File_selector.predicate
    ; Repr.field
        "only_generated_files"
        Repr.bool
        ~get:Dune_engine.File_selector.only_generated_files
    ]
;;

let dep_repr =
  Repr.variant
    "dep"
    [ Repr.case "glob" file_selector_repr ~proj:(function
        | Dep.File_selector selector -> Some selector
        | Dep.Env _ | Dep.File _ | Dep.Alias _ | Dep.Universe -> None)
    ; Repr.case "Env" Repr.string ~proj:(function
        | Dep.Env var -> Some var
        | Dep.File_selector _ | Dep.File _ | Dep.Alias _ | Dep.Universe -> None)
    ; Repr.case "File" path_repr ~proj:(function
        | Dep.File path -> Some path
        | Dep.File_selector _ | Dep.Env _ | Dep.Alias _ | Dep.Universe -> None)
    ; Repr.case "Alias" alias_repr ~proj:(function
        | Dep.Alias alias -> Some alias
        | Dep.File_selector _ | Dep.Env _ | Dep.File _ | Dep.Universe -> None)
    ; Repr.case0 "Universe" ~test:(function
        | Dep.Universe -> true
        | Dep.File_selector _ | Dep.Env _ | Dep.File _ | Dep.Alias _ -> false)
    ]
;;

let deps_repr = Repr.view (Repr.list dep_repr) ~to_:Dep.Set.to_list

let action_repr =
  Repr.view syntax_repr ~to_:(fun action ->
    Action.for_shell action |> Dune_rules.Action_for_shell.encode_for_rules)
;;

let target_names ~root names =
  Filename.Set.to_list_map names ~f:(fun name ->
    Path.Build.relative_fname root name |> Path.Build.to_string)
;;

let targets_repr =
  Repr.record
    "targets"
    [ Repr.field "files" (Repr.list Repr.string) ~get:(fun targets ->
        target_names ~root:targets.Targets.Validated.root targets.files)
    ; Repr.field "directories" (Repr.list Repr.string) ~get:(fun targets ->
        target_names ~root:targets.Targets.Validated.root targets.dirs)
    ]
;;

let rule_context (rule : Dune_engine.Reflection.Rule.t) =
  match Path.Build.extract_build_context rule.targets.Targets.Validated.root with
  | None -> None
  | Some (context, _) -> Some (Filename.to_string context)
;;

let rule_repr =
  Repr.record
    "rule"
    [ Repr.field "deps" deps_repr ~get:(fun rule -> rule.Dune_engine.Reflection.Rule.deps)
    ; Repr.field "targets" targets_repr ~get:(fun rule ->
        rule.Dune_engine.Reflection.Rule.targets)
    ; Repr.field "context" (Repr.option Repr.string) ~get:rule_context
    ; Repr.field "action" action_repr ~get:(fun rule ->
        rule.Dune_engine.Reflection.Rule.action)
    ]
;;

let rules_repr = Repr.list rule_repr
let deps_only_repr = Repr.list deps_repr

let dep_sets_of_rules rules =
  List.map rules ~f:(fun rule -> rule.Dune_engine.Reflection.Rule.deps)
;;

let rec normalize_dyn : Dyn.t -> Dyn.t = function
  | Option None -> Option None
  | Option (Some value) -> normalize_dyn value
  | List values -> List (List.map values ~f:normalize_dyn)
  | Array values -> Array (Array.map values ~f:normalize_dyn)
  | Tuple values -> Tuple (List.map values ~f:normalize_dyn)
  | Record fields ->
    Record
      (List.filter_map fields ~f:(fun (name, value) ->
         match normalize_dyn value with
         | Option None -> None
         | value -> Some (name, value)))
  | Variant (name, values) -> Variant (name, List.map values ~f:normalize_dyn)
  | Map values ->
    Map (List.map values ~f:(fun (key, value) -> normalize_dyn key, normalize_dyn value))
  | Set values -> Set (List.map values ~f:normalize_dyn)
  | ( Opaque
    | Unit
    | Int _
    | Int32 _
    | Int64 _
    | Nativeint _
    | Bool _
    | String _
    | Bytes _
    | Char _
    | Float _ ) as value -> value
;;

let dyn_of_repr repr value = normalize_dyn (Repr.to_dyn repr value)

let rec dune_lang_of_sexp : Sexp.t -> Dune_lang.t = function
  | Atom value -> Dune_lang.atom_or_quoted_string value
  | List values -> List (List.map values ~f:dune_lang_of_sexp)
;;

let print_sexp_repr ppf repr value =
  let sexp = dyn_of_repr repr value |> Sexp.of_dyn |> dune_lang_of_sexp in
  Format.fprintf ppf "%a@," Pp.to_fmt (Dune_lang.pp sexp)
;;

let print_json oc json =
  output_string oc (Json.to_string json);
  output_char oc '\n'
;;

let print_rules_sexp ppf rules =
  Syntax.prepare_formatter ppf;
  Format.pp_open_vbox ppf 0;
  Format.pp_print_list (fun ppf rule -> print_sexp_repr ppf rule_repr rule) ppf rules;
  Format.pp_print_flush ppf ()
;;

let print_rule_deps_only_sexp ppf rules =
  Syntax.prepare_formatter ppf;
  Format.pp_open_vbox ppf 0;
  Format.pp_print_list
    (fun ppf rule -> print_sexp_repr ppf deps_repr rule.Dune_engine.Reflection.Rule.deps)
    ppf
    rules;
  Format.pp_print_flush ppf ()
;;

let term =
  let+ builder = Common.Builder.term
  and+ out =
    Arg.(
      value
      & opt (some string) None
      & info [ "o" ] ~docv:"FILE" ~doc:(Some "Output to a file instead of stdout."))
  and+ recursive =
    Arg.(
      value
      & flag
      & info
          [ "r"; "recursive" ]
          ~doc:
            (Some
               "Print all rules needed to build the transitive dependencies of the given \
                targets."))
  and+ deps_only =
    Arg.(
      value
      & flag
      & info [ "deps" ] ~doc:(Some "Only print the dependencies of matching rules."))
  and+ format =
    let doc = Printf.sprintf "$(docv) must be %s" (Arg.doc_alts_enum Output_format.all) in
    Arg.(
      value
      & opt (enum Output_format.all) Output_format.Sexp
      & info [ "format" ] ~docv:"FORMAT" ~doc:(Some doc))
  (* CR-someday Alizter: document this option *)
  and+ targets = Arg.(value & pos_all dep [] & Arg.info [] ~docv:"TARGET" ~doc:None) in
  let common, config = Common.init builder in
  let out = Option.map ~f:Path.of_string out in
  Scheduler_setup.go_with_rpc_server ~common ~config (fun () ->
    Build.build_memo_exn (fun () ->
      let open Memo.O in
      let* setup = Util.setup () in
      let* request =
        match targets with
        | [] ->
          Target.all_direct_targets None
          >>| Path.Build.Map.foldi ~init:[] ~f:(fun p _ acc -> Path.build p :: acc)
          >>| Action_builder.paths
        | _ -> Memo.return (Target.interpret_targets (Common.root common) setup targets)
      in
      let+ rules = Dune_engine.Reflection.eval ~request ~recursive in
      let print oc =
        let ppf = Format.formatter_of_out_channel oc in
        match format, deps_only with
        | Output_format.Sexp, false -> print_rules_sexp ppf rules
        | Output_format.Sexp, true -> print_rule_deps_only_sexp ppf rules
        | Json, false -> print_json oc (Json.of_dyn (dyn_of_repr rules_repr rules))
        | Json, true ->
          print_json
            oc
            (Json.of_dyn (dyn_of_repr deps_only_repr (dep_sets_of_rules rules)))
      in
      match out with
      | None -> print stdout
      | Some fn -> Io.with_file_out fn ~f:print))
;;

let command = Cmd.v info term
