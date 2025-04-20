open Import

let process_rule ~expander rule =
  let { Rule_conf.targets; action; _ } = rule in
  (* Convert targets to the format expected by setup_rule *)
  let string_targets, static_targets =
    match targets with
    | Infer -> Targets_spec.Infer, Targets_spec.Infer
    | Static { targets; multiplicity; named_targets } ->
      (* For string_targets (going to setup_rule) *)
      let string_sw_targets =
        List.map targets ~f:(fun (sw, _kind) ->
          match String_with_vars.text_only sw with
          | Some s -> s, sw (* This makes (string * String_with_vars.t) *)
          | None -> failwith "Cannot expand variables in target name")
      in
      (* No need to transform named_targets as they're already (string * String_with_vars.t) *)
      let string_sw_spec =
        Targets_spec.Static
          { targets = string_sw_targets
          ; multiplicity
          ; named_targets (* Keep as is - already (string * String_with_vars.t) *)
          }
      in
      (* For static_targets (to return from function) *)
      let processed_targets =
        List.map targets ~f:(fun (sw, kind) ->
          match String_with_vars.text_only sw with
          | Some s -> s, kind
          | None -> failwith "Cannot expand variables in target name")
      in
      let processed_named =
        List.map named_targets ~f:(fun (name, sw) ->
          match String_with_vars.text_only sw with
          | Some s -> name, s
          | None ->
            failwith (Printf.sprintf "Cannot expand variables in named target '%s'" name))
      in
      let processed_spec =
        Targets_spec.Static
          { targets = processed_targets; multiplicity; named_targets = processed_named }
      in
      string_sw_spec, processed_spec
  in
  let expander', action = Expander.setup_rule ~expander ~targets:string_targets ~action in
  expander', static_targets, action
;;
