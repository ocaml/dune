open Import

type checked =
  | In_build_dir of (Context.t * Path.Source.t)
  | In_private_context of Path.Build.t
  | In_install_dir of (Context.t * Path.Source.t)
  | In_source_dir of Path.Source.t
  | External of Path.External.t

let check_path contexts =
  let contexts =
    Dune_engine.Context_name.Map.of_list_map_exn contexts ~f:(fun c -> Context.name c, c)
  in
  fun path ->
    let internal_path () =
      User_error.raise
        [ Pp.textf "This path is internal to dune: %s" (Path.to_string_maybe_quoted path)
        ]
    in
    let context_exn ctx =
      match Dune_engine.Context_name.Map.find contexts ctx with
      | Some context -> context
      | None ->
        User_error.raise
          [ Pp.textf
              "%s refers to unknown build context: %s"
              (Path.to_string_maybe_quoted path)
              (Dune_engine.Context_name.to_string ctx)
          ]
          ~hints:
            (User_message.did_you_mean
               (Dune_engine.Context_name.to_string ctx)
               ~candidates:
                 (Dune_engine.Context_name.Map.keys contexts
                  |> List.map ~f:Dune_engine.Context_name.to_string))
    in
    match path with
    | External e -> External e
    | In_source_tree s -> In_source_dir s
    | In_build_dir path ->
      (match Dune_engine.Dpath.analyse_target path with
       | Other _ -> internal_path ()
       | Alias (_, _) -> internal_path ()
       | Anonymous_action _ -> internal_path ()
       | Regular (name, src) ->
         (match Install.Context.analyze_path name src with
          | Invalid -> internal_path ()
          | Install (ctx, path) -> In_install_dir (context_exn ctx, path)
          | Normal (ctx, path) ->
            if Context_name.equal ctx Dune_rules.Private_context.t.name
            then
              In_private_context
                (Path.Build.append_source Dune_rules.Private_context.t.build_dir path)
            else In_build_dir (context_exn ctx, path)))
;;
