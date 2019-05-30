open! Stdune

module Context = Dune.Context
module Workspace = Dune.Workspace
module Dune_project = Dune.Dune_project
module Vcs = Dune.Vcs

let die = Dune.Import.die
let hint = Dune.Import.hint

type checked =
  | In_build_dir of (Context.t * Path.Source.t)
  | In_install_dir of (Context.t * Path.Source.t)
  | In_source_dir of Path.Source.t
  | External of Path.External.t

let check_path contexts =
  let contexts =
    String.Map.of_list_exn (List.map contexts ~f:(fun c -> c.Context.name, c))
  in
  fun path ->
    let internal path =
      die "This path is internal to dune: %s"
        (Path.to_string_maybe_quoted path)
    in
    let context_exn ctx =
      match String.Map.find contexts ctx with
      | Some context -> context
      | None ->
        die "%s refers to unknown build context: %s%s"
          (Path.to_string_maybe_quoted path)
          ctx
          (hint ctx (String.Map.keys contexts))
    in
    match Path.kind path with
    | External e -> External e
    | Local _ ->
      match Path.as_in_source_tree path with
      | Some src -> In_source_dir src
      | None ->
        match Path.extract_build_context path with
        | None -> internal path
        | Some (name, src) ->
          if name = "" || name.[0] = '.' then internal path;
          if name = "install" then (
            match Path.Source.split_first_component src with
            | None -> internal path
            | Some (ctx, src) ->
              In_install_dir (context_exn ctx, Path.Source.of_relative src)
          )
          else (In_build_dir (context_exn name, src))
