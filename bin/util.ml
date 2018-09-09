open! Stdune
open Dune
open Import

let parse_alias path ~contexts =
  let dir = Path.parent_exn path in
  let name = Path.basename path in
  match Path.extract_build_context dir with
  | None -> (contexts, dir, name)
  | Some ("install", _) ->
    die "Invalid alias: %s.\n\
         There are no aliases in %s."
      (Path.to_string_maybe_quoted Path.(relative build_dir "install"))
      (Path.to_string_maybe_quoted path)
  | Some (ctx, dir) -> ([ctx], dir, name)

let check_path contexts =
  let contexts =
    String.Set.of_list (List.map contexts ~f:(fun c -> c.Context.name))
  in
  fun path ->
    let internal path =
      die "This path is internal to dune: %s"
        (Path.to_string_maybe_quoted path)
    in
    if Path.is_in_build_dir path then
      match Path.extract_build_context path with
      | None -> internal path
      | Some (name, _) ->
        if name = "" || name.[0] = '.' then internal path;
        if not (name = "install" || String.Set.mem contexts name) then
          die "%s refers to unknown build context: %s%s"
            (Path.to_string_maybe_quoted path)
            name
            (hint name (String.Set.to_list contexts))
