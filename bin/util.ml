open! Stdune

module Context = Dune.Context
module Workspace = Dune.Workspace
module Dune_project = Dune.Dune_project

let die = Dune.Import.die
let hint = Dune.Import.hint
let warn = Dune.Errors.warn

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

let find_root () =
  let cwd = Sys.getcwd () in
  let rec loop counter ~candidates ~to_cwd dir =
    match Sys.readdir dir with
    | exception (Sys_error msg) ->
      warn Loc.none
        "Unable to read directory %s. \
         Will not look for root in parent directories@.\
         Reason: %s@.\
         To remove this warning, set your root explicitly using --root.@."
        dir msg;
      candidates
    | files ->
      let files = String.Set.of_list (Array.to_list files) in
      if String.Set.mem files Workspace.filename then
        cont counter ~candidates:((0, dir, to_cwd) :: candidates) dir ~to_cwd
      else if Wp.t = Jbuilder && String.Set.exists files ~f:(fun fn ->
        String.is_prefix fn ~prefix:"jbuild-workspace") then
        cont counter ~candidates:((1, dir, to_cwd) :: candidates) dir ~to_cwd
      else if String.Set.mem files Dune_project.filename then
        cont counter ~candidates:((2, dir, to_cwd) :: candidates) dir ~to_cwd
      else
        cont counter ~candidates dir ~to_cwd
  and cont counter ~candidates ~to_cwd dir =
    if counter > String.length cwd then
      candidates
    else
      let parent = Filename.dirname dir in
      if parent = dir then
        candidates
      else
        let base = Filename.basename dir in
        loop (counter + 1) parent ~candidates ~to_cwd:(base :: to_cwd)
  in
  match loop 0 ~candidates:[] ~to_cwd:[] cwd with
  | [] -> (cwd, [])
  | l ->
    let lowest_priority =
      List.fold_left l ~init:max_int ~f:(fun acc (prio, _, _) ->
        min acc prio)
    in
    let (_, dir, to_cwd) =
      List.find_exn l ~f:(fun (prio, _, _) -> prio = lowest_priority)
    in
    (dir, to_cwd)
