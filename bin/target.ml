open Stdune

module Context = Dune.Context
module Build = Dune.Build
module Build_system = Dune.Build_system
module Log = Dune.Log

let die = Dune.Import.die
let hint = Dune.Import.hint

type t =
  | File      of Path.t
  | Alias     of Alias.t

type resolve_input =
  | Path of Path.t
  | String of string

let request (setup : Dune.Main.build_system) targets =
  let open Build.O in
  List.fold_left targets ~init:(Build.return ()) ~f:(fun acc target ->
    acc >>>
    match target with
    | File path -> Build.path path
    | Alias { Alias. name; recursive; dir; contexts } ->
      let contexts = List.map ~f:Dune.Context.name contexts in
      (if recursive then
         Build_system.Alias.dep_rec_multi_contexts
       else
         Build_system.Alias.dep_multi_contexts)
        ~dir ~name ~file_tree:setup.workspace.conf.file_tree ~contexts)

let log_targets ~log targets =
  List.iter targets ~f:(function
    | File path ->
      Log.info log @@ "- " ^ (Path.to_string path)
    | Alias a -> Log.info log (Alias.to_log_string a));
  flush stdout

let target_hint (_setup : Dune.Main.build_system) path =
  assert (Path.is_managed path);
  let sub_dir = Option.value ~default:path (Path.parent path) in
  let candidates = Build_system.all_targets () in
  let candidates =
    if Path.is_in_build_dir path then
      candidates
    else
      List.map candidates ~f:(fun path ->
        match Path.extract_build_context path with
        | None -> path
        | Some (_, path) -> Path.source path)
  in
  let candidates =
    (* Only suggest hints for the basename, otherwise it's slow when there are
       lots of files *)
    List.filter_map candidates ~f:(fun path ->
      if Path.equal (Path.parent_exn path) sub_dir then
        Some (Path.to_string path)
      else
        None)
  in
  let candidates = String.Set.of_list candidates |> String.Set.to_list in
  hint (Path.to_string path) candidates

let resolve_path path ~(setup : Dune.Main.build_system) =
  let checked = Util.check_path setup.workspace.contexts path in
  let can't_build path =
    Error (path, target_hint setup path);
  in
  let as_source_dir src =
    if Dune.File_tree.dir_exists setup.workspace.conf.file_tree src then
      Some [ Alias (Alias.in_dir ~name:"default" ~recursive:true
                      ~contexts:setup.workspace.contexts path) ]
    else
      None
  in
  let build () =
    begin
      if Build_system.is_target path then
        Ok [File path]
      else
        can't_build path
    end
  in
  match checked with
  | External _ ->
    Ok [File path]
  | In_source_dir src ->
    (match as_source_dir src with
     | Some res -> Ok res
     | None ->
       match
         List.filter_map setup.workspace.contexts ~f:(fun ctx ->
           let path = Path.append ctx.Context.build_dir path in
           if Build_system.is_target path then
             Some (File path)
           else
             None)
       with
       | [] -> can't_build path
       | l  -> Ok l)
  | In_build_dir (_ctx, src) ->
    (match as_source_dir src with
     | Some res -> Ok res
     | None ->
       build ())
  | In_install_dir _ ->
    build ()

let resolve_target common ~(setup : Dune.Main.build_system) s =
  match Alias.of_string common s ~contexts:setup.workspace.contexts with
  | Some a -> Ok [Alias a]
  | None ->
    let path = Path.relative_exn Path.root (Common.prefix_target common s) in
    resolve_path path ~setup

let resolve_targets_mixed ~log common (setup : Dune.Main.build_system)
      user_targets =
  match user_targets with
  | [] -> []
  | _ ->
    let targets =
      List.map user_targets ~f:(function
        | String s -> resolve_target common ~setup s
        | Path p -> resolve_path p ~setup) in
    if common.config.display = Verbose then begin
      Log.info log "Actual targets:";
      List.concat_map targets ~f:(function
        | Ok targets -> targets
        | Error _ -> [])
      |> log_targets ~log
    end;
    targets

let resolve_targets ~log common (setup : Dune.Main.build_system) user_targets =
  List.map ~f:(fun s -> String s) user_targets
  |> resolve_targets_mixed ~log common setup

let resolve_targets_exn ~log common setup user_targets =
  resolve_targets ~log common setup user_targets
  |> List.concat_map ~f:(function
    | Error (path, hint) ->
      die "Don't know how to build %a%s" Path.pp path hint
    | Ok targets ->
      targets)
