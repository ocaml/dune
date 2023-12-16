open Import

let find_by_dir map (dir : Path.Source.t) =
  let rec loop d =
    match Path.Source.Map.find map d with
    | Some s -> s
    | None ->
      (match Path.Source.parent d with
       | Some d -> loop d
       | None ->
         Code_error.raise
           "find_by_dir: invalid directory"
           [ "d", Path.Source.to_dyn d; "dir", Path.Source.to_dyn dir ])
  in
  loop dir
;;

let invalid_path dir =
  Code_error.raise
    "path doesn't belong to any source dir"
    [ "dir", Path.Build.to_dyn dir ]
;;

let find_by_dir map ~dir =
  if Path.Build.is_root dir
  then invalid_path dir
  else (
    match Dune_engine.Dpath.analyse_target dir with
    | Regular (name, src) ->
      (match Install.Context.analyze_path name src with
       | Normal (_, path) -> find_by_dir map path
       | _ -> invalid_path dir)
    | _ -> invalid_path dir)
;;
