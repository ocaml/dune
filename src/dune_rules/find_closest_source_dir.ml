open Import

let find_by_dir_exn map ~dir =
  let rec loop d =
    match Path.Build.Map.find map d with
    | Some s -> s
    | None ->
      (match Path.Build.parent d with
       | Some d -> loop d
       | None ->
         Code_error.raise
           "find_by_dir: invalid directory"
           [ "d", Path.Build.to_dyn d; "dir", Path.Build.to_dyn dir ])
  in
  loop dir
;;
