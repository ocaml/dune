open Import

let digest_with_lstat path =
  match Path.lstat path with
  | Error e ->
    User_error.raise
      [ Pp.textf "Can't stat path %S:" (Path.to_string path); Unix_error.Detailed.pp e ]
  | Ok stats ->
    let stats_for_digest = Dune_digest.Stats_for_digest.of_unix_stats stats in
    (match Dune_digest.path_with_stats ~allow_dirs:true path stats_for_digest with
     | Ok digest -> digest
     | Error (Unix_error e) ->
       User_error.raise
         [ Pp.textf "Can't digest path %S:" (Path.to_string path)
         ; Unix_error.Detailed.pp e
         ]
     | Error Unexpected_kind ->
       User_error.raise
         [ Pp.textf
             "Can't digest path %S: Unexpected file kind %S (%s)"
             (Path.to_string path)
             (File_kind.to_string stats_for_digest.st_kind)
             (File_kind.to_string_hum stats_for_digest.st_kind)
         ])
;;
