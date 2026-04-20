open Import

let digest_with_stat path =
  match Path.stat path with
  | Error e ->
    User_error.raise
      [ Pp.textf "Can't stat path %S:" (Path.to_string path); Unix_error.Detailed.pp e ]
  | Ok stats ->
    let stats_for_digest = Dune_digest.Stats_for_digest.of_unix_stats stats in
    (match stats_for_digest.st_kind with
     | S_DIR ->
       let d = Dune_digest.Manual.create () in
       Fpath.traverse
         ~init:()
         ~sorted:true
         ~on_file:(fun ~dir fname () ->
           let path = Path.L.relative path [ dir; fname ] in
           Dune_digest.Manual.string d (Path.to_string path);
           let digest = Dune_digest.file path in
           Dune_digest.Manual.digest d digest)
         ~on_dir:(fun ~dir fname () ->
           let path = Path.L.relative path [ dir; fname ] in
           Dune_digest.Manual.string d (Path.to_string path))
         ~dir:(Path.to_string path)
         ();
       Dune_digest.Manual.get d
     | _ ->
       (match Dune_digest.path_with_stats path stats_for_digest with
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
            ]))
;;
