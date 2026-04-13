open Import

let copy ~src ~dst ~copy_file ~mkdir ~on_unsupported ?(on_symlink = `Raise) () =
  let handle_kind ~src ~dst kind =
    match (kind : Unix.file_kind) with
    | S_REG -> copy_file ~src ~dst
    | S_DIR ->
      mkdir ~src ~dst;
      Fpath.traverse
        ~dir:(Path.to_string src)
        ~init:()
        ~on_file:(fun ~dir fname () ->
          let rel = Filename.concat dir fname in
          let src = Path.relative src rel in
          let dst = Path.relative dst rel in
          copy_file ~src ~dst)
        ~on_dir:(fun ~dir fname () ->
          let rel = Filename.concat dir fname in
          let src = Path.relative src rel in
          let dst = Path.relative dst rel in
          mkdir ~src ~dst)
        ~on_other:
          (`Call
              (fun ~dir fname kind () ->
                let src = Path.relative src (Filename.concat dir fname) in
                on_unsupported ~src kind))
        ~on_symlink:
          (match on_symlink with
           | `Ignore -> `Ignore
           | `Raise -> `Raise
           | `Call f ->
             `Call
               (fun ~dir fname () ->
                 let src = Path.relative src (Filename.concat dir fname) in
                 (), f ~src))
        ()
    | kind -> on_unsupported ~src kind
  in
  let { Unix.st_kind; _ } = Unix.stat (Path.to_string src) in
  match (st_kind : Unix.file_kind) with
  | kind -> handle_kind ~src ~dst kind
;;
