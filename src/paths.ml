open Stdune

let make s =
  let d = Path.relative_to_build_dir s in
  fun () -> d

let digest_db             = make ".digest-db"
let db                    = make ".db"
let log                   = make "log"
let to_delete_source_tree = make ".to-delete-in-source-tree"
let to_promote            = make ".to-promote"
let aliases               = make ".aliases"
let misc                  = make ".misc"
let universe_file         = make ".universe-state"
let install_dir           = make "install"
let sandbox_dir           = make ".sandbox"
