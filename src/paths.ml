open Stdune

let digest_db     = Path.relative_to_build_dir ".digest-db"
let db            = Path.relative_to_build_dir ".db"
let log           = Path.relative_to_build_dir "log"
let to_promote    = Path.relative_to_build_dir ".to-promote"
let aliases       = Path.relative_to_build_dir ".aliases"
let misc          = Path.relative_to_build_dir ".misc"
let universe_file = Path.relative_to_build_dir ".universe-state"
let install_dir   = Path.relative_to_build_dir "install"
let sandbox_dir   = Path.relative_to_build_dir ".sandbox"

let to_delete_source_tree =
  Path.relative_to_build_dir ".to-delete-in-source-tree"
