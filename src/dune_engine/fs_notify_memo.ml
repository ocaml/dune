open! Stdune
open! Import

let memo =
  Memo.create "fs_notify" ~doc:"fs notify"
    ~input:(module Path)
    ~output:(Simple (module Unit))
    ~visibility:Hidden Sync
    (fun _path -> ())

let depend p = Memo.exec memo p

let invalidate p = Memo.Cell.invalidate (Memo.cell memo p)
