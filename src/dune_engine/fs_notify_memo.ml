open! Stdune
open! Import

let memo =
  Memo.create "fs_notify" ~doc:"fs notify"
    ~input:(module Path)
    ~output:(Simple (module Unit))
    ~visibility:Hidden
    (fun _path -> Memo.Build.return ())

let depend p = Memo.exec memo p

let invalidate p = Memo.Cell.invalidate (Memo.cell memo p)
