open! Stdune
open! Import

let memo =
  Memo.create "fs_notify" ~doc:"fs notify"
    ~input:(module Path)
    ~output:(Simple (module Unit))
    ~visibility:Hidden
    (fun _path -> Memo.Build.return ())

let depend path = Memo.exec memo path

module Invalidate_result = struct
  type t =
    | Invalidated
    | Skipped
end

let invalidate path =
  match Memo.Expert.previously_evaluated_cell memo path with
  | None -> Invalidate_result.Skipped
  | Some cell ->
    Memo.Cell.invalidate cell;
    Invalidated
