open! Stdune
open! Import
open Memo.Build.O

let memo =
  Memo.create "fs_notify" ~doc:"fs notify"
    ~input:(module Path)
    ~output:(Simple (module Unit))
    ~visibility:Hidden
    (fun _path -> Memo.Build.return ())

(* Currently, we do not expose this low-level primitive. If you need it, perhaps
   you could add a higher-level primitive instead, such as [file_exists]? *)
let depend path =
  if Path.is_in_build_dir path then
    Code_error.raise "Fs_memo.depend called on a build path" [];
  Memo.exec memo path

let file_exists path =
  let+ () = depend path in
  Path.exists path

let file_digest path =
  let+ () = depend path in
  Cached_digest.source_or_external_file path

module Invalidate_result = struct
  type t =
    | Invalidated
    | Skipped
end

let invalidate path =
  if Path.is_in_build_dir path then
    Code_error.raise "Fs_memo.invalidate called on a build path" [];
  match Memo.Expert.previously_evaluated_cell memo path with
  | None -> Invalidate_result.Skipped
  | Some cell ->
    Memo.Cell.invalidate cell;
    Invalidated
