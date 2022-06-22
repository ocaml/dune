open Import
open Memo.O

let impl bin =
  let* _ = Build_system.build_file bin in
  let+ line =
    Memo.of_non_reproducible_fiber
    @@ Process.run_capture_line Process.Strict bin [ "--print-version" ]
  in
  match String.lsplit2 ~on:' ' line with
  | Some (coq, _ocaml) -> Some coq
  | None -> None

let memo = Memo.create "coq-version" ~input:(module Path) impl

let version ~bin = Memo.exec memo bin
