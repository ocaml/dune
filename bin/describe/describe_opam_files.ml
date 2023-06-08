open Import
open Stdune

let term =
  let+ common = Common.term
  and+ format = Describe_common.Format.arg in
  let config = Common.init common in
  Scheduler.go ~common ~config @@ fun () ->
  let open Fiber.O in
  let+ res =
    Build_system.run_exn (fun () -> Describe_common.Opam_files.get ())
  in
  match format with
  | Csexp -> Csexp.to_channel stdout (Sexp.of_dyn res)
  | Sexp -> Describe_common.print_as_sexp res

let command =
  let doc =
    "Print information about the opam files that have been discovered."
  in
  let info = Cmd.info ~doc "opam-files" in
  Cmd.v info term
