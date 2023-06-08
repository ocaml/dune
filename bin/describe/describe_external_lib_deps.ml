open Import
open Stdune

let term =
  let+ common = Common.term
  and+ context_name = Common.context_arg ~doc:"Build context to use."
  and+ format = Describe_common.Format.arg in
  let config = Common.init common in
  Scheduler.go ~common ~config @@ fun () ->
  let open Fiber.O in
  let* setup = Import.Main.setup () in
  let* setup = Memo.run setup in
  let super_context = Import.Main.find_scontext_exn setup ~name:context_name in
  let+ res =
    Build_system.run_exn (fun () ->
        Describe_common.External_lib_deps.get setup super_context)
  in
  match format with
  | Csexp -> Csexp.to_channel stdout (Sexp.of_dyn res)
  | Sexp -> Describe_common.print_as_sexp res

let command =
  let doc =
    "Print out external libraries needed to build the project. It's an \
     approximated set of libraries."
  in
  let info = Cmd.info ~doc "external-lib-deps" in
  Cmd.v info term
