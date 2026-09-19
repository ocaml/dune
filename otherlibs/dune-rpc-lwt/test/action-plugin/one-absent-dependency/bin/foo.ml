open Dune_rpc_lwt.V1.Action_plugin

let action dap =
  let open Lwt.Syntax in
  let read () = read_file dap ~path:"some_absent_dependency" in
  if Array.length Sys.argv = 1
  then
    let* data = read () in
    Lwt_io.printl data
  else
    let* data =
      Lwt.catch read (function
        | Error.E _ -> Lwt.return "fallback"
        | exn -> Lwt.fail exn)
    in
    Lwt_io.with_file ~mode:Output "result" (fun output -> Lwt_io.write_line output data)
;;

let retry_action dap =
  Lwt_list.iter_s
    (fun path ->
       Lwt.catch
         (fun () ->
            let open Lwt.Syntax in
            let* () = build_deps dap [ Dep.File path ] in
            Lwt_io.printf "request %s: built\n" path)
         (function
           | Error.E _ -> Lwt_io.printf "request %s: rejected\n" path
           | exn -> Lwt.fail exn))
    [ "missing-first"; "after-failure" ]
;;

let () =
  let action =
    match Sys.argv with
    | [| _; "retry" |] -> retry_action
    | _ -> action
  in
  try Lwt_main.run (run action) with
  | Error.E message ->
    prerr_endline message;
    exit 1
;;
