open Stdune

let () = Dune_tests_common.init ()

include struct
  open Dune_engine
  module Action = Action
  module Display = Display
  module Process = Process
  module Scheduler = Scheduler
end

let%expect_test "dune init proj creates .ocamlformat and .gitignore" =
  let dir = Temp.create Dir ~prefix:"dune" ~suffix:"init_test" in
  Sys.chdir (Path.to_string dir);
  let config =
    { Scheduler.Config.concurrency = 1
    ; stats = None
    ; insignificant_changes = `Ignore
    ; signal_watcher = `No
    ; watch_exclusions = []
    }
  in
  Scheduler.Run.go config ~timeout:5.0 ~file_watcher:No_watcher ~on_event:(fun _ _ -> ())
  @@ fun () ->
  let command = "dune init proj project" in
  let ic = Unix.open_process_in command in
  let rec read_all_output () =
    try
      let _ = input_line ic in
      read_all_output ()
    with
    | End_of_file -> ()
  in
  read_all_output ();
  let _ = Unix.close_process_in ic in
  let check_file file =
    let path = Path.append_local dir (Path.Local.of_string ("project/" ^ file)) in
    match (Unix.stat (Path.to_string path)).st_kind with
    | S_REG -> Printf.printf "File %s created\n" file
    | _ -> failwith (Printf.sprintf "Not a regular file: %s" file)
    | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      Printf.printf "File %s not found\n" file
  in
  Fiber.return (List.iter ~f:check_file [ ".ocamlformat"; ".gitignore" ])
;;
