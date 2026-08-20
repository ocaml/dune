external available : unit -> bool = "dune_test_landlock_scope_available"
external restrict : unit -> unit = "dune_test_landlock_scope_restrict"

let () =
  match Array.to_list Sys.argv with
  | [ _; "--available" ] -> if not (available ()) then exit 1
  | _ :: "--" :: prog :: args ->
    restrict ();
    Unix.execvp prog (Array.of_list (prog :: args))
  | _ -> exit 2
;;
