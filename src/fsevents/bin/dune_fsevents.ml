let paths, latency =
  let latency = ref 0. in
  let paths = ref [] in
  let anon p = paths := p :: !paths in
  Arg.parse
    [ "--latency", Arg.Set_float latency, "latency" ]
    anon
    "dune_fsevents [--latency float] [path]+";
  !paths, !latency
;;

let fsevents =
  Fsevents.create ~paths ~latency ~f:(fun events ->
    ListLabels.iter events ~f:(fun evt ->
      Printf.printf "%s\n%!" (Dyn.to_string (Fsevents.Event.to_dyn_raw evt))))
;;

let () =
  let dispatch_queue = Fsevents.Dispatch_queue.create () in
  Fsevents.start fsevents dispatch_queue;
  match Fsevents.Dispatch_queue.wait_until_stopped dispatch_queue with
  | Ok () -> ()
  | Error e -> raise e
;;
