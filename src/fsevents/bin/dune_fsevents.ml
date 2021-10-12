let paths, latency =
  let latency = ref 0. in
  let paths = ref [] in
  let anon p = paths := p :: !paths in
  Arg.parse
    [ ("--latency", Arg.Set_float latency, "latency") ]
    anon "dune_fsevents [--latency float] [path]+";
  (!paths, !latency)

let fsevents =
  Fsevents.create ~paths ~latency ~f:(fun events ->
      ListLabels.iter events ~f:(fun evt ->
          Printf.printf "%s\n%!" (Dyn.to_string (Fsevents.Event.to_dyn_raw evt))))

let () =
  let runloop = Fsevents.RunLoop.in_current_thread () in
  Fsevents.start fsevents runloop;
  match Fsevents.RunLoop.run_current_thread runloop with
  | Ok () -> ()
  | Error e -> raise e
