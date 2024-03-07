open! Stdune

let paths =
  let paths = ref [] in
  let anon p = paths := p :: !paths in
  Arg.parse [] anon "dune_fswatch_win [path]+";
  List.rev !paths
;;

let () =
  let t = Fswatch_win.create () in
  List.iter ~f:(Fswatch_win.add t) paths;
  let f event = Printf.printf "%s\n%!" (Dyn.to_string (Fswatch_win.Event.to_dyn event)) in
  while true do
    List.iter ~f (Fswatch_win.wait t ~sleep:500)
  done
;;
