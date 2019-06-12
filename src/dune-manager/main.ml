open Stdune
open Dune_manager

let () =
  let manager =
    DuneManager.make ~root:(Path.of_string "/tmp/dune-memory") ()
  in
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle (fun _ -> DuneManager.stop manager)) ;
  try
    DuneManager.run
      ~port_f:(fun port -> Printf.printf "listening on port %i\n%!" port)
      manager
  with
  | DuneManager.Error s ->
      Printf.fprintf stderr "%s: fatal error: %s\n" Sys.argv.(0) s ;
      exit 1
  | DuneManager.Stop ->
      ()
