open Stdune
open Dune_manager

let runtime_dir =
  let xdg =
    try Sys.getenv "XDG_RUNTIME_DIR"
    with Not_found -> failwith "XDG_RUNTIME_DIR is not set"
  in
  Filename.concat xdg "dune-manager"

let () =
  Path.mkdir_p (Path.of_string runtime_dir) ;
  let manager = DuneManager.make ()
  and port_path = Filename.concat runtime_dir "port" in
  let port_f port =
    let c = open_out port_path in
    let f () = output_string c (string_of_int port)
    and finally () = close_out c in
    Exn.protect ~f ~finally
  in
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle (fun _ -> DuneManager.stop manager)) ;
  try
    let f () = DuneManager.run ~port_f manager
    and finally () = Sys.remove port_path in
    Exn.protect ~f ~finally
  with
  | DuneManager.Error s ->
      Printf.fprintf stderr "%s: fatal error: %s\n" Sys.argv.(0) s ;
      exit 1
  | DuneManager.Stop ->
      ()
