open Stdune
open Dune_manager

let runtime_dir =
  let xdg =
    try Sys.getenv "XDG_RUNTIME_DIR"
    with Not_found -> failwith "XDG_RUNTIME_DIR is not set"
  in
  Filename.concat xdg "dune-manager"

let main () =
  let port_path = ref (Filename.concat runtime_dir "port")
  and usage = Printf.sprintf "%s [OPTIONS]" Sys.argv.(0) in
  Arg.parse_argv Sys.argv
    [ ( "--port"
      , Arg.Set_string port_path
      , Printf.sprintf "file to write listening port to (default: %s)"
          !port_path ) ]
    (fun o -> raise (Arg.Bad (Printf.sprintf "unexpected option: %s" o)))
    usage ;
  Path.mkdir_p (Path.of_string runtime_dir) ;
  let manager = DuneManager.make ()
  and port_f port =
    let c = open_out !port_path in
    let f () = output_string c (string_of_int port)
    and finally () = close_out c in
    Exn.protect ~f ~finally
  in
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle (fun _ -> DuneManager.stop manager)) ;
  try
    let f () = DuneManager.run ~port_f manager
    and finally () = Sys.remove !port_path in
    Exn.protect ~f ~finally
  with
  | DuneManager.Error s ->
      Printf.fprintf stderr "%s: fatal error: %s\n" Sys.argv.(0) s ;
      exit 1
  | DuneManager.Stop ->
      ()

let () =
  try main () with
  | Arg.Bad reason ->
      Printf.fprintf stderr "%s: command line error: %s" Sys.argv.(0) reason ;
      exit 1
  | Arg.Help help ->
      Printf.fprintf stdout "Usage: %s" help ;
      exit 1
