open Stdune
open Fiber.O
module Scheduler = Dune_engine.Scheduler
module Dune_rpc = Dune_rpc_private
module Request = Dune_rpc.Public.Request
module Diagnostic = Dune_rpc.Diagnostic
module Client = Dune_rpc_client.Client
module Session = Csexp_rpc.Session

(* enable to debug process stdout/stderr *)
let debug = false
let () = if debug then Dune_util.Log.init ~file:(Out_channel stderr) ()

let dune_prog =
  lazy
    (let path = Env_path.path Env.initial in
     Bin.which ~path "dune" |> Option.value_exn |> Path.to_absolute_filename)
;;

let init_chan ~root_dir =
  let build_dir = Filename.concat root_dir "_build" in
  let once () =
    let env = Env.get Env.initial in
    match Dune_rpc_impl.Where.Where.get ~env ~build_dir with
    | Error exn -> Exn.raise exn
    | Ok None -> Fiber.return None
    | Ok (Some where) ->
      let+ conn = Client.Connection.connect where in
      (match conn with
       | Ok s -> Some s
       | Error _ -> None)
  in
  let rec loop () =
    let* res = once () in
    match res with
    | Some res -> Fiber.return res
    | None -> Scheduler.sleep 0.2 >>= loop
  in
  loop ()
;;

let request_exn client witness n =
  let* staged = Client.Versioned.prepare_request client witness in
  let staged =
    match staged with
    | Ok s -> s
    | Error e -> raise (Dune_rpc.Version_error.E e)
  in
  Client.request client staged n
;;

let notification_exn client witness n =
  let* staged = Client.Versioned.prepare_notification client witness in
  let staged =
    match staged with
    | Ok s -> s
    | Error e -> raise (Dune_rpc.Version_error.E e)
  in
  Client.notification client staged n
;;

let run_client ?handler f =
  let* chan = init_chan ~root_dir:"." in
  let initialize =
    let id = Dune_rpc.Id.make (Atom "test") in
    Dune_rpc.Initialize.Request.create ~id
  in
  Dune_rpc_impl.Client.client ?handler chan initialize ~f:(fun client ->
    Fiber.finalize
      (fun () -> f client)
      ~finally:(fun () ->
        notification_exn client Dune_rpc.Public.Notification.shutdown ()))
;;

let read_lines in_ =
  let in_ = Unix.in_channel_of_descr in_ in
  let rec loop acc =
    let* res = Scheduler.async (fun () -> input_line in_) in
    match res with
    | Ok a -> loop (a :: acc)
    | Error e ->
      (match e.exn with
       | End_of_file -> ()
       | _ ->
         Format.eprintf "Error reading channel: %a@.%!" Exn_with_backtrace.pp_uncaught e);
      Fiber.return (String.concat (List.rev acc) ~sep:"\n")
  in
  let+ res = loop [] in
  close_in_noerr in_;
  res
;;

let run ?env ~prog ~argv () =
  let stdout_i, stdout_w = Unix.pipe ~cloexec:true () in
  let stderr_i, stderr_w = Unix.pipe ~cloexec:true () in
  let pid =
    let argv = prog :: argv in
    let env = Option.map ~f:Spawn.Env.of_list env in
    Spawn.spawn
      ~prog
      ~argv
      ~stdout:stdout_w
      ~stderr:stderr_w
      ~stdin:(Lazy.force Dev_null.in_)
      ?env
      ()
    |> Pid.of_int
  in
  Unix.close stdout_w;
  Unix.close stderr_w;
  ( pid
  , (let+ proc = Scheduler.wait_for_process ~timeout:3.0 pid in
     if proc.status <> Unix.WEXITED 0
     then (
       let name =
         sprintf "%s %s" ("$PATH/" ^ Filename.basename prog) (String.concat ~sep:" " argv)
       in
       match proc.status with
       | Unix.WEXITED i -> printfn "%s returned %d" name i
       | Unix.WSIGNALED i -> printfn "%s received signal %i" name i
       | _ -> assert false))
  , read_lines stdout_i
  , read_lines stderr_i )
;;

let run_server ?(watch_mode_args = [ "--passive-watch-mode" ]) ?env ~root_dir () =
  run
    ?env
    ~prog:(Lazy.force dune_prog)
    ~argv:([ "build"; "--root"; root_dir ] @ watch_mode_args)
    ()
;;

let dune_build client what =
  printfn "Building %s" what;
  let+ res =
    request_exn client (Dune_rpc.Decl.Request.witness Dune_rpc_impl.Decl.build) [ what ]
  in
  match res with
  | Error e ->
    Format.eprintf
      "Error building %s:@.%s@."
      what
      (Dyn.to_string (Dune_rpc.Response.Error.to_dyn e))
  | Ok res ->
    printfn
      "Build %s %s"
      what
      (match res with
       | Success -> "succeeded"
       | Failure -> "failed")
;;

let with_dune_watch ?watch_mode_args ?env f =
  let root_dir = "." in
  let xdg_runtime_dir = Filename.get_temp_dir_name () in
  Unix.putenv "XDG_RUNTIME_DIR" xdg_runtime_dir;
  let pid, run_server, server_stdout, server_stderr =
    run_server ?watch_mode_args ?env ~root_dir ()
  in
  let+ res, (stdout, stderr) =
    Fiber.fork_and_join
      (fun () -> Fiber.fork_and_join_unit (fun () -> run_server) (fun () -> f pid))
      (fun () -> Fiber.fork_and_join (fun () -> server_stdout) (fun () -> server_stderr))
  in
  (* We wait until the tests finish to print stdout and stderr for determinism.
     But this has the disadvantage that the fiber above will not always
     terminate for failed tests. Thus, the output below will never be shown. *)
  if debug
  then (
    if stdout <> "" then printfn "stdout:\n%s" stdout;
    if stderr <> "" then printfn "stderr:\n%s" stderr);
  res
;;

let config =
  Dune_engine.Clflags.display := Quiet;
  { Scheduler.Config.concurrency = 1
  ; stats = None
  ; print_ctrl_c_warning = false
  ; watch_exclusions = []
  }
;;

let run run =
  let cwd = Sys.getcwd () in
  let dir = Temp.create Dir ~prefix:"dune" ~suffix:"rpc_test" in
  let run () =
    Fiber.with_error_handler run ~on_error:(fun exn ->
      Exn_with_backtrace.pp_uncaught Format.err_formatter exn;
      Format.pp_print_flush Format.err_formatter ();
      Exn_with_backtrace.reraise exn)
  in
  Exn.protect
    ~finally:(fun () -> Sys.chdir cwd)
    ~f:(fun () ->
      Sys.chdir (Path.to_string dir);
      Scheduler.Run.go config run ~timeout:5.0 ~on_event:(fun _ _ -> ()))
;;
