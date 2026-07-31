open Stdune
open Fiber.O
open Dune_scheduler
module Where = Dune_rpc.Private.Where
module Registry = Dune_rpc.Private.Registry
module Poll_active = Rpc.Poll_active
open Dune_rpc_e2e

let try_ ~times ~delay_seconds ~f =
  let rec loop = function
    | 0 -> Fiber.return None
    | n ->
      let* res = f () in
      (match res with
       | Some s -> Fiber.return (Some s)
       | None ->
         let* () = Scheduler.sleep (Time.Span.of_secs delay_seconds) in
         loop (n - 1))
  in
  loop times
;;

let run =
  let cwd = Sys.getcwd () in
  Dune_engine.Clflags.display := Quiet;
  let config =
    { Scheduler.Config.concurrency = 1
    ; print_ctrl_c_warning = false
    ; watch_exclusions = []
    }
  in
  fun run ->
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
        Scheduler.Run.go config run ~timeout:(Time.Span.of_secs 5.0) ~on_event:(fun _ ->
          ()))
;;

let%expect_test "poll skips scans after the registry mtime changes" =
  let module IO = struct
    let mtime = ref 0.0
    let file : Registry.File.t option ref = ref None
    let scans = ref 0
    let stat _ = Fiber.return (Ok (`Mtime !mtime))

    let scandir _ =
      incr scans;
      let files =
        match !file with
        | None -> []
        | Some { Registry.File.path; _ } -> [ Filename.basename path ]
      in
      Fiber.return (Ok files)
    ;;

    let read_file path =
      match !file with
      | Some { Registry.File.path = registered_path; contents }
        when String.equal path registered_path -> Fiber.return (Ok contents)
      | None | Some _ -> Fiber.return (Error (Failure path))
    ;;
  end
  in
  let module Poll = Registry.Poll (Fiber) (IO) in
  let case () =
    let config =
      Registry.Config.create
        (Xdg.create
           ~env:(function
             | "XDG_RUNTIME_DIR" -> Some "."
             | _ -> None)
           ())
    in
    let registry = Registry.create config in
    let poll description =
      let+ result = Poll.poll registry in
      match result with
      | Error exn -> raise exn
      | Ok refresh ->
        printfn
          "%s: scans=%d added=%d current=%d"
          description
          !IO.scans
          (List.length (Registry.Refresh.added refresh))
          (List.length (Registry.current registry))
    in
    let* () = poll "initial" in
    let dune = Registry.Dune.create ~where:(`Unix "rpc") ~root:"." ~pid:1 in
    let (`Caller_should_write file) = Registry.Config.register config dune in
    IO.file := Some file;
    IO.mtime := 1.0;
    let* () = poll "after change" in
    poll "subsequent poll"
  in
  run case;
  [%expect
    {|
    initial: scans=1 added=0 current=0
    after change: scans=1 added=0 current=0
    subsequent poll: scans=1 added=0 current=0 |}]
;;

let%expect_test "turn on dune watch and wait until the connection is listed" =
  let case () =
    let runtime_dir = "_runtime_dir" in
    Unix.mkdir runtime_dir 0o777;
    let xdg_runtime_dir = Filename.concat "." runtime_dir in
    let config =
      Registry.Config.create
        (Xdg.create
           ~env:(function
             | "XDG_RUNTIME_DIR" -> Some xdg_runtime_dir
             | _ -> None)
           ())
    in
    let poll = Registry.create config in
    let+ dune =
      let env =
        ("XDG_RUNTIME_DIR=" ^ xdg_runtime_dir) :: Array.to_list (Unix.environment ())
      in
      with_dune_watch ~env (fun pid ->
        let+ res =
          try_ ~times:5 ~delay_seconds:0.2 ~f:(fun () ->
            let+ refresh = Poll_active.poll poll in
            match refresh with
            | Error _ -> None
            | Ok r ->
              if List.is_non_empty (Registry.Refresh.removed r)
              then Code_error.raise "removed should be empty" [];
              (match Registry.Refresh.errored r with
               | [] -> ()
               | errors ->
                 List.map errors ~f:(fun (name, exn) -> name, Exn.to_dyn exn)
                 |> Code_error.raise "errored should be empty");
              (match Registry.Refresh.added r with
               | [ a ] -> Some a
               | [] -> None
               | _ :: _ ->
                 Code_error.raise "added returned more than one dune instance" []))
        in
        Unix.kill (Stdune.Pid.to_int pid) Sys.sigint;
        res)
    in
    match dune with
    | None -> printfn "[FAILURE] unable to find connection"
    | Some dune ->
      let root = Registry.Dune.root dune in
      let where =
        match Registry.Dune.where dune with
        | `Ip (host, port) -> `Ip (host, port)
        | `Unix path ->
          let cwd = Sys.getcwd () in
          `Unix
            (match String.drop_prefix path ~prefix:cwd with
             | None -> path
             | Some s -> "$CWD" ^ s)
      in
      printfn "[PASS] found %s at %s" root (Where.to_string where)
  in
  run case;
  [%expect
    {|
    $PATH/dune build --root . --passive-watch-mode returned 130
    [PASS] found . at unix:path=%24CWD/_build/.rpc/dune |}]
;;
