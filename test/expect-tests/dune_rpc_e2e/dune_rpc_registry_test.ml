open Stdune
open Fiber.O
module Where = Dune_rpc_private.Where
module Registry = Dune_rpc_private.Registry
module Scheduler = Dune_engine.Scheduler
module Poll_active = Dune_rpc_impl.Poll_active
open Dune_rpc_e2e

let try_ ~times ~delay ~f =
  let rec loop = function
    | 0 -> Fiber.return None
    | n ->
      let* res = f () in
      (match res with
       | Some s -> Fiber.return (Some s)
       | None ->
         let* () = Scheduler.sleep delay in
         loop (n - 1))
  in
  loop times
;;

let run =
  let cwd = Sys.getcwd () in
  Dune_engine.Clflags.display := Quiet;
  let config =
    { Scheduler.Config.concurrency = 1
    ; stats = None
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
        Scheduler.Run.go config run ~timeout:5.0 ~on_event:(fun _ _ -> ()))
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
          try_ ~times:5 ~delay:0.2 ~f:(fun () ->
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
