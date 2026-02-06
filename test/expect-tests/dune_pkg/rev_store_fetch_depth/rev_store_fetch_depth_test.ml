open Stdune
open Fiber.O
open Git_test_utils
module Rev_store = Dune_pkg.Rev_store
module Console = Dune_console

let () =
  let cache_dir = lazy (Temp.create Dir ~prefix:"isolated-cache-" ~suffix:"") in
  let env =
    Env.update Env.initial ~var:"XDG_CACHE_HOME" ~f:(fun _ ->
      Some (Path.to_string (Lazy.force cache_dir)))
    |> Env.get
  in
  Dune_util.override_xdg (Xdg.create ~env ());
  Config.init String.Map.empty;
  Dune_tests_common.init ()
;;

let%expect_test "second fetch uses refs for efficient negotiation (fix #13323)" =
  (* This test verifies that when fetching a second commit from the same repo,
     git can negotiate what it already has using refs created by previous
     fetches. This avoids re-downloading objects we already have. *)
  let repo_dir = Temp.create Dir ~prefix:"git-repo-" ~suffix:"" in
  let trace_file = Temp.create File ~prefix:"git-trace-" ~suffix:".log" in
  let env =
    Env.add Env.initial ~var:"GIT_TRACE_PACKET" ~value:(Path.to_string trace_file)
    |> Env.add ~var:"GIT_PROTOCOL" ~value:"version=2"
  in
  (Dune_scheduler.Scheduler.Run.go
     { concurrency = 2; print_ctrl_c_warning = false; watch_exclusions = [] }
     ~on_event:(fun _ _ -> ())
   @@ fun () ->
   let* rev_store = Rev_store.get in
   let git = git ~dir:repo_dir in
   (* Create a repository with initial commits *)
   let* () = git_init_and_config_user repo_dir in
   (* Allow fetching by SHA (required for git daemon) *)
   let* () = git [ "config"; "uploadpack.allowAnySHA1InWant"; "true" ] in
   let* () =
     Fiber.sequential_iter [ 1; 2; 3 ] ~f:(fun i ->
       let file = sprintf "file%d" i in
       Io.write_lines (Path.relative repo_dir file) [ sprintf "content %d" i ];
       git [ "add"; file ] >>> git [ "commit"; "-m"; sprintf "commit %d" i ])
   in
   let port =
     (* Find an available port by binding to port 0 *)
     let sock = Unix.socket PF_INET SOCK_STREAM 0 in
     Unix.setsockopt sock SO_REUSEADDR true;
     Unix.bind sock (ADDR_INET (Unix.inet_addr_loopback, 0));
     let port =
       match Unix.getsockname sock with
       | Unix.ADDR_INET (_, port) -> port
       | _ -> assert false
     in
     Unix.close sock;
     port
   in
   let url = sprintf "git://127.0.0.1:%d/%s" port (Path.basename repo_dir) in
   (* Run daemon in background while we do the test. The test cancels the
       build to kill the daemon, so we catch errors. *)
   Fiber.map ~f:(fun _ -> ())
   @@ Fiber.map_reduce_errors (module Monoid.Unit) ~on_error:(fun _ -> Fiber.return ())
   @@ fun () ->
   Fiber.fork_and_join_unit
     (fun () ->
        (* Start git daemon to serve the repo over git:// protocol. This is
            needed because file:// URLs don't use pack protocol negotiation.
            The daemon has a timeout as a safety backstop. *)
        let parent_dir = Path.parent_exn repo_dir in
        Dune_engine.Process.run
          ~dir:parent_dir
          ~display
          ~stdout_to:(make_stdout ())
          ~stderr_to:(make_stderr ())
          Dune_engine.Process.Failure_mode.(
            Timeout { timeout = Some (Time.Span.of_secs 5.0); failure_mode = Return })
          (Lazy.force Dune_vcs.Vcs.git)
          [ "daemon"
          ; "--verbose"
          ; "--export-all"
          ; sprintf "--base-path=%s" (Path.to_string parent_dir)
          ; "--listen=127.0.0.1"
          ; sprintf "--port=%d" port
          ; "--reuseaddr"
          ; Path.to_string repo_dir
          ]
        >>| fun _ -> ())
     (fun () ->
        (* Wait for daemon to be ready *)
        let* () =
          (* Wait for the git daemon to be ready by polling with ls-remote *)
          let wait_for_daemon ~url ~max_attempts =
            let rec loop remaining =
              if remaining <= 0
              then Fiber.return (Error "git-daemon failed to start")
              else
                let* result =
                  Dune_engine.Process.run_capture_lines
                    ~dir:(Path.parent_exn repo_dir)
                    ~display
                    ~stderr_to:(make_stderr ())
                    Dune_engine.Process.Failure_mode.Return
                    (Lazy.force Dune_vcs.Vcs.git)
                    [ "ls-remote"; url ]
                in
                match result with
                | _, 0 -> Fiber.return (Ok ())
                | _, _ ->
                  let* () = Dune_scheduler.Scheduler.sleep (Time.Span.of_secs 0.1) in
                  loop (remaining - 1)
            in
            loop max_attempts
          in
          wait_for_daemon ~url ~max_attempts:20
          >>| function
          | Ok () -> ()
          | Error msg -> Code_error.raise msg []
        in
        let remote = Rev_store.remote rev_store ~loc:Loc.none ~url in
        (* Get the first HEAD commit *)
        let* first_head = git_out ~dir:repo_dir [ "rev-parse"; "HEAD" ] in
        (* First fetch - this populates the cache *)
        let* () =
          Rev_store.Object.of_sha1 first_head
          |> Option.value_exn
          |> Rev_store.fetch_object rev_store remote
          >>| function
          | Ok _ -> ()
          | Error lines ->
            Code_error.raise "first fetch failed" [ "output", Dyn.list Dyn.string lines ]
        in
        (* Add more commits to the repo *)
        let* () =
          Fiber.sequential_iter [ 4; 5 ] ~f:(fun i ->
            let file = sprintf "file%d" i in
            Io.write_lines (Path.relative repo_dir file) [ sprintf "content %d" i ];
            git [ "add"; file ] >>> git [ "commit"; "-m"; sprintf "commit %d" i ])
        in
        (* Get the new HEAD commit *)
        let* second_head = git_out ~dir:repo_dir [ "rev-parse"; "HEAD" ] in
        (* Second fetch with tracing - negotiation should use refs from first fetch *)
        let* () =
          Rev_store.Object.of_sha1 second_head
          |> Option.value_exn
          |> Rev_store.fetch_object ~env rev_store remote
          >>| function
          | Ok _ -> ()
          | Error lines ->
            Code_error.raise "second fetch failed" [ "output", Dyn.list Dyn.string lines ]
        in
        let have_count =
          Io.lines_of_file trace_file
          |> List.filter ~f:(Re.execp (Re.compile (Re.str "> have")))
          |> List.length
        in
        Console.print [ Pp.textf "Negotiation 'have' lines sent: %d" have_count ];
        Dune_scheduler.Scheduler.cancel_current_build ()));
  (* With refs created by previous fetches, git can tell the server what it
     already has, avoiding redundant downloads. *)
  [%expect {| Negotiation 'have' lines sent: 3 |}]
;;
