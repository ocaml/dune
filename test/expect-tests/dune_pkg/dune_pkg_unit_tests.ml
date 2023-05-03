open Stdune
module Fetch = Dune_pkg.Fetch
module Scheduler = Dune_engine.Scheduler

let serve_once ~filename ~port () =
  let host = Unix.inet_addr_loopback in
  let addr = Unix.ADDR_INET (host, port) in
  let sock = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock addr;
  Unix.listen sock 5;
  Thread.create
    (fun sock ->
      let descr, _sockaddr = Unix.accept sock in
      let content = Io.String_path.read_file filename in
      let content_length = String.length content in
      let write_end = Unix.out_channel_of_descr descr in
      Printf.fprintf write_end {|HTTP/1.1 200 Ok
Content-Length: %d

%s|}
        content_length content;
      close_out write_end)
    sock

let url ~port ~filename =
  let localhost = Unix.inet_addr_loopback |> Unix.string_of_inet_addr in
  Format.sprintf "http://%s:%d/%s" localhost port filename |> OpamUrl.of_string

let download ~port ~filename ~target () =
  let open Fiber.O in
  let url = url ~port ~filename in
  let* () = Fetch.fetch Loc.none url ~target in
  print_endline "Done downloading";
  Fiber.return ()

let run thunk =
  let on_event _config _event = () in
  let config : Scheduler.Config.t =
    { concurrency = 1
    ; stats = None
    ; insignificant_changes = `Ignore
    ; signal_watcher = `No
    ; watch_exclusions = []
    }
  in
  Scheduler.Run.go config ~on_event thunk

let random_port state =
  (* ephemeral port range that is not assinged by IANA to prevent collisions *)
  Base.Random.State.int_incl state 49152 65535

let%expect_test "downloading simple file" =
  (* needed to get the testing machinery working *)
  Dune_tests_common.init ();
  let rng = Base.Random.State.make_self_init ~allow_in_tests:true () in
  let port = random_port rng in
  let filename = "plaintext.md" in
  let server = serve_once ~filename ~port () in
  let destination = "downloaded.md" in
  let ext = Path.External.of_filename_relative_to_initial_cwd destination in
  let target = Path.external_ ext in
  run (download ~port ~filename ~target);
  Thread.join server;
  let served_content = Io.String_path.read_file filename in
  let downloaded_content = Io.String_path.read_file destination in
  Printf.printf "Served file:\n%s\nDownloaded file:\n%s\nEqual: %B"
    served_content downloaded_content
    (String.equal served_content downloaded_content);
  [%expect
    {|
    Done downloading
    Served file:
    Plaintext
    =========

    This is a plaintext file to make sure that downloading files from the internal
    webserver works as desired.

    Downloaded file:
    Plaintext
    =========

    This is a plaintext file to make sure that downloading files from the internal
    webserver works as desired.

    Equal: true |}]
