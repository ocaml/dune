open Stdune
module Scheduler = Dune_engine.Scheduler
module Checksum = Dune_pkg.Checksum
module Fetch = Dune_pkg.Fetch

let () = Dune_tests_common.init ()

let url ~port ~filename =
  let localhost = Unix.inet_addr_loopback |> Unix.string_of_inet_addr in
  Format.sprintf "http://%s:%d/%s" localhost port filename |> OpamUrl.of_string
;;

let calculate_checksum ~filename = OpamHash.compute filename |> Checksum.of_opam_hash

let wrong_checksum =
  OpamHash.compute_from_string "random content" |> Checksum.of_opam_hash
;;

let target destination =
  let ext = Path.External.of_filename_relative_to_initial_cwd destination in
  Path.external_ ext
;;

let serve_once ~filename =
  let host = Unix.inet_addr_loopback in
  let addr = Unix.ADDR_INET (host, 0) in
  let sock = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock addr;
  Unix.listen sock 5;
  let port =
    match Unix.getsockname sock with
    | Unix.ADDR_INET (_, port) -> port
    | ADDR_UNIX _ -> assert false
  in
  let thread =
    Thread.create
      (fun sock ->
        let descr, _sockaddr = Unix.accept sock in
        let content = Io.String_path.read_file filename in
        let content_length = String.length content in
        let write_end = Unix.out_channel_of_descr descr in
        Printf.fprintf
          write_end
          {|HTTP/1.1 200 Ok
Content-Length: %d

%s|}
          content_length
          content;
        close_out write_end)
      sock
  in
  port, thread
;;

let download ?(reproducible = true) ~unpack ~port ~filename ~target ?checksum () =
  let open Fiber.O in
  let url = url ~port ~filename in
  let* res = Fetch.fetch ~unpack ~checksum ~target url in
  match res with
  | Error (Unavailable None) ->
    let errs = [ Pp.text "Failure while downloading" ] in
    User_error.raise ~loc:Loc.none errs
  | Error (Unavailable (Some msg)) ->
    User_error.raise ~loc:Loc.none [ User_message.pp msg ]
  | Error (Checksum_mismatch actual_checksum) ->
    let expected_checksum = Option.value_exn checksum in
    User_error.raise
      ~loc:Loc.none
      [ Pp.text "Expected checksum was"
      ; Pp.verbatim @@ Checksum.to_string expected_checksum
      ; Pp.text "but got"
      ; (if reproducible
         then Pp.verbatim @@ Checksum.to_string actual_checksum
         else Pp.text "<REDACTED>")
      ]
  | Ok () ->
    print_endline "Done downloading";
    Fiber.return ()
;;

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
;;

let%expect_test "downloading simple file" =
  let filename = "plaintext.md" in
  let port, server = serve_once ~filename in
  let destination = "destination.md" in
  run
    (download
       ~unpack:false
       ~port
       ~filename
       ~target:(target destination)
       ~checksum:(calculate_checksum ~filename));
  Thread.join server;
  let served_content = Io.String_path.read_file filename in
  let downloaded_content = Io.String_path.read_file destination in
  Printf.printf
    "Served file:\n%s\nDownloaded file:\n%s\nEqual: %B"
    served_content
    downloaded_content
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
;;

let%expect_test "downloading but the checksums don't match" =
  let filename = "plaintext.md" in
  let port, server = serve_once ~filename in
  let destination = "destination.md" in
  run
    (download
       ~unpack:false
       ~port
       ~filename
       ~target:(target destination)
       ~checksum:wrong_checksum);
  Thread.join server;
  print_endline "Finished successfully?";
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (Dune_util__Report_error.Already_reported)
  Trailing output
  ---------------
  Error: Expected checksum was
  md5=c533195dc4253503071a19d42f08e877
  but got
  md5=cbe78b067d4739684e86edfd2cb518bd |}]
;;

let%expect_test "downloading, without any checksum" =
  let filename = "plaintext.md" in
  let port, server = serve_once ~filename in
  let destination = "destination.md" in
  run (download ~unpack:false ~port ~filename ~target:(target destination));
  Thread.join server;
  print_endline "Finished successfully, no checksum verification";
  [%expect {|
    Done downloading
    Finished successfully, no checksum verification |}]
;;

let%expect_test "downloading, tarball" =
  let filename = "tarball.tar.gz" in
  let port, server = serve_once ~filename in
  let destination = "tarball" in
  run
    (download
     (* the tar utility that produces [filename] isn't portable and/or
        deterministic enough to print the actual checksum *)
       ~reproducible:false
       ~unpack:true
       ~checksum:wrong_checksum
       ~port
       ~filename
       ~target:(target destination));
  Thread.join server;
  print_endline "Finished successfully, no checksum verification";
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (Dune_util__Report_error.Already_reported)
  Trailing output
  ---------------
  Error: Expected checksum was
  md5=c533195dc4253503071a19d42f08e877
  but got
  <REDACTED> |}]
;;
