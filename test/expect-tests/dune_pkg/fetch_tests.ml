open Stdune
module Scheduler = Dune_engine.Scheduler
module Checksum = Dune_pkg.Checksum
module Fetch = Dune_pkg.Fetch

let plaintext_md = "tar-inputs/plaintext.md"
let () = Dune_tests_common.init ()

let url ~port ~filename =
  let localhost = Unix.inet_addr_loopback |> Unix.string_of_inet_addr in
  Format.sprintf "http://%s:%d/%s" localhost port filename |> OpamUrl.of_string
;;

let calculate_checksum ~filename = OpamHash.compute filename |> Checksum.of_opam_hash

let wrong_checksum =
  OpamHash.compute_from_string "random content" |> Checksum.of_opam_hash
;;

let archive = "tarball.tar.gz"

let subdir destination =
  let ext = Path.External.of_filename_relative_to_initial_cwd destination in
  Path.external_ ext
;;

let serve_once ~filename =
  let host = Unix.inet_addr_loopback in
  let addr = Unix.ADDR_INET (host, 0) in
  let server = Http.Server.make addr in
  Http.Server.start server;
  let port = Http.Server.port server in
  let thread =
    Thread.create
      (fun server ->
        Http.Server.accept server ~f:(fun write_end ->
          Http.Server.respond_file write_end ~file:filename);
        Http.Server.stop server)
      server
  in
  port, thread
;;

let download ?(reproducible = true) ~unpack ~port ~filename ~target ?checksum () =
  let open Fiber.O in
  let url = url ~port ~filename in
  let* res = Fetch.fetch ~unpack ~checksum ~target ~url:(Loc.none, url) in
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
    { concurrency = 1; stats = None; print_ctrl_c_warning = false; watch_exclusions = [] }
  in
  Scheduler.Run.go config ~on_event thunk
;;

let%expect_test "downloading simple file" =
  let filename = plaintext_md in
  let port, server = serve_once ~filename in
  let destination = "destination.md" in
  run
    (download
       ~unpack:false
       ~port
       ~filename:""
       ~target:(subdir destination)
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
  let port, server = serve_once ~filename:plaintext_md in
  let destination = "destination.md" in
  run
    (download
       ~unpack:false
       ~port
       ~filename:""
       ~target:(subdir destination)
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
  let port, server = serve_once ~filename:plaintext_md in
  let destination = "destination.md" in
  run (download ~unpack:false ~port ~filename:"" ~target:(subdir destination));
  Thread.join server;
  print_endline "Finished successfully, no checksum verification";
  [%expect {|
    Done downloading
    Finished successfully, no checksum verification |}]
;;

let%expect_test "downloading, tarball" =
  let port, server = serve_once ~filename:archive in
  let destination = "tarball" in
  run
    (download
     (* the tar utility that produces [filename] isn't portable and/or
        deterministic enough to print the actual checksum *)
       ~reproducible:false
       ~unpack:true
       ~checksum:wrong_checksum
       ~port
       ~filename:""
       ~target:(subdir destination));
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

let%expect_test "downloading, tarball with no checksum match" =
  (* This test ensures that the contents of the extracted tarball are in the
     correct location. *)
  let port, server = serve_once ~filename:archive in
  let target = subdir "tarball" in
  run (download ~reproducible:false ~unpack:true ~port ~filename:"" ~target);
  Thread.join server;
  print_endline "Finished successfully, no checksum verification";
  (* print all the files in the target directory *)
  let () =
    print_endline "------\nfiles in target dir:";
    Dune_engine.No_io.Path.Untracked.readdir_unsorted target
    |> Result.value ~default:[]
    |> List.sort ~compare:String.compare
    |> List.iter ~f:print_endline
  in
  [%expect
    {|
    Done downloading
    Finished successfully, no checksum verification
    ------
    files in target dir:
    file2.md
    plaintext.md |}]
;;

let download_git rev_store url ~target =
  let open Fiber.O in
  Fetch.fetch_git rev_store ~target ~url:(Loc.none, url)
  >>| function
  | Error _ ->
    let errs = [ Pp.text "Failure while downloading" ] in
    User_error.raise ~loc:Loc.none errs
  | Ok () -> ()
;;

let%expect_test "downloading via git" =
  let source = subdir "source-repository" in
  let url = OpamUrl.parse (sprintf "git+file://%s" (Path.to_string source)) in
  let rev_store_dir = subdir "rev-store" in
  let target = subdir "checkout-into-here" in
  (* The file at [entry] is created by [create_repo_at] *)
  let entry = Path.relative target "entry" in
  Path.mkdir_p target;
  run (fun () ->
    let open Fiber.O in
    let* rev_store = Dune_pkg.Rev_store.load_or_create ~dir:rev_store_dir in
    let* (_commit : string) = Rev_store_tests.create_repo_at source in
    let+ () = download_git rev_store url ~target in
    print_endline (Io.read_file entry));
  [%expect {| just some content |}]
;;

let%expect_test "attempting to download an invalid git url" =
  let source = subdir "source" in
  let url = OpamUrl.parse "git+file://foo/bar" in
  let rev_store_dir = subdir "rev-store-dir" in
  let target = subdir "target" in
  let entry = Path.relative target "e" in
  run (fun () ->
    let open Fiber.O in
    let* rev_store = Dune_pkg.Rev_store.load_or_create ~dir:rev_store_dir in
    let* (_commit : string) = Rev_store_tests.create_repo_at source in
    let+ () = download_git rev_store url ~target in
    print_endline (Io.read_file entry));
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (Dune_util__Report_error.Already_reported)
  Trailing output
  ---------------
  fatal: '/bar' does not appear to be a git repository
  fatal: Could not read from remote repository.

  Please make sure you have the correct access rights
  and the repository exists.
  Error: Failed to run external command:
  'git ls-remote "file://foo/bar"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file://foo/bar" |}]
;;
