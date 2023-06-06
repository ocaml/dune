open Stdune
module Fetch = Dune_pkg.Fetch
module Checksum = Dune_pkg.Checksum
module Lock_dir = Dune_pkg.Lock_dir
module Scheduler = Dune_engine.Scheduler
module Package_name = Dune_lang.Package_name

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

let calculate_checksum ~filename =
  OpamHash.compute filename |> Checksum.of_opam_hash

let wrong_checksum =
  OpamHash.compute_from_string "random content" |> Checksum.of_opam_hash

let download ~port ~filename ~target ?checksum () =
  let open Fiber.O in
  let url = url ~port ~filename in
  let* res = Fetch.fetch ~checksum ~target url in
  match res with
  | Error (Unavailable None) ->
    let errs = [ Pp.text "Failure while downloading" ] in
    User_error.raise ~loc:Loc.none errs
  | Error (Unavailable (Some msg)) ->
    User_error.raise ~loc:Loc.none [ User_message.pp msg ]
  | Error (Checksum_mismatch actual_checksum) ->
    let expected_checksum =
      match checksum with
      | Some v -> v
      | None -> assert false
    in
    User_error.raise ~loc:Loc.none
      [ Pp.text "Expected checksum was"
      ; Pp.text @@ Checksum.to_string expected_checksum
      ; Pp.text "but got"
      ; Pp.text @@ Checksum.to_string actual_checksum
      ]
  | Ok () ->
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

let random_port () =
  let state = Base.Random.State.make_self_init ~allow_in_tests:true () in
  (* ephemeral port range that is not assinged by IANA to prevent collisions *)
  Base.Random.State.int_incl state 49152 65535

let target destination =
  let ext = Path.External.of_filename_relative_to_initial_cwd destination in
  Path.external_ ext

let%expect_test "downloading simple file" =
  Dune_tests_common.init ();
  let filename = "plaintext.md" in
  let port = random_port () in
  let server = serve_once ~filename ~port () in
  let destination = "destination.md" in
  run
    (download ~port ~filename ~target:(target destination)
       ~checksum:(calculate_checksum ~filename));
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

let%expect_test "downloading but the checksums don't match" =
  Dune_tests_common.init ();
  let filename = "plaintext.md" in
  let port = random_port () in
  let server = serve_once ~filename ~port () in
  let destination = "destination.md" in
  run
    (download ~port ~filename ~target:(target destination)
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

let%expect_test "downloading, without any checksum" =
  Dune_tests_common.init ();
  let filename = "plaintext.md" in
  let port = random_port () in
  let server = serve_once ~filename ~port () in
  let destination = "destination.md" in
  run (download ~port ~filename ~target:(target destination));
  Thread.join server;
  print_endline "Finished successfully, no checksum verification";
  [%expect
    {|
    Done downloading
    Finished successfully, no checksum verification |}]

let lock_dir_encode_decode_round_trip_test ~lock_dir_path ~make_lock_dir =
  let lock_dir_path = Path.Source.of_string lock_dir_path in
  let lock_dir_original = make_lock_dir ~lock_dir_path in
  Lock_dir.write_disk ~lock_dir_path lock_dir_original;
  let lock_dir_round_tripped =
    Lock_dir.read_disk ~lock_dir_path |> Result.ok_exn
  in
  if Lock_dir.equal lock_dir_round_tripped lock_dir_original then
    print_endline "lockdir matches after roundtrip:"
  else print_endline "lockdir doesn't match after roundtrip:";
  print_endline (Lock_dir.to_dyn lock_dir_round_tripped |> Dyn.to_string)

let%expect_test "encode/decode round trip test for lockdir with no deps" =
  lock_dir_encode_decode_round_trip_test ~lock_dir_path:"empty_lock_dir"
    ~make_lock_dir:(fun ~lock_dir_path:_ ->
      Lock_dir.create_latest_version Package_name.Map.empty);
  [%expect
    {|
    lockdir matches after roundtrip:
    { version = (0, 1); packages = map {} } |}]

let%expect_test "encode/decode round trip test for lockdir with simple deps" =
  lock_dir_encode_decode_round_trip_test ~lock_dir_path:"simple_lock_dir"
    ~make_lock_dir:(fun ~lock_dir_path ->
      let mk_pkg_basic ~name ~version =
        let name = Package_name.of_string name in
        ( name
        , { Lock_dir.Pkg.build_command = None
          ; install_command = None
          ; deps = []
          ; info =
              { Lock_dir.Pkg_info.name; version; dev = false; source = None }
          ; lock_dir = lock_dir_path
          ; exported_env = []
          } )
      in
      Lock_dir.create_latest_version
        (Package_name.Map.of_list_exn
           [ mk_pkg_basic ~name:"foo" ~version:"0.1.0"
           ; mk_pkg_basic ~name:"bar" ~version:"0.2.0"
           ]));
  [%expect
    {|
    lockdir matches after roundtrip:
    { version = (0, 1)
    ; packages =
        map
          { "bar" :
              { build_command = None
              ; install_command = None
              ; deps = []
              ; info =
                  { name = "bar"; version = "0.2.0"; dev = false; source = None }
              ; lock_dir = In_source_tree "simple_lock_dir"
              ; exported_env = []
              }
          ; "foo" :
              { build_command = None
              ; install_command = None
              ; deps = []
              ; info =
                  { name = "foo"; version = "0.1.0"; dev = false; source = None }
              ; lock_dir = In_source_tree "simple_lock_dir"
              ; exported_env = []
              }
          }
    } |}]

let%expect_test "encode/decode round trip test for lockdir with complex deps" =
  let module Action = Dune_lang.Action in
  let module String_with_vars = Dune_lang.String_with_vars in
  lock_dir_encode_decode_round_trip_test ~lock_dir_path:"complex_lock_dir"
    ~make_lock_dir:(fun ~lock_dir_path ->
      let pkg_a =
        let name = Package_name.of_string "a" in
        ( name
        , { Lock_dir.Pkg.build_command =
              Some
                Action.(
                  Progn [ Echo [ String_with_vars.make_text Loc.none "hello" ] ])
          ; install_command =
              Some
                (Action.System
                   (* String_with_vars.t doesn't round trip so we have to set
                      [quoted] if the string would be quoted *)
                   (String_with_vars.make_text ~quoted:true Loc.none
                      "echo 'world'"))
          ; deps = []
          ; info =
              { Lock_dir.Pkg_info.name
              ; version = "0.1.0"
              ; dev = false
              ; source =
                  Some
                    (External_copy (Loc.none, Path.External.of_string "/tmp/a"))
              }
          ; lock_dir = lock_dir_path
          ; exported_env =
              [ { Action.Env_update.op = Eq
                ; var = "foo"
                ; value = String_with_vars.make_text Loc.none "bar"
                }
              ]
          } )
      in
      let pkg_b =
        let name = Package_name.of_string "b" in
        ( name
        , { Lock_dir.Pkg.build_command = None
          ; install_command = None
          ; deps = [ fst pkg_a ]
          ; info =
              { Lock_dir.Pkg_info.name
              ; version = "dev"
              ; dev = true
              ; source =
                  Some
                    (Fetch
                       { url = (Loc.none, "https://github.com/foo/b")
                       ; checksum =
                           Some
                             ( Loc.none
                             , Checksum.of_string
                                 "sha256=adfc38f14c0188a2ad80d61451d011d27ab8839b717492d7ad42f7cb911c54c3"
                             )
                       })
              }
          ; lock_dir = lock_dir_path
          ; exported_env = []
          } )
      in
      let pkg_c =
        let name = Package_name.of_string "c" in
        ( name
        , { Lock_dir.Pkg.build_command = None
          ; install_command = None
          ; deps = [ fst pkg_a; fst pkg_b ]
          ; info =
              { Lock_dir.Pkg_info.name
              ; version = "0.2"
              ; dev = false
              ; source =
                  Some
                    (Fetch
                       { url = (Loc.none, "https://github.com/foo/c")
                       ; checksum = None
                       })
              }
          ; lock_dir = lock_dir_path
          ; exported_env = []
          } )
      in
      Lock_dir.create_latest_version
        (Package_name.Map.of_list_exn [ pkg_a; pkg_b; pkg_c ]));
  [%expect
    {|
    lockdir matches after roundtrip:
    { version = (0, 1)
    ; packages =
        map
          { "a" :
              { build_command = Some [ "progn"; [ "echo"; "hello" ] ]
              ; install_command = Some [ "system"; "echo 'world'" ]
              ; deps = []
              ; info =
                  { name = "a"
                  ; version = "0.1.0"
                  ; dev = false
                  ; source = Some External_copy External "/tmp/a"
                  }
              ; lock_dir = In_source_tree "complex_lock_dir"
              ; exported_env = [ { op = "="; var = "foo"; value = "bar" } ]
              }
          ; "b" :
              { build_command = None
              ; install_command = None
              ; deps = [ "a" ]
              ; info =
                  { name = "b"
                  ; version = "dev"
                  ; dev = true
                  ; source =
                      Some
                        Fetch
                          "https://github.com/foo/b",Some
                                                       "sha256=adfc38f14c0188a2ad80d61451d011d27ab8839b717492d7ad42f7cb911c54c3"
                  }
              ; lock_dir = In_source_tree "complex_lock_dir"
              ; exported_env = []
              }
          ; "c" :
              { build_command = None
              ; install_command = None
              ; deps = [ "a"; "b" ]
              ; info =
                  { name = "c"
                  ; version = "0.2"
                  ; dev = false
                  ; source = Some Fetch "https://github.com/foo/c",None
                  }
              ; lock_dir = In_source_tree "complex_lock_dir"
              ; exported_env = []
              }
          }
    } |}]
