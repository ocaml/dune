open Import
open Fiber.O
open Git_test_utils
module Rev_store = Dune_pkg.Rev_store
module Opam_repo = Dune_pkg.Opam_repo
module Console = Dune_console

let () =
  Dune_util.override_xdg
    (let env =
       Env.update Env.initial ~var:"XDG_CACHE_HOME" ~f:(fun _ ->
         Some (Path.of_filename_relative_to_initial_cwd ".cache" |> Path.to_string))
       |> Env.get
     in
     Xdg.create ~env ());
  Config.init String.Map.empty;
  Dune_tests_common.init ()
;;

let%expect_test "fetching non-existent object twice returns consistent results" =
  (* This test checks that fetching a non-existent object twice returns
     consistent errors. *)
  let dir = Temp.create Dir ~prefix:"git-repo-" ~suffix:"" in
  let rev_store = run (fun () -> Rev_store.get) in
  run (fun () -> create_repo_at dir >>| ignore);
  let remote =
    Rev_store.remote
      rev_store
      ~loc:Loc.none
      ~url:(Path.to_string dir |> OpamUrl.parse |> OpamUrl.to_string)
  in
  (* A SHA that definitely doesn't exist *)
  let fake_sha = "0000000000000000000000000000000000000000" in
  let fetch_fake () =
    Rev_store.Object.of_sha1 fake_sha
    |> Option.value_exn
    |> Rev_store.fetch_object rev_store remote
    >>| function
    | Ok _ -> Console.print [ Pp.text "fetch returned Ok (unexpected)" ]
    | Error _ -> Console.print [ Pp.text "fetch returned Error (expected)" ]
  in
  (* First attempt should fail *)
  run (fun () -> fetch_fake ());
  [%expect {| fetch returned Error (expected) |}];
  (* Second attempt should also fail *)
  run (fun () -> fetch_fake ());
  [%expect {| fetch returned Error (expected) |}]
;;

let%expect_test "adding remotes" =
  let dir = Temp.create Dir ~prefix:"git-repo-" ~suffix:"" in
  run (fun () ->
    let* rev_store = Rev_store.get in
    let* _head = create_repo_at dir in
    let opam_url = Path.to_string dir |> OpamUrl.parse in
    Dune_pkg.OpamUrl.resolve opam_url ~loc:Loc.none rev_store
    >>= function
    | Error _ -> Fiber.return @@ print_endline "Unable to find revision"
    | Ok r ->
      print_endline "Successfully found remote";
      Dune_pkg.OpamUrl.fetch_revision opam_url ~loc:Loc.none r rev_store
      >>| (function
       | Error _ -> print_endline "Unable to fetch revision"
       | Ok _ -> print_endline "successfully fetched revision"));
  [%expect
    {|
    Successfully found remote
    successfully fetched revision
     |}]
;;

let%expect_test "fetching an object twice from the store" =
  Rev_store.Debug.files_and_submodules_cache := true;
  Rev_store.Debug.content_of_files_cache := true;
  let dir = Temp.create Dir ~prefix:"git-repo-" ~suffix:"" in
  let write dir file content = Io.write_lines (Path.relative dir file) content in
  (* Initialise revision store *)
  let rev_store = run (fun () -> Rev_store.get) in
  let remote =
    Rev_store.remote
      rev_store
      ~loc:Loc.none
      ~url:(Path.to_string dir |> OpamUrl.parse |> OpamUrl.to_string)
  in
  (* Initialise remote repository *)
  let () = run (fun () -> create_repo_at dir >>| ignore) in
  let get_remote_head dir = git_out ~dir [ "rev-parse"; "HEAD" ] in
  let add_file_to_remote commit_msg dir file contents =
    write dir file contents;
    git ~dir [ "add"; file ] >>> git ~dir [ "commit"; sprintf "-m '%s'" commit_msg ]
  in
  let get_file remote_revision file expected_file_content =
    let* at_rev =
      Rev_store.Object.of_sha1 remote_revision
      |> Option.value_exn
      |> Rev_store.fetch_object rev_store remote
      >>| Result.to_option
    in
    let at_rev = Option.value_exn at_rev in
    Rev_store.At_rev.directory_entries at_rev ~recursive:true Path.Local.root
    |> Rev_store.File.Set.find ~f:(fun f ->
      Path.Local.equal (Rev_store.File.path f) (Path.Local.of_string file))
    |> Option.to_list
    |> Rev_store.content_of_files rev_store
    >>| function
    | [ content ] ->
      (* If files don't end in a newline, Git may insert a trailing new line
         anyway. Therefore in order to compare file contents, we make sure we
         strip trailing new lines if they exist. *)
      assert (
        String.equal
          (String.drop_suffix_if_exists ~suffix:"\n" expected_file_content)
          (String.drop_suffix_if_exists ~suffix:"\n" content))
    | _ -> assert false
  in
  (* Getting the file twice misses the cache on the first time, but hits on the
     second in both caches. *)
  let file_A = "file_A" in
  let file_A_content = "this is file A" in
  run (fun () ->
    let* () = add_file_to_remote "added file A" dir file_A [ file_A_content ] in
    let* remote_revision = get_remote_head dir in
    get_file remote_revision file_A file_A_content
    >>> get_file remote_revision file_A file_A_content);
  [%expect
    {|
    ("files_and_submodules", [ ("cached", None) ])
    ("contents_of_files",
     [ ("files", [ Direct { path = "file_A"; size = 15; hash = Sha1 <opaque> } ])
     ; ("cached", map {})
     ])
    ("files_and_submodules", [ ("cached", Some <opaque>) ])
    ("contents_of_files",
     [ ("files", [ Direct { path = "file_A"; size = 15; hash = Sha1 <opaque> } ])
     ; ("cached", map { Sha1 <opaque> : "this is file A\n\
                                         " })
     ])
    |}];
  (* Now we check that updating the repository doesn't invalidate our cache. *)
  let file_B = "file_B" in
  let file_B_content = "this is file B" in
  run (fun () ->
    let* () = add_file_to_remote "added file B" dir file_B [ file_B_content ] in
    let* remote_revision = get_remote_head dir in
    Fiber.sequential_iter
      ~f:Fun.id
      [ get_file remote_revision file_A file_A_content
      ; get_file remote_revision file_A file_A_content
      ; get_file remote_revision file_B file_B_content
      ; get_file remote_revision file_B file_B_content
      ]);
  (* Here we expect the following in each case:

     1. file_A: The [files_and_submodules] cache will miss, since the contents
        of the files have changed. The [contents_of_files] cache will however hit,
        since the contents of file_A haven't changed.

     2. file_A: Both the [files_and_submodules] and the [contents_of_files]
        caches will hit.

     3. file_B: The [files_and_submodules] cache will hit, however the
        [contents_of_files] cache will miss since we haven't seen file_B before.

     4. file_B: Both the [files_and_submodules] and the [contents_of_files]
        caches will hit.
  *)
  [%expect
    {|
    ("files_and_submodules", [ ("cached", None) ])
    ("contents_of_files",
     [ ("files", [ Direct { path = "file_A"; size = 15; hash = Sha1 <opaque> } ])
     ; ("cached", map { Sha1 <opaque> : "this is file A\n\
                                         " })
     ])
    ("files_and_submodules", [ ("cached", Some <opaque>) ])
    ("contents_of_files",
     [ ("files", [ Direct { path = "file_A"; size = 15; hash = Sha1 <opaque> } ])
     ; ("cached", map { Sha1 <opaque> : "this is file A\n\
                                         " })
     ])
    ("files_and_submodules", [ ("cached", Some <opaque>) ])
    ("contents_of_files",
     [ ("files", [ Direct { path = "file_B"; size = 15; hash = Sha1 <opaque> } ])
     ; ("cached", map {})
     ])
    ("files_and_submodules", [ ("cached", Some <opaque>) ])
    ("contents_of_files",
     [ ("files", [ Direct { path = "file_B"; size = 15; hash = Sha1 <opaque> } ])
     ; ("cached", map { Sha1 <opaque> : "this is file B\n\
                                         " })
     ])
    |}];
  (* Checking for the presence of the lmdb database. *)
  Console.print
    [ (let path = Path.External.cwd () |> Path.external_ in
       Path.L.relative path [ ".cache"; "dune"; "rev_store" ]
       |> Path.to_string
       |> Sys.readdir
       |> Array.to_list
       |> List.sort ~compare:String.compare
       |> Dyn.list Dyn.string
       |> Dyn.pp)
    ];
  [%expect {| [ "data.mdb"; "lock.mdb" ] |}]
;;
