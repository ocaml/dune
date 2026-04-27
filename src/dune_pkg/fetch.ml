open Import
open Fiber.O

module Curl = struct
  let bin =
    lazy
      (match Bin.which ~path:(Env_path.path Env.initial) "curl" with
       | Some p -> p
       | None ->
         let curl = User_message.command "curl" in
         let sep = Pp.space in
         User_error.raise
           ~hints:
             [ Pp.concat
                 ~sep
                 [ Pp.text "Install"; curl; Pp.text "with your system package manager." ]
             ]
           [ Pp.concat
               ~sep
               [ Pp.text "The program"
               ; curl
               ; Pp.text "does not appear to be installed. Dune uses"
               ; curl
               ; Pp.text "to download packages. Dune requires that the"
               ; curl
               ; Pp.text
                   "executable be located in one of the directories listed in the PATH \
                    variable."
               ]
           ])
  ;;

  let user_agent =
    lazy
      (let base = "dune" in
       match Build_info.V1.version () with
       | None -> base
       | Some v -> base ^ "." ^ Build_info.V1.Version.to_string v)
  ;;

  let curl_features_regex =
    (* If these features are present, then --compressed is supported *)
    let features = [ "libz"; "brotli"; "zstd" ] in
    Re.compile
      Re.(seq [ bol; str "Features:"; rep (first (alt (List.map ~f:str features))) ])
  ;;

  let compressed_supported =
    (* We check if curl supports --compressed by running curl -V and checking if
       the output contains the features we need. *)
    Fiber.Lazy.create (fun () ->
      let+ lines, _ =
        let stderr_to =
          Process.Io.make_stderr
            ~output_on_success:Swallow
            ~output_limit:Dune_engine.Execution_parameters.Action_output_limit.default
        in
        Process.run_capture_lines
          Return
          ~stderr_to
          ~display:Quiet
          (Lazy.force bin)
          [ "-V" ]
      in
      match List.find_map lines ~f:(Re.exec_opt curl_features_regex) with
      | Some group -> Re.Group.test group 0
      | None -> false)
  ;;

  let run ~url ~temp_dir ~output =
    let* compressed_supported = Fiber.Lazy.force compressed_supported in
    let args =
      List.flatten
        [ [ "-L"
          ; "-s"
          ; "--user-agent"
          ; Lazy.force user_agent
          ; "--write-out"
          ; "\"%{http_code}\"" (* This arg must be quoted to work on windows *)
          ; "-o"
          ; Path.to_string output
          ]
        ; (if compressed_supported then [ "--compressed" ] else [])
        ; [ "--"; url ]
        ]
    in
    let stderr = Path.relative temp_dir "curl.stderr" in
    let+ http_code, exit_code =
      let stderr_to = Process.Io.file stderr Out in
      Process.run_capture_line
        Return
        ~stderr_to
        ~display:!Dune_engine.Clflags.display
        (Lazy.force bin)
        args
    in
    if exit_code <> 0
    then (
      let stderr =
        match Io.read_file stderr with
        | s ->
          Fpath.unlink_no_err (Path.to_string stderr);
          [ Pp.text s ]
        | exception s ->
          [ Pp.textf
              "Failed to read stderr from file %s"
              (Path.to_string_maybe_quoted stderr)
          ; Exn.pp s
          ]
      in
      Error
        (User_message.make
           ([ Pp.concat
                ~sep:Pp.space
                [ User_message.command "curl"
                ; Pp.textf "returned an invalid error code %d" exit_code
                ]
            ]
            @ stderr)))
    else (
      Fpath.unlink_no_err (Path.to_string stderr);
      match
        let open Option.O in
        let suffix = {|"|} in
        let prefix = suffix in
        String.drop_prefix_and_suffix ~prefix ~suffix http_code >>= Int.of_string
      with
      | None ->
        Error
          (User_message.make
             [ Pp.concat
                 ~sep:Pp.space
                 [ User_message.command "curl"
                 ; Pp.textf "returned an HTTP code we don't understand: %S" http_code
                 ]
             ])
      | Some http_code ->
        if http_code = 200
        then Ok ()
        else
          Error (User_message.make [ Pp.textf "Download failed with code %d" http_code ]))
  ;;
end

type failure =
  | Checksum_mismatch of Checksum.t
  | Unavailable of User_message.t option

let unpack_archive ~archive_driver ~target ~archive =
  Archive_driver.extract archive_driver ~archive ~target
  >>| Result.map_error ~f:(fun () ->
    Pp.textf "Unable to extract %s" (Path.to_string_maybe_quoted archive))
;;

let check_checksum checksum path =
  let checksum_error =
    match checksum with
    | None -> None
    | Some expected ->
      OpamHash.mismatch (Path.to_string path) (Checksum.to_opam_hash expected)
  in
  match checksum_error with
  | Some c -> Error (Checksum_mismatch (Checksum.of_opam_hash c))
  | None -> Ok ()
;;

let with_download url checksum ~target ~f =
  let url = OpamUrl.to_string url in
  let temp_dir =
    let prefix = "dune" in
    let suffix = Filename.basename url in
    Temp_dir.dir_for_target ~target ~prefix ~suffix
  in
  let output = Path.relative temp_dir "download" in
  Fiber.finalize ~finally:(fun () ->
    Temp.destroy Dir temp_dir;
    Fiber.return ())
  @@ fun () ->
  Curl.run ~temp_dir ~url ~output
  >>= function
  | Error message -> Fiber.return @@ Error (Unavailable (Some message))
  | Ok () ->
    (match check_checksum checksum output with
     | Ok () -> f output
     | Error _ as e -> Fiber.return e)
;;

let fetch_curl ~unpack:unpack_flag ~checksum ~target (url : OpamUrl.t) =
  with_download url checksum ~target ~f:(fun output ->
    match unpack_flag with
    | false ->
      Unix.rename (Path.to_string output) (Path.to_string target);
      Fiber.return @@ Ok ()
    | true ->
      unpack_archive
        ~archive_driver:
          (Archive_driver.choose_for_filename_default_to_tar (OpamUrl0.to_string url))
        ~target
        ~archive:output
      >>| (function
       | Ok () -> Ok ()
       | Error msg ->
         let exn =
           User_message.make
             [ Pp.textf
                 "Failed to unpack archive downloaded from %s"
                 (OpamUrl.to_string url)
             ; Pp.text "Reason:"
             ; msg
             ]
         in
         Error (Unavailable (Some exn))))
;;

let fetch_git rev_store ~target ~url:(url_loc, url) =
  OpamUrl.resolve url ~loc:url_loc rev_store
  >>= (function
   | Error _ as e -> Fiber.return e
   | Ok r -> OpamUrl.fetch_revision url ~loc:url_loc r rev_store)
  >>= function
  | Error msg -> Fiber.return @@ Error (Unavailable (Some msg))
  | Ok at_rev ->
    let+ res = Rev_store.At_rev.check_out at_rev ~target in
    Ok res
;;

let fetch_local ~checksum ~target (url, url_loc) =
  if not (OpamUrl.is_local url)
  then Code_error.raise "fetch_local: url should be file://" [ "url", OpamUrl.to_dyn url ];
  let path =
    match OpamUrl.classify url url_loc with
    | `Path p -> p
    | `Git | `Archive ->
      Code_error.raise "fetch_local: not a path" [ "url", OpamUrl.to_dyn url ]
  in
  match check_checksum checksum path with
  | Error _ as e -> Fiber.return e
  | Ok () ->
    let+ unpack_result =
      unpack_archive
        ~archive_driver:
          (Archive_driver.choose_for_filename_default_to_tar (OpamUrl0.to_string url))
        ~target
        ~archive:path
    in
    Result.map_error unpack_result ~f:(fun pp ->
      Unavailable (Some (User_message.make [ Pp.text "Could not unpack:"; pp ])))
;;

let is_descendant t ~of_ =
  let is_desc_e t ~of_ =
    let open Path.External in
    is_root of_
    || equal of_ t
    || String.starts_with ~prefix:(to_string of_ ^ "/") (to_string t)
  in
  match t, of_ with
  | Path.External t, Path.External of_ -> is_desc_e t ~of_
  | _ -> Path.is_descendant t ~of_
;;

let resolve_directory_symlinks_in root =
  let follow_symlink_exn name =
    match Fpath.follow_symlink name with
    | Ok resolved -> Some resolved
    | Error (Unix_error _) -> None
    | Error Not_a_symlink ->
      Code_error.raise
        "resolve_directory_symlinks_in: not a symlink"
        [ "name", Dyn.string name ]
    | Error Max_depth_exceeded ->
      User_error.raise
        [ Pp.textf "Unable to resolve symlink %s: too many levels of symbolic links" name
        ]
  in
  let cycle_error name =
    User_error.raise
      [ Pp.textf "Unable to resolve symlink %s, it is part of a cycle." name ]
  in
  let rec resolve_rec dir seen =
    match Readdir.read_directory_with_kinds (Path.to_string dir) with
    | Error e -> Unix_error.Detailed.raise e
    | Ok entries ->
      let sorted_entries =
        List.sort
          ~compare:(fun (name1, _k1) (name2, _k2) -> String.compare name1 name2)
          entries
      in
      List.fold_left sorted_entries ~init:seen ~f:(fun seen (name, kind) ->
        let relative = Path.relative dir name in
        let full_name = Path.to_string relative in
        match (kind : Unix.file_kind) with
        | S_DIR -> resolve_rec relative seen
        | S_LNK ->
          if String.Set.mem seen full_name then cycle_error full_name;
          let seen = String.Set.add seen full_name in
          (match follow_symlink_exn full_name with
           | None ->
             (* Delete any broken symlinks from the unpacked archive. Dune can't
                handle broken symlinks in the _build directory, but some opam
                package contain broken symlinks. The logic here is applied to the
                contents of package source archives but not to packages whose source
                is in a local directory (e.g. when a package is pinned from the
                filesystem). Broken symlinks are excluded while copying files from
                local directories into the build directory, and the logic for
                excluding them lives in [Pkg_rules.source_rules]. *)
             Fpath.unlink_no_err full_name;
             Log.info
               "Deleted broken symlink from fetched archive"
               [ "full_name", Dyn.string full_name ];
             seen
           | Some raw_resolved ->
             (* [raw_resolved] is a relative build path but it might contain indirections,
                something like _build/foo/../bar
                or _build/../outside *)
             let canon_resolved = Path.of_string raw_resolved in
             if is_descendant relative ~of_:canon_resolved then cycle_error full_name;
             if not (is_descendant canon_resolved ~of_:root)
             then
               User_error.raise
                 [ Pp.textf
                     "Unable to resolve symlink %s: its target %s is outside the source \
                      directory"
                     full_name
                     (Path.to_string canon_resolved)
                 ];
             (match Unix.stat raw_resolved with
              | { Unix.st_kind = S_DIR; _ } ->
                Fpath.unlink_exn full_name;
                (match Fpath.mkdir_p full_name with
                 | `Created -> ()
                 | `Already_exists ->
                   User_error.raise
                     [ Pp.textf
                         "Unable to resolve symlink %s: a directory with the same name \
                          already exists."
                         full_name
                     ]);
                (match
                   Readdir.read_directory_with_kinds (Path.to_string canon_resolved)
                 with
                 | Error e -> Unix_error.Detailed.raise e
                 | Ok children ->
                   let symlinks_in_children =
                     List.fold_left
                       children
                       ~init:false
                       ~f:(fun symlinks_in_children (child_name, child_kind) ->
                         let child_path = Filename.concat raw_resolved child_name in
                         let src = Path.of_string child_path in
                         let dst =
                           Path.of_string (Filename.concat full_name child_name)
                         in
                         match (child_kind : Unix.file_kind) with
                         | S_REG ->
                           Io.portable_hardlink ~src ~dst;
                           symlinks_in_children
                         | S_DIR ->
                           Io.portable_symlink ~src ~dst;
                           true
                         | S_LNK ->
                           follow_symlink_exn child_path
                           |> Option.iter ~f:(fun linked_path ->
                             let src = Path.of_string linked_path in
                             Io.portable_symlink ~src ~dst);
                           true
                         | _ -> symlinks_in_children)
                   in
                   if symlinks_in_children then resolve_rec relative seen else seen)
              | _ ->
                (* We do not care about symlinks pointing to anything but directories. *)
                seen))
        | _ -> seen)
  in
  let _symlinks_seen : String.Set.t = resolve_rec root String.Set.empty in
  ()
;;

let fetch ~unpack ~checksum ~target ~url:(url_loc, url) =
  let event =
    Dune_trace.(
      Out.start (global ()) (fun () ->
        Dune_trace.Event.Async.fetch
          ~url:(OpamUrl.to_string url)
          ~target
          ~checksum:(Option.map ~f:Checksum.to_string checksum)))
  in
  let unsupported_backend s =
    User_error.raise ~loc:url_loc [ Pp.textf "Unsupported backend: %s" s ]
  in
  Fiber.finalize
    ~finally:(fun () ->
      Option.iter (Dune_trace.global ()) ~f:(fun trace ->
        Dune_trace.Out.finish trace event);
      Fiber.return ())
    (fun () ->
       let+ fetch_result =
         match url.backend with
         | `git ->
           let* rev_store = Rev_store.get in
           fetch_git rev_store ~target ~url:(url_loc, url)
         | `http -> fetch_curl ~unpack ~checksum ~target url
         | `rsync ->
           if not unpack
           then
             Code_error.raise
               "fetch_local: unpack is not set"
               [ "url", OpamUrl.to_dyn url ];
           fetch_local ~checksum ~target (url, url_loc)
         | `hg -> unsupported_backend "mercurial"
         | `darcs -> unsupported_backend "darcs"
       in
       match fetch_result with
       | Ok () ->
         let target_str = Path.to_string target in
         if (Unix.lstat target_str).st_kind = S_DIR
         then resolve_directory_symlinks_in target;
         Ok ()
       | Error e -> Error e)
;;

let fetch_without_checksum ~unpack ~target ~url =
  fetch ~unpack ~checksum:None ~url ~target
  >>| function
  | Ok () -> Ok ()
  | Error (Checksum_mismatch _) -> assert false
  | Error (Unavailable message) -> Error message
;;

let%test_module "resolve symlink tests" =
  (module struct
    let () =
      Printexc.record_backtrace true;
      Path.set_root (Path.External.cwd ());
      Path.Build.set_build_dir (Path.Outside_build_dir.of_string "_build");
      Log.init No_log_file
    ;;

    (** Prints the directory tree rooted at [root] in sorted order.
       - files: "path [file]"
       - hardlinks: "path [hardlink]" for the first occurrence,
         "path [hardlink of p1, p2, ...]" for subsequent ones sharing the same inode
       - directories: "path/ [dir]"
       - symlinks: "path [symlink -> target]" (not entered)
       - other (pipes, sockets, etc.): "path [kind]" *)
    let dump_tree root =
      let str ~dir fname =
        let dir = if String.is_empty dir then root else Path.relative root dir in
        Path.to_string (Path.relative dir fname)
      in
      let inodes = Table.create (module Int) 16 in
      Fpath.traverse
        ~dir:(Path.to_string root)
        ~init:()
        ~sort_entries:true
        ~on_file:(fun ~dir fname () ->
          let s = str ~dir fname in
          match Path.lstat (Path.of_string s) with
          | Error _ -> printfn "%s [file]" s
          | Ok { st_nlink; st_ino; _ } ->
            if st_nlink <= 1
            then printfn "%s [file]" s
            else (
              let peers = Table.find inodes st_ino |> Option.value ~default:[] in
              Table.set inodes st_ino (s :: peers);
              match List.rev peers with
              | [] -> printfn "%s [hardlink]" s
              | others -> printfn "%s [hardlink of %s]" s (String.concat ~sep:", " others)))
        ~on_dir:(fun ~dir fname () -> printfn "%s/ [dir]" (str ~dir fname))
        ~on_symlink:
          (`Call
              (fun ~dir fname () ->
                let s = str ~dir fname in
                printfn "%s [symlink -> %s]" s (Unix.readlink s);
                (), None))
        ~on_other:
          (`Call
              (fun ~dir fname kind () ->
                printfn "%s [%s]" (str ~dir fname) (File_kind.to_string_hum kind)))
        ()
    ;;

    (* [with_temp_dir name f] creates a temp directory, runs [f dir] which
       should return a string (use [%expect.output] to capture printed output).
       The temp dir path is replaced with [name] in the output before printing.
       User_error exceptions are caught and their message is printed (also
       censored). This allows [%expect] blocks to match against stable output. *)
    let with_temp_dir name f =
      Temp.with_temp_dir
        ~parent_dir:(Path.of_string ".")
        ~prefix:"symlink"
        ~suffix:"test"
        ~f:(function
        | Error e -> raise e
        | Ok dir ->
          let s =
            match f dir with
            | s -> s
            | exception User_error.E msg ->
              User_message.pp msg |> Format.asprintf "%a" Pp.to_fmt
          in
          Re.replace_string (Re.compile (Re.str (Path.to_string dir))) ~by:name s
          |> print_string)
    ;;

    let make_dir dir name = Path.mkdir_p (Path.relative dir name)
    let make_file dir name = Io.write_file (Path.relative dir name) name

    let make_symlink dir ~src ~dst =
      Unix.symlink src (Path.to_string (Path.relative dir dst))
    ;;

    let%expect_test "no symlink no change" =
      with_temp_dir "somedir" (fun dir ->
        make_dir dir "real_dir";
        make_dir dir "other_dir";
        make_file dir "real_dir/file2.txt";
        make_file dir "other_dir/file1.txt";
        printfn "before";
        dump_tree dir;
        resolve_directory_symlinks_in dir;
        printfn "\nafter";
        dump_tree dir;
        [%expect.output]);
      [%expect
        {|
        before
        somedir/real_dir/ [dir]
        somedir/other_dir/ [dir]
        somedir/other_dir/file1.txt [file]
        somedir/real_dir/file2.txt [file]

        after
        somedir/real_dir/ [dir]
        somedir/other_dir/ [dir]
        somedir/other_dir/file1.txt [file]
        somedir/real_dir/file2.txt [file]
        |}]
    ;;

    let%expect_test "nested directory symlinks resolved recursively" =
      with_temp_dir "$DIR" (fun dir ->
        make_dir dir "real_dir";
        make_dir dir "real_dir/sub";
        make_file dir "real_dir/sub/deep.txt";
        make_symlink dir ~src:"real_dir" ~dst:"link";
        resolve_directory_symlinks_in dir;
        dump_tree dir;
        [%expect.output]);
      [%expect
        {|
        $DIR/real_dir/ [dir]
        $DIR/link/ [dir]
        $DIR/link/sub/ [dir]
        $DIR/link/sub/deep.txt [hardlink]
        $DIR/real_dir/sub/ [dir]
        $DIR/real_dir/sub/deep.txt [hardlink of $DIR/link/sub/deep.txt]
        |}]
    ;;

    let%expect_test "outside test" =
      with_temp_dir "$OUTSIDE" (fun outside ->
        with_temp_dir "$DIR" (fun dir ->
          make_symlink dir ~src:(Path.reach ~from:dir outside) ~dst:"escape";
          resolve_directory_symlinks_in dir;
          [%expect.output]);
        [%expect.output]);
      [%expect
        {|
        Error: Unable to resolve symlink $DIR/escape: its target
        $OUTSIDE is outside the source directory
        |}]
    ;;

    let%expect_test "file symlinks" =
      with_temp_dir "$DIR" (fun dir ->
        make_file dir "file.txt";
        make_symlink dir ~src:"file.txt" ~dst:"link";
        resolve_directory_symlinks_in dir;
        dump_tree dir;
        [%expect.output]);
      [%expect
        {|
        $DIR/link [symlink -> file.txt]
        $DIR/file.txt [file]
        |}]
    ;;

    let%expect_test "broken symlinks deleted" =
      with_temp_dir "$DIR" (fun dir ->
        make_file dir "keep.txt";
        make_symlink dir ~src:"nonexistent" ~dst:"broken_link";
        (* Todo-ambre: read the log for the deletion message *)
        resolve_directory_symlinks_in dir;
        dump_tree dir;
        [%expect.output]);
      [%expect {| $DIR/keep.txt [file] |}]
    ;;

    let%expect_test "cycle" =
      with_temp_dir "$DIR" (fun dir ->
        make_dir dir "dir_a";
        make_dir dir "dir_b";
        make_file dir "dir_a/file.txt";
        make_file dir "dir_b/file.txt";
        make_symlink dir ~src:"../dir_b" ~dst:"dir_a/link_to_b";
        make_symlink dir ~src:"../dir_a" ~dst:"dir_b/link_to_a";
        resolve_directory_symlinks_in dir;
        [%expect.output]);
      [%expect
        {|
        Error: Unable to resolve symlink
        $DIR/dir_a/link_to_b/link_to_a, it is part of a cycle.
        |}]
    ;;

    let%expect_test "nested file link" =
      with_temp_dir "$DIR" (fun dir ->
        make_dir dir "target_dir";
        make_file dir "target_dir/file.txt";
        make_dir dir "real_dir";
        make_file dir "real_dir/regular.txt";
        make_symlink dir ~src:"../target_dir/file.txt" ~dst:"real_dir/inner_link";
        make_symlink dir ~src:"real_dir" ~dst:"link";
        resolve_directory_symlinks_in dir;
        dump_tree dir;
        [%expect.output]);
      [%expect
        {|
        $DIR/target_dir/ [dir]
        $DIR/real_dir/ [dir]
        $DIR/link/ [dir]
        $DIR/link/regular.txt [hardlink]
        $DIR/link/inner_link [symlink -> ../target_dir/file.txt]
        $DIR/real_dir/regular.txt [hardlink of $DIR/link/regular.txt]
        $DIR/real_dir/inner_link [symlink -> ../target_dir/file.txt]
        $DIR/target_dir/file.txt [file]
        |}]
    ;;

    let%expect_test "pipes?" =
      with_temp_dir "$DIR" (fun dir ->
        make_dir dir "real_dir";
        make_file dir "real_dir/file.txt";
        (* Pipes aren't copied! *)
        Unix.mkfifo (Path.to_string (Path.relative dir "real_dir/my_pipe")) 0o644;
        make_symlink dir ~src:"my_pipe" ~dst:"real_dir/pipelink";
        make_symlink dir ~src:"real_dir" ~dst:"link";
        resolve_directory_symlinks_in dir;
        dump_tree dir;
        [%expect.output]);
      [%expect
        {|
        $DIR/real_dir/ [dir]
        $DIR/link/ [dir]
        $DIR/link/pipelink [symlink -> ../real_dir/my_pipe]
        $DIR/link/file.txt [hardlink]
        $DIR/real_dir/pipelink [symlink -> my_pipe]
        $DIR/real_dir/my_pipe [named pipe]
        $DIR/real_dir/file.txt [hardlink of $DIR/link/file.txt]
        |}]
    ;;
  end)
;;
