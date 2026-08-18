open Stdune
module Exn = Exn
module Proc = Proc
module Spawn = Spawn

let landlock_probe = Filename.concat (Sys.getcwd ()) "exe/landlock_probe.exe"

let open_directory path =
  Unix.openfile path [ O_RDONLY; O_CLOEXEC ] 0 |> Fd.unsafe_of_unix_file_descr
;;

let wait pid =
  match Proc.wait (Proc.Pid pid) [] with
  | None -> failwith "process was not reaped"
  | Some { status = WEXITED 0; _ } -> ()
  | Some { status = WEXITED n; _ } -> Printf.ksprintf failwith "exited with code %d" n
  | Some { status = WSIGNALED n; _ } -> Printf.ksprintf failwith "got signal %d" n
  | Some { status = WSTOPPED n; _ } -> Printf.ksprintf failwith "stopped with signal %d" n
;;

let with_landlock f =
  if Spawn.Landlock.available ()
  then f ()
  else if
    String.equal (Option.value (Sys.getenv_opt "CI") ~default:"false") "true"
    && Sys.file_exists "/proc/sys/kernel"
  then failwith "Landlock must be available in Linux CI"
;;

let with_open_directories paths ~f =
  let fds = List.map paths ~f:open_directory in
  Exn.protect ~finally:(fun () -> List.iter fds ~f:Fd.close) ~f:(fun () -> f fds)
;;

let create_file path = close_out (open_out path)

let remove_if_exists path =
  match Unix.lstat path with
  | { st_kind = S_DIR; _ } -> Unix.rmdir path
  | _ -> Unix.unlink path
  | exception Unix.Unix_error (ENOENT, _, _) -> ()
;;

let run_landlock_probe dir =
  with_landlock (fun () ->
    let allowed_dir = Filename.concat dir "allowed" in
    let denied_dir = Filename.concat dir "denied" in
    let allowed_marker = Filename.concat allowed_dir "marker" in
    let denied_marker = Filename.concat denied_dir "marker" in
    let cleanup () =
      List.iter [ allowed_marker; denied_marker ] ~f:remove_if_exists;
      List.iter [ allowed_dir; denied_dir; dir ] ~f:remove_if_exists
    in
    cleanup ();
    List.iter [ dir; allowed_dir; denied_dir ] ~f:(fun path -> Unix.mkdir path 0o700);
    Exn.protect ~finally:cleanup ~f:(fun () ->
      with_open_directories [ allowed_dir ] ~f:(fun allowed_dirs ->
        let landlock = Spawn.Landlock.allow_writes_to_directories allowed_dirs in
        wait
          (Spawn.spawn
             ~landlock
             ~prog:landlock_probe
             ~argv0:"landlock_probe.exe"
             ~args:(Array.Immutable.of_list [ allowed_marker; denied_marker ])
             ()));
      create_file denied_marker))
;;

let%expect_test "landlock rules follow file descriptors across renames" =
  with_landlock (fun () ->
    let root = "landlock-renamed" in
    let original = Filename.concat root "original" in
    let renamed = Filename.concat root "renamed" in
    let allowed_marker = Filename.concat renamed "allowed" in
    let denied_marker = Filename.concat original "denied" in
    let cleanup () =
      List.iter [ allowed_marker; denied_marker ] ~f:remove_if_exists;
      List.iter [ original; renamed; root ] ~f:remove_if_exists
    in
    cleanup ();
    Unix.mkdir root 0o700;
    Unix.mkdir original 0o700;
    Exn.protect ~finally:cleanup ~f:(fun () ->
      with_open_directories [ original ] ~f:(fun original_fds ->
        let landlock = Spawn.Landlock.allow_writes_to_directories original_fds in
        Unix.rename original renamed;
        Unix.mkdir original 0o700;
        wait
          (Spawn.spawn
             ~landlock
             ~prog:landlock_probe
             ~argv0:"landlock_probe.exe"
             ~args:(Array.Immutable.of_list [ allowed_marker; denied_marker ])
             ()))));
  [%expect {| |}]
;;

let%expect_test "landlock when spawning" =
  run_landlock_probe "landlock-spawn";
  [%expect {| |}]
;;

let%expect_test "extend a policy with multiple writable directories" =
  with_landlock (fun () ->
    let root = "landlock-multiple-directories" in
    let base = Filename.concat root "base" in
    let added1 = Filename.concat root "added1" in
    let added2 = Filename.concat root "added2" in
    let denied = Filename.concat root "denied" in
    let directories = [ base; added1; added2; denied ] in
    let base_marker = Filename.concat base "marker" in
    let added1_marker = Filename.concat added1 "marker" in
    let added2_marker = Filename.concat added2 "marker" in
    let denied_marker = Filename.concat denied "marker" in
    let markers = [ base_marker; added1_marker; added2_marker; denied_marker ] in
    let cleanup () =
      List.iter markers ~f:remove_if_exists;
      List.iter (directories @ [ root ]) ~f:remove_if_exists
    in
    cleanup ();
    Unix.mkdir root 0o700;
    List.iter directories ~f:(fun path -> Unix.mkdir path 0o700);
    Exn.protect ~finally:cleanup ~f:(fun () ->
      with_open_directories directories ~f:(function
        | [ base_fd; added1_fd; added2_fd; _ ] ->
          let landlock = Spawn.Landlock.allow_writes_to_directories [ base_fd ] in
          let landlock =
            Spawn.Landlock.add_writable_directories [ added1_fd; added2_fd ] landlock
          in
          wait
            (Spawn.spawn
               ~landlock
               ~prog:landlock_probe
               ~argv0:"landlock_probe.exe"
               ~args:
                 (Array.Immutable.of_list
                    [ "write-three"
                    ; base_marker
                    ; added1_marker
                    ; added2_marker
                    ; denied_marker
                    ])
               ())
        | _ -> failwith "unexpected number of directory descriptors")));
  [%expect {| |}]
;;

let%expect_test "write access rights" =
  with_landlock (fun () ->
    let root = "landlock-access-rights" in
    let allowed = Filename.concat root "allowed" in
    let denied = Filename.concat root "denied" in
    let names = [ "truncate"; "delete"; "rename-source"; "reparent-source" ] in
    let files =
      List.concat_map [ allowed; denied ] ~f:(fun dir ->
        List.map names ~f:(Filename.concat dir))
    in
    let directories =
      List.concat_map [ allowed; denied ] ~f:(fun dir ->
        [ Filename.concat dir "remove-dir"; Filename.concat dir "mkdir" ])
    in
    let symlinks =
      List.map [ allowed; denied ] ~f:(fun dir -> Filename.concat dir "symlink")
    in
    let rename_targets =
      [ Filename.concat allowed "rename-target"
      ; Filename.concat denied "rename-target"
      ; Filename.concat denied "reparent-target"
      ]
    in
    let cleanup () =
      List.iter (symlinks @ rename_targets @ files) ~f:remove_if_exists;
      List.iter (directories @ [ allowed; denied; root ]) ~f:remove_if_exists
    in
    cleanup ();
    Unix.mkdir root 0o700;
    List.iter [ allowed; denied ] ~f:(fun path -> Unix.mkdir path 0o700);
    List.iter files ~f:create_file;
    List.iter
      [ Filename.concat allowed "remove-dir"; Filename.concat denied "remove-dir" ]
      ~f:(fun path -> Unix.mkdir path 0o700);
    Exn.protect ~finally:cleanup ~f:(fun () ->
      with_open_directories [ allowed ] ~f:(fun allowed_fds ->
        let landlock = Spawn.Landlock.allow_writes_to_directories allowed_fds in
        wait
          (Spawn.spawn
             ~landlock
             ~prog:landlock_probe
             ~argv0:"landlock_probe.exe"
             ~args:(Array.Immutable.of_list [ "access-rights"; allowed; denied ])
             ()))));
  [%expect {| |}]
;;
