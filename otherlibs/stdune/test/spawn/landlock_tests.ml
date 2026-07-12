open Stdune

let () = assert (Landlock.available ())
let external_path path = Path.as_external path |> Option.value_exn
let exists path = Path.stat path |> Result.is_ok

let spawn ?landlock root script =
  let pid =
    Spawn.spawn
      ?landlock
      ~cwd:(Spawn.Working_dir.Path (Path.to_string root))
      ~prog:"/bin/sh"
      ~argv:[ "sh"; "-c"; "exec 2>/dev/null; " ^ script ]
      ()
  in
  let { Proc.Process_info.status; _ } = Proc.wait (Pid pid) [] |> Option.value_exn in
  match status with
  | WEXITED 0 -> ()
  | WEXITED code -> Printf.ksprintf failwith "child exited with code %d" code
  | WSIGNALED signal -> Printf.ksprintf failwith "child got signal %d" signal
  | WSTOPPED signal -> Printf.ksprintf failwith "child stopped with signal %d" signal
;;

let%expect_test "landlock policy confines only the child" =
  let root = Temp.create Dir ~prefix:"landlock" ~suffix:"test" in
  let denied_dir = Path.relative root "denied" in
  let writable_dir = Path.relative root "writable" in
  let existing_file = Path.relative denied_dir "existing" in
  let denied_new_file = Path.relative denied_dir "new" in
  let outside_file = Path.relative writable_dir "child" in
  let parent_file = Path.relative denied_dir "parent" in
  Path.mkdir_p denied_dir;
  Path.mkdir_p writable_dir;
  Io.write_file existing_file "original\n";
  let policy =
    Landlock.Policy.create ~deny_write:[ external_path denied_dir ] ~allow_write:[]
  in
  spawn
    ~landlock:policy
    root
    "echo child > writable/child; if echo child > denied/new; then exit 10; fi; if echo \
     changed > denied/existing; then exit 11; fi";
  Io.write_file parent_file "parent\n";
  Printf.printf
    "outside: %sdenied new exists: %b\nexisting: %sparent: %s"
    (Io.read_file outside_file)
    (exists denied_new_file)
    (Io.read_file existing_file)
    (Io.read_file parent_file);
  [%expect
    {|
    outside: child
    denied new exists: false
    existing: original
    parent: parent
    |}]
;;

let%expect_test "landlock policy allows explicit write holes" =
  let root = Temp.create Dir ~prefix:"landlock" ~suffix:"test" in
  let denied_dir = Path.relative root "denied" in
  let allowed_dir = Path.relative denied_dir "allowed" in
  let allowed_file = Path.relative allowed_dir "child" in
  let denied_file = Path.relative denied_dir "blocked" in
  Path.mkdir_p allowed_dir;
  let policy =
    Landlock.Policy.create
      ~deny_write:[ external_path denied_dir ]
      ~allow_write:[ external_path allowed_dir ]
  in
  spawn
    ~landlock:policy
    root
    "echo child > denied/allowed/child; if echo child > denied/blocked; then exit 10; fi";
  Printf.printf
    "allowed: %sdenied exists: %b\n"
    (Io.read_file allowed_file)
    (exists denied_file);
  [%expect
    {|
    allowed: child
    denied exists: false
    |}]
;;

let%expect_test "restriction does not leak into later spawns" =
  let root = Temp.create Dir ~prefix:"landlock" ~suffix:"test" in
  let denied_dir = Path.relative root "denied" in
  let restricted_file = Path.relative denied_dir "restricted" in
  let later_file = Path.relative denied_dir "later" in
  Path.mkdir_p denied_dir;
  let policy =
    Landlock.Policy.create ~deny_write:[ external_path denied_dir ] ~allow_write:[]
  in
  (* The restricted child cannot write into the denied directory. *)
  spawn ~landlock:policy root "if echo x > denied/restricted; then exit 10; fi";
  (* A child spawned afterwards without a policy is unaffected: the parent never
     restricted itself, so the restriction is not inherited. *)
  spawn root "echo later > denied/later";
  Printf.printf
    "restricted exists: %b\nlater: %s"
    (exists restricted_file)
    (Io.read_file later_file);
  [%expect
    {|
    restricted exists: false
    later: later
    |}]
;;
