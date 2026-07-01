open Stdune

let () = Dune_tests_common.init ()

type spec =
  { wrapped : bool
  ; stubs : bool
  }

type command_result =
  { status : Unix.process_status
  ; stdout : string
  ; stderr : string
  }

let dune_prog =
  lazy
    (Dune_under_test.path
     |> Path.of_string_allow_outside_workspace
     |> Path.to_absolute_filename)
;;

let status_to_string = function
  | Unix.WEXITED n -> sprintf "exited %d" n
  | Unix.WSIGNALED n -> sprintf "signaled %d" n
  | Unix.WSTOPPED n -> sprintf "stopped %d" n
;;

let command_to_string prog argv = String.concat ~sep:" " (prog :: argv)

let spawn_and_capture ?env ~prog ~argv ~cwd () =
  Temp.with_temp_file ~dir:cwd ~prefix:"stdout" ~suffix:"" ~f:(fun stdout_result ->
    Temp.with_temp_file ~dir:cwd ~prefix:"stderr" ~suffix:"" ~f:(fun stderr_result ->
      let stdout_path = Result.ok_exn stdout_result in
      let stderr_path = Result.ok_exn stderr_result in
      let stdout_fd =
        Unix.openfile
          (Path.to_absolute_filename stdout_path)
          [ Unix.O_WRONLY; Unix.O_TRUNC ]
          0o666
      in
      let stderr_fd =
        Unix.openfile
          (Path.to_absolute_filename stderr_path)
          [ Unix.O_WRONLY; Unix.O_TRUNC ]
          0o666
      in
      let pid =
        Exn.protect
          ~finally:(fun () ->
            Unix.close stdout_fd;
            Unix.close stderr_fd)
          ~f:(fun () ->
            let env = Option.map env ~f:Spawn.Env.of_list in
            Spawn.spawn
              ~prog
              ~argv:(prog :: argv)
              ~stdout:stdout_fd
              ~stderr:stderr_fd
              ?env
              ())
      in
      let waited_pid, status = Unix.waitpid [] pid in
      if waited_pid <> pid then Code_error.raise "waitpid returned another process" [];
      { status; stdout = Io.read_file stdout_path; stderr = Io.read_file stderr_path }))
;;

let with_chdir dir ~f =
  let cwd = Sys.getcwd () in
  Unix.chdir (Path.to_absolute_filename dir);
  Exn.protect ~f ~finally:(fun () -> Unix.chdir cwd)
;;

let run_dune ?env root argv =
  let prog = Lazy.force dune_prog in
  let result =
    with_chdir root ~f:(fun () -> spawn_and_capture ?env ~prog ~argv ~cwd:root ())
  in
  match result.status with
  | Unix.WEXITED 0 ->
    if String.is_empty result.stderr
    then result.stdout
    else
      Code_error.raise
        "command printed to stderr"
        [ "command", Dyn.string (command_to_string prog argv)
        ; "stderr", Dyn.string result.stderr
        ]
  | status ->
    Code_error.raise
      "command failed"
      [ "command", Dyn.string (command_to_string prog argv)
      ; "status", Dyn.string (status_to_string status)
      ; "stdout", Dyn.string result.stdout
      ; "stderr", Dyn.string result.stderr
      ]
;;

let write_file root name contents = Io.write_file (Path.relative root name) contents

let setup_project root { wrapped; stubs } =
  write_file
    root
    "dune-project"
    {|(lang dune 2.8)
(package
 (name foo))
|};
  let lib = Path.relative root "lib" in
  let exe = Path.relative root "exe" in
  Path.mkdir_p lib;
  Path.mkdir_p exe;
  let foreign_stubs =
    if stubs then " (foreign_stubs (language c) (names stub))\n" else ""
  in
  write_file
    lib
    "dune"
    (sprintf
       "(library\n (public_name foo)\n (wrapped %b)\n%s (modules ()))\n"
       wrapped
       foreign_stubs);
  if stubs then write_file lib "stub.c" "void foo() {}\n";
  write_file
    exe
    "dune"
    {|(executable
 (name b)
 (link_flags -linkall)
 (libraries foo))
|};
  write_file
    exe
    "b.ml"
    {|let () = print_endline "exe working"
|}
;;

let archive_files root =
  let lib_dir = Path.relative root "_build/install/default/lib/foo" in
  match Path.readdir_unsorted lib_dir with
  | Error _ -> []
  | Ok files ->
    files
    |> List.filter_map ~f:(fun fn ->
      let fn = Filename.to_string fn in
      Option.some_if (Filename.check_suffix fn ".a") fn)
    |> List.sort ~compare:String.compare
;;

let print_exec_output name output = printfn "%s: %s" name (Stdlib.String.trim output)

let run spec =
  Temp.with_temp_dir
    ~parent_dir:(Path.of_string (Sys.getcwd ()))
    ~prefix:"archive_creation_behavior"
    ~suffix:""
    ~f:(fun root_result ->
      let root = Result.ok_exn root_result in
      setup_project root spec;
      ignore (run_dune root [ "build"; "--root"; "."; "@install" ] : string);
      archive_files root |> List.iter ~f:print_endline;
      run_dune root [ "exec"; "./exe/b.exe" ] |> print_exec_output "local";
      Path.rm_rf ~allow_external:true (Path.relative root "lib");
      let env =
        let ocamlpath =
          "OCAMLPATH="
          ^ Path.to_absolute_filename (Path.relative root "_build/install/default/lib")
        in
        Array.to_list (Unix.environment ())
        |> List.filter ~f:(fun var -> not (String.starts_with var ~prefix:"OCAMLPATH="))
        |> fun env -> env @ [ ocamlpath ]
      in
      run_dune ?env:(Some env) root [ "exec"; "--build-dir=_b2"; "./exe/b.exe" ]
      |> print_exec_output "external")
;;
