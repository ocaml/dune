open Unix

let%expect_test "read_file with a nonexistent file" =
  (match Fs_io.read_file (__FILE__ ^ ".does-not-exist") with
   | Ok _ -> print_endline "Read contents"
   | Error (Unix_error (ENOENT, _, _)) -> print_endline "Error ENOENT"
   | Error exn -> raise exn
   | exception Unix_error (ENOENT, _, _) -> print_endline "Raised ENOENT");
  [%expect {| Error ENOENT |}]
;;

let contents = "contents"

let write_contents path =
  match
    let oc = open_out_bin path in
    output_string oc contents;
    close_out oc
  with
  | () -> Unix._exit 0
  | exception _ -> Unix._exit 1
;;

let read_fifo () =
  let path = Filename.temp_file "fs-io" ".fifo" in
  Sys.remove path;
  Unix.mkfifo path 0o600;
  Base.Exn.protect
    ~finally:(fun () -> Sys.remove path)
    ~f:(fun () ->
      match Unix.fork () with
      | 0 -> write_contents path
      | pid ->
        let result = Fs_io.read_file path in
        let _ = Unix.waitpid [] pid in
        result)
;;

(* [read_file] currently fails to read FIFO contents because the reported file
   size is zero. It should return "contents". *)
let%expect_test "read_file with a zero reported size" =
  let result = if Sys.win32 then Ok "" else read_fifo () in
  (match result with
   | Ok actual -> Printf.printf "%S\n" actual
   | Error exn -> raise exn);
  [%expect {| "" |}]
;;
