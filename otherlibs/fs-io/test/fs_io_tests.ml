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

let%expect_test "read_file_if_small size boundaries" =
  let path = Filename.temp_file "fs-io" ".txt" in
  Base.Exn.protect
    ~finally:(fun () -> Sys.remove path)
    ~f:(fun () ->
      let oc = open_out_bin path in
      output_string oc contents;
      close_out oc;
      let read max_size =
        Printf.printf "limit %d: " max_size;
        match Fs_io.read_file_if_small path ~max_size with
        | Ok (Some contents) -> Printf.printf "%S\n" contents
        | Ok None -> print_endline "too large"
        | Error exn -> raise exn
      in
      List.iter read [ 0; 7; 8; 9 ];
      let oc = open_out_bin path in
      close_out oc;
      read 0;
      Unix.LargeFile.truncate path 16_777_216L;
      read 8;
      [%expect
        {|
        limit 0: too large
        limit 7: too large
        limit 8: "contents"
        limit 9: "contents"
        limit 0: ""
        limit 8: too large
        |}])
;;

let%expect_test "read_file_if_small with a nonexistent file" =
  (match Fs_io.read_file_if_small (__FILE__ ^ ".does-not-exist") ~max_size:100 with
   | Ok _ -> print_endline "Read contents"
   | Error (Unix_error (ENOENT, _, _)) -> print_endline "Error ENOENT"
   | Error exn -> raise exn);
  [%expect {| Error ENOENT |}]
;;

let%expect_test "read_file_if_small rejects directories" =
  let path = Filename.temp_file "fs-io" ".dir" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  Base.Exn.protect
    ~finally:(fun () -> Unix.rmdir path)
    ~f:(fun () ->
      (match Fs_io.read_file_if_small path ~max_size:0 with
       | Ok _ -> print_endline "Read contents"
       | Error (Unix_error ((EISDIR | EACCES), _, _)) ->
         print_endline "Rejected directory"
       | Error exn -> raise exn);
      [%expect {| Rejected directory |}])
;;
