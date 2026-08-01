open Unix

let%expect_test "read_file with a nonexistent file" =
  (match Fs_io.read_file (__FILE__ ^ ".does-not-exist") with
   | Ok _ -> print_endline "Read contents"
   | Error (Unix_error (ENOENT, _, _)) -> print_endline "Error ENOENT"
   | Error exn -> raise exn
   | exception Unix_error (ENOENT, _, _) -> print_endline "Raised ENOENT");
  [%expect {| Raised ENOENT |}]
;;
