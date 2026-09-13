open Stdune
open Dune_tests_common
open Dyn

let () = init ()

let print_cleanup (path, fd) =
  let closed =
    match Unix.fstat (Fd.unsafe_to_unix_file_descr fd) with
    | exception Unix.Unix_error (EBADF, _, _) -> true
    | _ -> false
  in
  printfn "exists: %b" (Fpath.exists (Path.to_string path));
  printfn "closed: %b" closed
;;

let%expect_test "write through a read-only temporary file's creation descriptor" =
  let dir = Temp.create Dir ~prefix:"temp-fd" ~suffix:"test" in
  let perm = Permissions.Mode.create ~user:Permissions.read () in
  let content = String.make (128 * 8) 'x' ^ "\000\r\n\255" in
  let file =
    Temp.with_temp_file_fd
      ~perm
      ~dir
      ~prefix:"file"
      ~suffix:"test"
      ~f:(fun result ->
        let path, fd = Result.ok_exn result in
        let mode =
          (Unix.fstat (Fd.unsafe_to_unix_file_descr fd)).st_perm
          |> Permissions.Mode.of_int
        in
        printfn "read-only: %b" (not (Permissions.test_any Permissions.write mode));
        Io.write_fd fd content |> Result.ok_exn;
        printfn "contents match: %b" (String.equal content (Io.read_file_exn path));
        path, fd)
      ()
  in
  print_cleanup file;
  [%expect
    {|
    read-only: true
    contents match: true
    exists: false
    closed: true
    |}]
;;

let%expect_test "temporary descriptor cleanup on callback failure" =
  let dir = Temp.create Dir ~prefix:"temp-fd" ~suffix:"test" in
  let file = ref None in
  (match
     Temp.with_temp_file_fd
       ~dir
       ~prefix:"file"
       ~suffix:"test"
       ~f:(fun result ->
         let ((_, fd) as created) = Result.ok_exn result in
         file := Some created;
         let mode =
           (Unix.fstat (Fd.unsafe_to_unix_file_descr fd)).st_perm
           |> Permissions.Mode.of_int
         in
         printfn "default writable: %b" (Permissions.test Permissions.write mode);
         raise Exit)
       ()
   with
   | () -> printfn "unexpected success"
   | exception Exit -> printfn "callback exception propagated");
  Option.iter !file ~f:print_cleanup;
  [%expect
    {|
    default writable: true
    callback exception propagated
    exists: false
    closed: true
    |}]
;;

let%expect_test "temporary descriptor creation failure" =
  let parent = Temp.create Dir ~prefix:"temp-fd" ~suffix:"test" in
  let dir = Path.relative parent "missing" in
  Temp.with_temp_file_fd
    ~dir
    ~prefix:"file"
    ~suffix:"test"
    ~f:(function
      | Error (Unix.Unix_error (ENOENT, _, _)) -> printfn "creation error"
      | Error exn -> raise exn
      | Ok _ -> printfn "unexpected success")
    ();
  [%expect {| creation error |}]
;;

let%expect_test "temporary file can be renamed while its descriptor is open" =
  let dir = Temp.create Dir ~prefix:"temp-fd" ~suffix:"test" in
  let dst = Path.relative dir "published" in
  let file =
    Temp.with_temp_file_fd
      ~dir
      ~prefix:"file"
      ~suffix:"test"
      ~f:(fun result ->
        let path, fd = Result.ok_exn result in
        Io.write_fd fd "contents" |> Result.ok_exn;
        Unix.rename (Path.to_string path) (Path.to_string dst);
        path, fd)
      ()
  in
  print_cleanup file;
  printfn "%s" (Io.read_file_exn dst);
  [%expect
    {|
    exists: false
    closed: true
    contents
    |}]
;;

let%expect_test "Temp.clear_dir works" =
  let path = Temp.create Dir ~prefix:"dune" ~suffix:"unit_test" in
  Io.write_file_exn (Path.relative path "foo") "";
  let print () =
    Path.readdir_unsorted path |> Result.to_dyn (list Filename.to_dyn) opaque |> print_dyn
  in
  print ();
  Temp.clear_dir path;
  print ();
  [%expect
    {|
    Ok [ "foo" ]
    Ok []
  |}]
;;
