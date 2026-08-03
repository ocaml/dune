open Stdune

external sendfile
  :  src:Unix.file_descr
  -> dst:Unix.file_descr
  -> int
  -> unit
  = "stdune_sendfile"

let sendfile_rejects_premature_eof () =
  let dir = Temp.create Dir ~prefix:"sendfile" ~suffix:"test" in
  let src = Path.relative dir "src" in
  let dst = Path.relative dir "dst" in
  Io.write_file src "";
  match Unix.fork () with
  | 0 ->
    let src = Unix.openfile (Path.to_string src) [ O_RDONLY ] 0 in
    let dst = Unix.openfile (Path.to_string dst) [ O_WRONLY; O_CREAT ] 0o600 in
    (match sendfile ~src ~dst 1 with
     | () -> Unix._exit 2
     | exception Unix.Unix_error _ -> Unix._exit 0)
  | child ->
    let child = Pid.of_int_exn child in
    let rec wait attempts =
      match Proc.wait (Pid child) [ WNOHANG ] with
      | None when attempts = 0 ->
        Pid.kill_exn child `Pid Kill;
        ignore (Proc.wait (Pid child) [] : Proc.Process_info.t option);
        false
      | None ->
        Unix.sleepf 0.01;
        wait (attempts - 1)
      | Some { status = WEXITED 0; _ } -> true
      | Some _ -> false
    in
    wait 100
;;

let%expect_test "sendfile rejects a source ending before the requested size" =
  assert (sendfile_rejects_premature_eof ());
  [%expect {||}]
;;
