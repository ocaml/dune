open Stdune

(* Avoid running inherited [at_exit] hooks in forked children. *)
external sys_exit : int -> 'a = "caml_sys_exit"

let sleep_forever () =
  while true do
    Unix.sleep max_int
  done
;;

let wait_no_eintr pid =
  let pid = Pid.to_int pid in
  let rec loop () =
    match Unix.waitpid [] pid with
    | _ -> ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in
  loop ()
;;

let read_until_eof fd =
  let buffer = Bytes.create 1 in
  let rec loop () =
    match Unix.read fd buffer 0 1 with
    | 0 -> Unix.close fd
    | _ -> loop ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in
  loop ()
;;

let fork_child_with_grandchild () =
  let read_fd, write_fd = Unix.pipe () in
  match Unix.fork () with
  | 0 ->
    (match
       Unix.close read_fd;
       let child_read_fd, child_write_fd = Unix.pipe () in
       match Unix.fork () with
       | 0 ->
         Unix.close write_fd;
         Unix.close child_write_fd;
         read_until_eof child_read_fd;
         sys_exit 0
       | grandchild ->
         Unix.close child_read_fd;
         let oc = Unix.out_channel_of_descr write_fd in
         Printf.fprintf oc "%d\n%!" grandchild;
         close_out oc;
         sleep_forever ()
     with
     | _ -> sys_exit 1)
  | child ->
    let child = Pid.of_int_exn child in
    Unix.close write_fd;
    let ic = Unix.in_channel_of_descr read_fd in
    let grandchild = input_line ic |> Int.of_string_exn |> Pid.of_int_exn in
    close_in ic;
    child, grandchild
;;

let cleanup_child_tree (child, _grandchild) =
  Unix.kill (Pid.to_int child) Sys.sigkill;
  wait_no_eintr child
;;

let children_contain_only_child ~child ~grandchild =
  let self = Pid.me () in
  let rec loop attempts_left =
    match Proc.Linux.Process_tree.children_of self with
    | Error _ -> Error ()
    | Ok children ->
      if Pid.Set.mem children child && not (Pid.Set.mem children grandchild)
      then Ok true
      else if attempts_left = 0
      then Ok false
      else (
        Unix.sleepf 0.01;
        loop (attempts_left - 1))
  in
  loop 100
;;

let%expect_test "Linux process tree direct children" =
  let child_tree = fork_child_with_grandchild () in
  Fun.protect
    ~finally:(fun () -> cleanup_child_tree child_tree)
    (fun () ->
       let child, grandchild = child_tree in
       match children_contain_only_child ~child ~grandchild with
       | Error () -> print_endline "error reading process tree"
       | Ok found -> printfn "found direct child only: %b" found);
  [%expect {| found direct child only: true |}]
;;

let%expect_test "Linux process tree reports missing roots" =
  (match Proc.Linux.Process_tree.children_of (Pid.of_int_exn max_int) with
   | Ok _ -> print_endline "unexpected success"
   | Error _ -> print_endline "error");
  [%expect {| error |}]
;;
