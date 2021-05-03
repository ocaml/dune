let printf = Printf.printf

open Stdune
open Base

let tmp_dir = Stdlib.Filename.concat (Unix.getcwd ()) "working-dir"

let () =
  try Unix.mkdir tmp_dir 0o777 with
  | _ -> ()

let () = Unix.chdir tmp_dir

let () = Path.set_root (Path.External.of_string tmp_dir)

let () = Path.Build.set_build_dir (Path.Build.Kind.of_string "_build")

let now () = Unix.gettimeofday ()

let retry_loop (type a) ~period ~timeout ~(f : unit -> a option) : a option =
  let t0 = now () in
  let rec loop () =
    match f () with
    | Some res -> Some res
    | None ->
      let t1 = now () in
      if Float.( < ) (t1 -. t0) timeout then (
        Thread.delay period;
        loop ()
      ) else
        None
  in
  loop ()

let print_s sexp = printf "%s\n" (Sexp.to_string_hum sexp)

let sexp_of_path path = sexp_of_string (Path.to_string path)

let%expect_test _ =
  let mutex = Mutex.create () in
  let critical_section ~f =
    Mutex.lock mutex;
    Exn.protect ~f ~finally:(fun () -> Mutex.unlock mutex)
  in
  let events_buffer = ref [] in
  let watcher =
    Dune_file_watcher.create ~debounce_interval:None
      ~thread_safe_send_files_changed:(fun events ->
        Mutex.lock mutex;
        events_buffer := !events_buffer @ events;
        Mutex.unlock mutex)
      ~root:Path.root
  in
  Dune_file_watcher.wait_watches_established_blocking watcher;
  Stdio.Out_channel.write_all "x" ~data:"x";
  let res =
    retry_loop ~period:0.01 ~timeout:3.0 ~f:(fun () ->
        critical_section ~f:(fun () ->
            match !events_buffer with
            | [] -> None
            | list ->
              events_buffer := [];
              Some list))
  in
  print_s ([%sexp_of: path list option] res);
  [%expect {|
((x))
|}]
