open Stdune

let enabled = ref false

module Fd_count = struct
  type t = Unknown | This of int

  let try_to_use_lsof () =
    (* note: we do not use the Process module here, because it would
       create a circular dependency *)
    let temp = Filename.temp_file "dune" ".lsof" in
    let stdout =
      Unix.openfile temp [O_WRONLY; O_CREAT; O_TRUNC; O_SHARE_DELETE] 0o666
    in
    let prog = "/usr/sbin/lsof" in
    let argv = [prog; "-w"; "-p"; string_of_int (Unix.getpid ())] in
    let pid = Spawn.spawn ~prog ~argv ~stdout () in
    Unix.close stdout;
    match Unix.waitpid [] pid with
    | _, Unix.WEXITED 0 ->
      let num_lines = List.length (Io.input_lines (open_in temp)) in
      This (num_lines - 1) (* the output contains a header line *)
    | _ -> Unknown

  let get () =
    match Sys.readdir "/proc/self/fd" with
    | exception _ ->
      begin match try_to_use_lsof () with
      | exception _ -> Unknown
      | value -> value
      end
    | files -> This (Array.length files - 1 (* -1 for the dirfd *))

  let map2 ~f a b =
    match a, b with
    | Unknown, x | x, Unknown -> x
    | This a, This b -> This (f a b)

  let max = map2 ~f:max

  let to_string = function
    | Unknown -> "unknown"
    | This n -> string_of_int n
end

type t =
  { mutable fds : Fd_count.t
  }

let observed_max =
  { fds = Unknown
  }

let catapult = Catapult.make ()

let record () =
  Catapult.emit_gc_counters catapult;
  if !enabled then begin
    let fds = Fd_count.get () in
    observed_max.fds <- Fd_count.max fds observed_max.fds
  end

let dump () =
  let pr fmt = Printf.eprintf (fmt ^^ "\n") in
  pr "Stats:";
  pr "max opened fds: %s" (Fd_count.to_string observed_max.fds);
  flush stderr

let enable () =
  enabled := true;
  at_exit dump

let enable_catapult path =
  Catapult.enable catapult path;
  at_exit (fun () -> Catapult.close catapult)

let with_process ~program ~args fiber =
  let open Fiber.O in
  let event = Catapult.on_process_start catapult ~program ~args in
  fiber >>| fun result ->
  Catapult.on_process_end catapult event;
  result
