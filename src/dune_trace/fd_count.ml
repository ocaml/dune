open Stdune

type t =
  | Unknown
  | This of int

module Mac = struct
  external open_fds : pid:int -> int = "dune_trace_open_fds"
  external available : unit -> bool = "dune_trace_available"
end

let lsof =
  let prog = lazy (Bin.which ~path:(Env_path.path Env.initial) "lsof") in
  (* note: we do not use the Process module here, because it would create a
     circular dependency *)
  fun () ->
    match Lazy.force prog with
    | None -> Unknown
    | Some prog ->
      let lsof_r, lsof_w = Unix.pipe ~cloexec:true () in
      let prog = Path.to_string prog in
      let pid =
        let argv =
          [ prog; "-l"; "-O"; "-P"; "-n"; "-w"; "-p"; string_of_int (Unix.getpid ()) ]
        in
        Spawn.spawn ~prog ~argv ~stdout:lsof_w () |> Pid.of_int
      in
      Unix.close lsof_w;
      (match
         (* CR-someday rgrinberg: we can't reap anything here withhout
            co-operation from the scheduler *)
         let _, status = Unix.waitpid [] (Pid.to_int pid) in
         status
       with
       | Unix.WEXITED 0 ->
         let count =
           let chan = Unix.in_channel_of_descr lsof_r in
           let rec loop acc =
             match input_line chan with
             | exception End_of_file -> acc
             | _ -> loop (acc + 1)
           in
           (* the output contains a header line *)
           let res = loop (-1) in
           Io.close_in chan;
           res
         in
         This count
       | (exception Unix.Unix_error (_, _, _))
       (* The final [waitpid] call fails with:

            {[
              Error: waitpid(): No child processes
            ]} *)
       | _ ->
         Unix.close lsof_r;
         Unknown)
;;

let proc_fs () =
  match Sys.readdir "/proc/self/fd" with
  | files -> This (Array.length files - 1 (* -1 for the dirfd *))
  | exception Sys_error _ -> Unknown
;;

let how = ref `Unknown
let pid = lazy (Unix.getpid ())

let get () =
  match !how with
  | `Disable -> Unknown
  | `Lsof -> lsof ()
  | `Proc_fs -> proc_fs ()
  | `Mac -> This (Mac.open_fds ~pid:(Lazy.force pid))
  | `Unknown ->
    if Mac.available ()
    then (
      how := `Mac;
      This (Mac.open_fds ~pid:(Lazy.force pid)))
    else (
      match proc_fs () with
      | This _ as n ->
        how := `Proc_fs;
        n
      | Unknown ->
        let res = lsof () in
        (how
         := match res with
            | This _ -> `Lsof
            | Unknown -> `Disable);
        res)
;;
