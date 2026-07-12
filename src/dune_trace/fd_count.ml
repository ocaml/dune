open Stdune

type t =
  | Unknown
  | This of int

module Mac = struct
  external open_fds : pid:int -> int = "dune_trace_open_fds"
  external available : unit -> bool = "dune_trace_available"
end

let proc_fs () =
  match Sys.readdir "/proc/self/fd" with
  | files -> This (Array.length files - 1 (* -1 for the dirfd *))
  | exception Sys_error _ -> Unknown
;;

let how = ref `Unknown

let get () =
  match !how with
  | `Disable -> Unknown
  | `Proc_fs -> proc_fs ()
  | `Mac -> This (Mac.open_fds ~pid:(Pid.me () |> Pid.to_int))
  | `Unknown ->
    if Mac.available ()
    then (
      how := `Mac;
      This (Mac.open_fds ~pid:(Pid.me () |> Pid.to_int)))
    else (
      match proc_fs () with
      | Unknown -> Unknown
      | This _ as n ->
        how := `Proc_fs;
        n)
;;
