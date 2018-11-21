open Stdune

let enabled = ref false

module Fd_count = struct
  type t = Unknown | This of int

  let get () =
    match Sys.readdir "/proc/self/fd" with
    | exception _ -> Unknown
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

let record () =
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
