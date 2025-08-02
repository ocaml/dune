module Readdir_result = struct
  [@@@warning "-37"]

  (* The values are constructed on the C-side *)
  type t =
    | End_of_directory
    | Entry of string * File_kind.Option.t
end

external readdir_with_kind_if_available_unix
  :  Unix.dir_handle
  -> Readdir_result.t
  = "caml__dune_filesystem_stubs__readdir"

let readdir_with_kind_if_available_win32 : Unix.dir_handle -> Readdir_result.t =
  fun dir ->
  (* Windows also gives us the information about file kind and it's discarded by
     [readdir]. We could do better here, but the Windows code is more
     complicated. (there's an additional OCaml abstraction layer) *)
  match Unix.readdir dir with
  | exception End_of_file -> Readdir_result.End_of_directory
  | entry -> Entry (entry, UNKNOWN)
;;

let readdir_with_kind_if_available : Unix.dir_handle -> Readdir_result.t =
  if Stdlib.Sys.win32
  then readdir_with_kind_if_available_win32
  else readdir_with_kind_if_available_unix
;;

let read_directory_with_kinds_exn dir_path =
  let dir = Unix.opendir dir_path in
  Fun.protect
    ~finally:(fun () -> Unix.closedir dir)
    (fun () ->
       let rec loop acc =
         match readdir_with_kind_if_available dir with
         | Entry (("." | ".."), _) -> loop acc
         | End_of_directory -> acc
         | Entry (base, kind) ->
           let k kind = loop ((base, kind) :: acc) in
           let skip () = loop acc in
           File_kind.Option.elim
             kind
             ~none:(fun () ->
               match Unix.lstat (Filename.concat dir_path base) with
               | exception Unix.Unix_error _ ->
                 (* File disappeared between readdir & lstat system calls. Handle
                   as if readdir never told us about it *)
                 skip ()
               | stat -> k stat.st_kind)
             ~some:k
       in
       loop [])
;;

let read_directory_with_kinds dir_path =
  Unix_error.Detailed.catch read_directory_with_kinds_exn dir_path
;;

let read_directory_exn dir_path =
  let dir = Unix.opendir dir_path in
  Fun.protect
    ~finally:(fun () -> Unix.closedir dir)
    (fun () ->
       let rec loop acc =
         match readdir_with_kind_if_available dir with
         | Entry (("." | ".."), _) -> loop acc
         | End_of_directory -> acc
         | Entry (base, _) -> loop (base :: acc)
       in
       loop [])
;;

let read_directory dir_path = Unix_error.Detailed.catch read_directory_exn dir_path
