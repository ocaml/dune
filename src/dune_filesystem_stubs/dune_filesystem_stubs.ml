module File_kind = struct
  type t = Unix.file_kind =
    | S_REG
    | S_DIR
    | S_CHR
    | S_BLK
    | S_LNK
    | S_FIFO
    | S_SOCK

  module Option = struct
    [@@@ warning "-37"]
    (* The values are constructed on the C-side *)
    type t = private
      | S_REG
      | S_DIR
      | S_CHR
      | S_BLK
      | S_LNK
      | S_FIFO
      | S_SOCK
      | UNKNOWN

    let elim ~none ~some t =
      match t with
      | S_REG -> some (S_REG : Unix.file_kind)
      | S_DIR -> some S_DIR
      | S_CHR -> some S_CHR
      | S_BLK -> some S_BLK
      | S_LNK -> some S_LNK
      | S_FIFO -> some S_FIFO
      | S_SOCK -> some S_SOCK
      | UNKNOWN -> none ()
    ;;
  end
end

module Readdir_result = struct

  [@@@ warning "-37"]

  (* The values are constructed on the C-side *)
  type t = private
    | End_of_directory
    | Entry of string * File_kind.Option.t
end

external readdir_with_kind_if_available
  :  Unix.dir_handle
  -> Readdir_result.t
  = "caml__dune_filesystem_stubs__readdir"

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
               | exception _ ->
                 (* File disappeared between readdir & lstat system calls.
                    Handle as if readdir never told us about it *)
                 skip ()
               | stat -> k stat.st_kind)
             ~some:k
       in
       loop [])
;;

let catch_unix_error f x = match f x with
  | exception Unix.Unix_error (error, _, _) -> Error error
  | res -> Ok res

let read_directory_with_kinds dir_path =
  catch_unix_error read_directory_with_kinds_exn dir_path

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

let read_directory dir_path =
  catch_unix_error read_directory_exn dir_path
