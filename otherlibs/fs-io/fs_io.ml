module Exn = struct
  let protectx x ~f ~finally =
    match f x with
    | y ->
      finally x;
      y
    | exception e ->
      finally x;
      raise e
  ;;
end

module Bytes = BytesLabels

let rec eagerly_input_acc ic s ~pos ~len acc =
  if len <= 0
  then acc
  else (
    let r = input ic s pos len in
    if r = 0 then acc else eagerly_input_acc ic s ~pos:(pos + r) ~len:(len - r) (acc + r))
;;

(* [eagerly_input_string ic len] tries to read [len] chars from the channel.
   Unlike [really_input_string], if the file ends before [len] characters are
   found, it returns the characters it was able to read instead of raising an
   exception.

   This can be detected by checking that the length of the resulting string is
   less than [len]. *)
let eagerly_input_string ic len =
  let buf = Bytes.create len in
  let r = eagerly_input_acc ic buf ~pos:0 ~len 0 in
  if r = len then Bytes.unsafe_to_string buf else Bytes.sub_string buf ~pos:0 ~len:r
;;

let with_file_in fn ~f = Exn.protectx (Stdlib.open_in_bin fn) ~finally:close_in ~f
let too_big = Failure "file is too large"

let read_all_unless_large =
  (* We use 65536 because that is the size of OCaml's IO buffers. *)
  let chunk_size = 65536 in
  (* Generic function for channels such that seeking is unsupported or broken *)
  let read_all_generic t buffer =
    let rec loop () =
      Buffer.add_channel buffer t chunk_size;
      loop ()
    in
    try loop () with
    | End_of_file -> Ok (Buffer.contents buffer)
  in
  fun t ->
    (* Optimisation for regular files: if the channel supports seeking, we
       compute the length of the file so that we read exactly what we need and
       avoid an extra memory copy. We expect that most files Dune reads are
       regular files so this optimizations seems worth it. *)
    match in_channel_length t with
    | exception Sys_error _ -> read_all_generic t (Buffer.create chunk_size)
    | n when n > Sys.max_string_length -> Error too_big
    | n ->
      (* For some files [in_channel_length] returns an invalid value. For
         instance for files in /proc it returns [0] and on Windows the returned
         value is larger than expected (it counts linebreaks as 2 chars, even
         in text mode).

         To be robust in both directions, we: - use [eagerly_input_string]
         instead of [really_input_string] in case we reach the end of the file
         early - read one more character to make sure we did indeed reach the
         end of the file *)
      let s = eagerly_input_string t n in
      (match input_char t with
       | exception End_of_file -> Ok s
       | c ->
         (* The [+ chunk_size] is to make sure there is at least [chunk_size]
            free space so that the first [Buffer.add_channel buffer t
            chunk_size] in [read_all_generic] does not grow the buffer. *)
         let buffer = Buffer.create (String.length s + 1 + chunk_size) in
         Buffer.add_string buffer s;
         Buffer.add_char buffer c;
         read_all_generic t buffer)
;;

let read_file_chan fn = with_file_in fn ~f:read_all_unless_large

let read_all_fd =
  let rec read fd buf pos left =
    if left = 0
    then `Ok
    else (
      match Unix.read fd buf pos left with
      | 0 -> `Eof
      | n -> read fd buf (pos + n) (left - n))
  in
  fun fd ->
    match Unix.fstat fd with
    | exception Unix.Unix_error (e, x, y) -> Error (`Unix (e, x, y))
    | { Unix.st_size; _ } ->
      if st_size = 0
      then Ok ""
      else if st_size > Sys.max_string_length
      then Error `Too_big
      else (
        let b = Bytes.create st_size in
        match read fd b 0 st_size with
        | exception Unix.Unix_error (e, x, y) -> Error (`Unix (e, x, y))
        | `Eof -> Error `Retry
        | `Ok -> Ok (Bytes.unsafe_to_string b))
;;

let with_file_in_fd fn ~f =
  Exn.protectx (Unix.openfile fn [ O_RDONLY; O_CLOEXEC ] 0) ~f ~finally:Unix.close
;;

let read_file fn =
  with_file_in_fd fn ~f:(fun fd ->
    match read_all_fd fd with
    | Ok s -> Ok s
    | Error `Retry -> read_file_chan fn
    | Error `Too_big -> Error too_big
    | Error (`Unix (e, s, x)) -> Error (Unix.Unix_error (e, s, x)))
;;

let write_file =
  let rec write fd str pos left =
    if left > 0
    then (
      let written = Unix.single_write_substring fd str pos left in
      write fd str (pos + written) (left - written))
  in
  fun ~perm ~path ~data ->
    match
      Unix.openfile path [ O_WRONLY; O_CLOEXEC; O_CREAT; O_TRUNC ] perm
      |> Exn.protectx ~finally:Unix.close ~f:(fun fd ->
        write fd data 0 (String.length data))
    with
    | exception exn -> Error exn
    | () -> Ok ()
;;
