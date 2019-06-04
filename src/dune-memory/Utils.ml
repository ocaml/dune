open Stdune

exception Failed of string

let unix f =
  try f ()
  with Unix.Unix_error (e, name, arg) ->
    raise
      (Failed
         (Printf.sprintf "unable to %s %s: %s" name arg (Unix.error_message e)))

let rec mkpath p =
  let s = Path.to_string p in
  if not (Sys.file_exists s) then (
    mkpath (Path.parent_exn p) ;
    Unix.mkdir s 0o700 )
