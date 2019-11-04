exception Failed of string

let unix f =
  try f ()
  with Unix.Unix_error (e, name, arg) ->
    raise
      (Failed
         (Printf.sprintf "unable to %s %s: %s" name arg (Unix.error_message e)))
