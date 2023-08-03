type t =
  | Success
  | Error
  | Signal

let all = [ Success; Error; Signal ]

let code = function
  | Success -> 0
  | Error -> 1
  | Signal -> 130
;;

let doc = function
  | Success -> "on success."
  | Error -> "if an error happened."
  | Signal -> "if it was interrupted by a signal."
;;

let info e = Cmdliner.Cmd.Exit.info (code e) ~doc:(doc e)
