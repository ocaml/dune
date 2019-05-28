open! Stdune

type some =
  | Symlink
  | Copy

type t = some option

(* these should be listed in the default order of preference *)
let all = [None; Some Symlink; Some Copy]

let error =
  Error "invalid sandboxing mode, must be 'none', 'symlink' or 'copy'"

let of_string = function
  | "none" -> Ok None
  | "symlink" -> Ok (Some Symlink : t)
  | "copy" -> Ok (Some Copy)
  | _ -> error
