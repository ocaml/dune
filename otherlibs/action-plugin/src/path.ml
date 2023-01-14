type t = string

let concat = Filename.concat

let to_string t = t

let of_string path =
  match Filename.is_relative path with
  | false ->
    invalid_arg
      (Printf.sprintf
         "Path \"%s\" is absolute. All paths used with dune-action-plugin must \
          be relative."
         path)
  | true -> path

let exists t =
  match Unix.stat (to_string t) with
  | { Unix.st_kind = S_REG; _ } -> true
  | { Unix.st_kind = _; _ } -> false
  | exception Unix.Unix_error (_, _, _) -> false

module O = struct
  let ( ^/ ) = concat
end
