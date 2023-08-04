type t = string

let concat = Filename.concat
let to_string t = t

let of_string path =
  match Filename.is_relative path with
  | false ->
    invalid_arg
      (Printf.sprintf
         "Path \"%s\" is absolute. All paths used with dune-action-plugin must be \
          relative."
         path)
  | true -> path
;;

module O = struct
  let ( ^/ ) = concat
end
