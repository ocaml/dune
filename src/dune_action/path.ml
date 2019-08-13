type t = string

let concat = Filename.concat

let to_string t = t

let of_string path =
  match Filename.is_relative path with
  | false ->
    failwith
      "Path.of_string - absolute path provided. All paths used with \
       Dune_action must be relative."
  | true -> path

module O = struct
  let ( ^/ ) = concat
end
