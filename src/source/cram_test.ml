open Import

type t =
  | File of Path.Source.t
  | Dir of
      { file : Path.Source.t
      ; dir : Path.Source.t
      }

module Name = struct
  type t = string

  let to_string s = s
  let to_alias = Alias_name.of_string
end

let fname_in_dir_test = Filename.run_t
let suffix = ".t"
let is_cram_suffix fn = String.ends_with (Filename.to_string fn) ~suffix

let to_dyn =
  let open Dyn in
  function
  | File f -> variant "File" [ Path.Source.to_dyn f ]
  | Dir { file; dir } ->
    variant
      "Dir"
      [ record [ "file", Path.Source.to_dyn file; "dir", Path.Source.to_dyn dir ] ]
;;

let path = function
  | File file -> file
  | Dir d -> d.dir
;;

let name t ~dune_version =
  let name = path t |> Path.Source.basename |> Filename.to_string in
  if dune_version >= (3, 25)
  then name
  else String.drop_suffix ~suffix name |> Option.value_exn
;;

let script t =
  match t with
  | File f -> f
  | Dir d -> d.file
;;
