open Import

type t =
  | File of Path.Source.t
  | Dir of
      { file : Path.Source.t
      ; dir : Path.Source.t
      }

let fname_in_dir_test = "run.t"
let suffix = ".t"
let is_cram_suffix = String.is_suffix ~suffix

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

let name t =
  path t |> Path.Source.basename |> String.drop_suffix ~suffix |> Option.value_exn
;;

let script t =
  match t with
  | File f -> f
  | Dir d -> d.file
;;
