open Import

type t =
  | File of Path.Source.t
  | Dir of
      { file : Path.Source.t
      ; dir : Path.Source.t
      }

let is_cram_suffix = String.is_suffix ~suffix:".t"

let dyn_of_t =
  let open Dyn in
  function
  | File f -> variant "File" [ Path.Source.to_dyn f ]
  | Dir { file; dir } ->
    variant
      "Dir"
      [ record [ "file", Path.Source.to_dyn file; "dir", Path.Source.to_dyn dir ] ]
;;

let name t =
  String.drop_suffix
    ~suffix:".t"
    (match t with
     | File file -> Path.Source.basename file
     | Dir { file = _; dir } -> Path.Source.basename dir)
  |> Option.value_exn
;;

let script t =
  match t with
  | File f -> f
  | Dir d -> d.file
;;

let fname_in_dir_test = "run.t"
