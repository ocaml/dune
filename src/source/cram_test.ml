open Import

type t =
  | File of Source_path.t
  | Dir of
      { file : Source_path.t
      ; dir : Source_path.t
      }

module Name = struct
  type t = string

  let to_string s = s
  let to_alias = Alias_name.of_string
end

let fname_in_dir_test = Filename.run_t
let suffix = ".t"
let is_cram_suffix fn = String.ends_with (Filename.to_string fn) ~suffix

let repr =
  let dir =
    Repr.record
      "cram-directory"
      [ Repr.field "file" Source_path.repr ~get:fst
      ; Repr.field "dir" Source_path.repr ~get:snd
      ]
  in
  Repr.variant
    "cram-test"
    [ Repr.case "File" Source_path.repr ~proj:(function
        | File file -> Some file
        | Dir _ -> None)
    ; Repr.case "Dir" dir ~proj:(function
        | Dir { file; dir } -> Some (file, dir)
        | File _ -> None)
    ]
;;

let to_dyn = Repr.to_dyn repr

let path = function
  | File file -> file
  | Dir d -> d.dir
;;

let name t ~dune_version =
  let name = path t |> Source_path.basename |> Filename.to_string in
  if dune_version >= (3, 25)
  then name
  else String.drop_suffix ~suffix name |> Option.value_exn
;;

let script t =
  match t with
  | File f -> f
  | Dir d -> d.file
;;
