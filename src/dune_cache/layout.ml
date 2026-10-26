open Import

let ( / ) = Path.relative
let build_cache_dir = lazy (Lazy.force Dune_util.cache_root_dir / "db")
let temp_dir = lazy (Lazy.force build_cache_dir / "temp")

let cache_path ~dir ~hex =
  let two_first_chars = sprintf "%c%c" hex.[0] hex.[1] in
  dir / two_first_chars / hex
;;

let list_entries ~storage =
  let readdir where = Path.readdir_unsorted where |> Result.value ~default:[] in
  let entries dir =
    match
      let dir_s = Filename.to_string dir in
      String.length dir_s = 2 && String.for_all ~f:Char.is_lowercase_hex dir_s
    with
    | false -> []
    | true ->
      let dir = Path.relative_fname storage dir in
      readdir dir
      |> List.filter_map ~f:(fun entry_name ->
        Filename.to_string entry_name
        |> Digest.from_hex
        |> Option.map ~f:(fun digest -> Path.relative_fname dir entry_name, digest))
  in
  readdir storage |> List.to_seq |> Seq.concat_map ~f:(fun x -> List.to_seq (entries x))
;;

module Versioned = struct
  let metadata_storage_dir t =
    lazy (Lazy.force build_cache_dir / "meta" / Version.Metadata.to_string t)
  ;;

  let file_storage_dir =
    let directory version =
      lazy (Lazy.force build_cache_dir / "files" / Version.File.to_string version)
    in
    let v3 = directory Version.File.V3 in
    let v4 = directory Version.File.V4 in
    function
    | Version.File.V3 -> v3
    | V4 -> v4
  ;;

  let metadata_path t ~rule_or_action_digest =
    lazy
      (let dir = Lazy.force (metadata_storage_dir t) in
       cache_path ~dir ~hex:(Digest.to_string rule_or_action_digest))
  ;;

  let file_path t ~file_digest =
    lazy
      (let dir = Lazy.force (file_storage_dir t) in
       cache_path ~dir ~hex:(Digest.to_string file_digest))
  ;;

  let list_metadata_entries t =
    list_entries ~storage:(Lazy.force (metadata_storage_dir t))
  ;;

  let list_file_entries t = list_entries ~storage:(Lazy.force (file_storage_dir t))
end

let metadata_storage_dir = Versioned.metadata_storage_dir Version.Metadata.current
let metadata_path = Versioned.metadata_path Version.Metadata.current
let file_storage_dir = Versioned.file_storage_dir Version.File.current
let file_path = Versioned.file_path Version.File.current

let create_cache_directories () =
  [ Lazy.force temp_dir; Lazy.force metadata_storage_dir; Lazy.force file_storage_dir ]
  |> Result.List.iter ~f:(fun path ->
    match Fpath.mkdir_p (Path.to_string path) with
    | `Already_exists | `Created -> Ok ()
    | exception Unix.Unix_error (e, _, _) -> Error (path, e))
;;
