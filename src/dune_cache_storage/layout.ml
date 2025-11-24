open Stdune
open Import

let ( / ) = Path.relative

(** The directory containing the build cache.
    Uses [$DUNE_CACHE_ROOT] if set, or
    [$DUNE_CACHE_HOME/db] if set, or
    [Dune_util.default_cache_dir/db] otherwise. *)
let build_cache_dir =
  lazy
    (let high_priority_var = "DUNE_CACHE_ROOT" in
     match Sys.getenv_opt high_priority_var with
     | Some path ->
       Dune_util.check_absolute ~var:high_priority_var ~path;
       Path.external_ (Path.External.of_string path)
     | None ->
       let low_priority_var = "DUNE_CACHE_HOME" in
       (match Sys.getenv_opt low_priority_var with
        | Some path ->
          Dune_util.check_absolute ~var:low_priority_var ~path;
          Path.external_ (Path.External.of_string path) / "db"
        | None -> Lazy.force Dune_util.default_cache_dir / "db"))
;;

let temp_dir = lazy (Lazy.force build_cache_dir / "temp")

let cache_path ~dir ~hex =
  let two_first_chars = sprintf "%c%c" hex.[0] hex.[1] in
  dir / two_first_chars / hex
;;

(* List all entries in a given storage directory. *)
let list_entries ~storage =
  let open Result.O in
  let entries dir =
    match String.length dir = 2 && String.for_all ~f:Char.is_lowercase_hex dir with
    | false ->
      (* Ignore directories whose name isn't a two-character hex value. *)
      Ok []
    | true ->
      let dir = storage / dir in
      Path.readdir_unsorted dir
      >>| List.filter_map ~f:(fun entry_name ->
        match Digest.from_hex entry_name with
        | None ->
          (* Ignore entries whose names are not hex values. *)
          None
        | Some digest -> Some (dir / entry_name, digest))
  in
  match Path.readdir_unsorted storage >>= Result.List.concat_map ~f:entries with
  | Ok res -> res
  | Error (ENOENT, _, _) -> []
  | Error unix_error -> User_error.raise [ Unix_error.Detailed.pp unix_error ]
;;

module Versioned = struct
  let metadata_storage_dir t =
    lazy (Lazy.force build_cache_dir / "meta" / Version.Metadata.to_string t)
  ;;

  let file_storage_dir t =
    lazy (Lazy.force build_cache_dir / "files" / Version.File.to_string t)
  ;;

  let value_storage_dir t =
    lazy (Lazy.force build_cache_dir / "values" / Version.Value.to_string t)
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

  let value_path t ~value_digest =
    lazy
      (let dir = Lazy.force (value_storage_dir t) in
       cache_path ~dir ~hex:(Digest.to_string value_digest))
  ;;

  let list_metadata_entries t =
    lazy (list_entries ~storage:(Lazy.force (metadata_storage_dir t)))
  ;;

  let list_file_entries t = lazy (list_entries ~storage:(Lazy.force (file_storage_dir t)))

  let list_value_entries t =
    lazy (list_entries ~storage:(Lazy.force (value_storage_dir t)))
  ;;
end

let metadata_storage_dir = Versioned.metadata_storage_dir Version.Metadata.current
let metadata_path = Versioned.metadata_path Version.Metadata.current
let file_storage_dir = Versioned.file_storage_dir Version.File.current
let file_path = Versioned.file_path Version.File.current
let value_storage_dir = Versioned.value_storage_dir Version.Value.current
let value_path = Versioned.value_path Version.Value.current

let create_cache_directories () =
  [ Lazy.force temp_dir
  ; Lazy.force metadata_storage_dir
  ; Lazy.force file_storage_dir
  ; Lazy.force value_storage_dir
  ]
  |> Result.List.iter ~f:(fun path ->
    match Fpath.mkdir_p (Path.to_string path) with
    | Already_exists | Created -> Ok ()
    | exception Unix.Unix_error (e, _, _) -> Error (path, e))
;;
