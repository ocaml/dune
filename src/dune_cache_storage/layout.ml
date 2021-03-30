open Stdune

let default_root_path () =
  Path.L.relative
    (Path.of_filename_relative_to_initial_cwd Xdg.cache_dir)
    [ "dune"; "db" ]

let root_path =
  let var = "DUNE_CACHE_ROOT" in
  match Sys.getenv_opt var with
  | None -> default_root_path ()
  | Some path ->
    if Filename.is_relative path then
      failwith (sprintf "%s should be an absolute path, but is %s" var path);
    Path.of_filename_relative_to_initial_cwd path

let ( / ) = Path.relative

(* We version metadata and actual cache content separately. *)
let metadata_storage_path = root_path / "meta" / "v5"

let file_storage_path = root_path / "files" / "v4"

let value_storage_path = root_path / "values" / "v3"

let cache_path ~dir ~hash =
  let two_first_chars = sprintf "%c%c" hash.[0] hash.[1] in
  dir / two_first_chars / hash

let metadata_path ~rule_or_action_digest =
  cache_path ~dir:metadata_storage_path
    ~hash:(Digest.to_string rule_or_action_digest)

let value_path ~value_digest =
  cache_path ~dir:value_storage_path ~hash:(Digest.to_string value_digest)

let file_path ~file_digest =
  cache_path ~dir:file_storage_path ~hash:(Digest.to_string file_digest)

let temp_path = root_path / "temp"

let root_path_subdirectories =
  [ metadata_storage_path; file_storage_path; value_storage_path; temp_path ]
