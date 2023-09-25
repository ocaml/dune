open! Stdune
module Encoder = Dune_lang.Encoder
module Decoder = Dune_lang.Decoder

let ( / ) = Path.relative

module Serializable = struct
  type t =
    { repo_id : Repository_id.t option
    ; source : string
    }

  let equal { repo_id; source } t =
    Option.equal Repository_id.equal repo_id t.repo_id && String.equal source t.source
  ;;

  let to_dyn { repo_id; source } =
    let open Dyn in
    variant
      "opam_repo_serializable"
      [ option Repository_id.to_dyn repo_id; string source ]
  ;;

  let encode { repo_id; source } =
    let open Encoder in
    record_fields
      [ field "source" string source; field_o "repo_id" Repository_id.encode repo_id ]
  ;;

  let decode =
    let open Decoder in
    fields
      (let+ source = field "source" string
       and+ repo_id = field_o "repo_id" Repository_id.decode in
       { repo_id; source })
  ;;
end

type t =
  { packages_dir_path : Path.t
  ; serializable : Serializable.t option
  }

let equal { packages_dir_path; serializable } t =
  Path.equal packages_dir_path t.packages_dir_path
  && Option.equal Serializable.equal serializable t.serializable
;;

let minimum_opam_version = OpamVersion.of_string "2.0"
let serializable { serializable; _ } = serializable

let repo_id t =
  let open Option.O in
  let* serializable = serializable t in
  serializable.repo_id
;;

let validate_repo_file opam_repo_dir_path =
  let opam_repo_file_path = opam_repo_dir_path / "repo" in
  let repo =
    try
      OpamFilename.raw (Path.to_string opam_repo_file_path)
      |> OpamFile.make
      |> OpamFile.Repo.read
    with
    | OpamSystem.Internal_error message -> User_error.raise [ Pp.text message ]
    | OpamPp.(Bad_format _ | Bad_format_list _ | Bad_version _) as bad_format_exn ->
      User_error.raise [ Pp.text (OpamPp.string_of_bad_format bad_format_exn) ]
    | unexpected_exn ->
      Code_error.raise
        (Printf.sprintf
           "Unexpected exception raised while validating opam repo file %s"
           (Path.to_string_maybe_quoted opam_repo_file_path))
        [ "exception", Exn.to_dyn unexpected_exn ]
  in
  match OpamFile.Repo.opam_version repo with
  | None ->
    User_error.raise
      [ Pp.textf
          "The file %s lacks an \"opam-version\" field."
          (Path.to_string_maybe_quoted opam_repo_file_path)
      ]
      ~hints:
        [ Pp.textf
            "Add `opam-version: \"%s\"` to the file."
            (OpamVersion.to_string minimum_opam_version)
        ]
  | Some opam_version ->
    if OpamVersion.compare opam_version minimum_opam_version < 0
    then
      User_error.raise
        [ Pp.textf
            "The file %s specifies an opam-version which is too low (%s). The minimum \
             opam-version is %s."
            (Path.to_string_maybe_quoted opam_repo_file_path)
            (OpamVersion.to_string opam_version)
            (OpamVersion.to_string minimum_opam_version)
        ]
        ~hints:
          [ Pp.textf
              "Change the opam-version field to `opam-version: \"%s\"`."
              (OpamVersion.to_string minimum_opam_version)
          ]
;;

let of_opam_repo_dir_path ~source ~repo_id opam_repo_dir_path =
  if not (Path.exists opam_repo_dir_path)
  then
    User_error.raise
      [ Pp.textf "%s does not exist" (Path.to_string_maybe_quoted opam_repo_dir_path) ];
  if not (Path.is_directory opam_repo_dir_path)
  then
    User_error.raise
      [ Pp.textf "%s is not a directory" (Path.to_string_maybe_quoted opam_repo_dir_path)
      ];
  let packages_dir_path = opam_repo_dir_path / "packages" in
  if not (Path.exists packages_dir_path && Path.is_directory packages_dir_path)
  then
    User_error.raise
      [ Pp.textf
          "%s doesn't look like a path to an opam repository as it lacks a subdirectory \
           named \"packages\""
          (Path.to_string_maybe_quoted opam_repo_dir_path)
      ];
  validate_repo_file opam_repo_dir_path;
  let serializable =
    Option.map source ~f:(fun source -> { Serializable.repo_id; source })
  in
  { packages_dir_path; serializable }
;;

let if_exists p =
  match Path.exists p with
  | false -> None
  | true -> Some p
;;

(* Return the path to the directory containing the version directories for a package name *)
let get_opam_package_version_dir_path { packages_dir_path; _ } opam_package_name =
  let p = packages_dir_path / OpamPackage.Name.to_string opam_package_name in
  if_exists p
;;

(* Return the path to an "opam" file describing a particular package
   (name and version) from this opam repository. *)
let get_opam_file_path t opam_package =
  let open Option.O in
  let* base = get_opam_package_version_dir_path t (OpamPackage.name opam_package) in
  base / OpamPackage.to_string opam_package / "opam" |> if_exists
;;

let get_opam_package_files_path t opam_package =
  let open Option.O in
  let* base = get_opam_package_version_dir_path t (OpamPackage.name opam_package) in
  base / OpamPackage.to_string opam_package / "files" |> if_exists
;;

(* Returns a list containing all versions of a package with a given name *)
let all_package_versions t opam_package_name =
  let open Option.O in
  let* version_dir_path = get_opam_package_version_dir_path t opam_package_name in
  match Path.readdir_unsorted version_dir_path with
  | Error e ->
    User_error.raise
      [ Pp.textf
          "Unable to read package versions from %s: %s"
          (Path.to_string_maybe_quoted version_dir_path)
          (Dune_filesystem_stubs.Unix_error.Detailed.to_string_hum e)
      ]
  | Ok version_dirs -> Some (List.map version_dirs ~f:OpamPackage.of_string)
;;

(* Reads an opam package definition from an "opam" file in this repository
   corresponding to a package (name and version). *)
let load_opam_package t opam_package =
  let open Option.O in
  let* opam_file_path = get_opam_file_path t opam_package in
  Some
    (OpamFile.OPAM.read
       (OpamFile.make (OpamFilename.raw (Path.to_string opam_file_path))))
;;

let load_all_versions ts opam_package_name =
  let versions =
    List.filter_map ts ~f:(fun t -> all_package_versions t opam_package_name)
  in
  match versions with
  | [] -> Error `Package_not_found
  | pkgs ->
    pkgs
    |> List.concat
    |> List.filter_map ~f:(fun opam_pkg ->
      List.find_map ts ~f:(fun t -> load_opam_package t opam_pkg))
    |> Result.ok
;;

module Private = struct
  let create ?source ?repo_id () =
    let packages_dir_path = Path.of_string "/" in
    let serializable =
      Option.map source ~f:(fun source -> { Serializable.repo_id; source })
    in
    { packages_dir_path; serializable }
  ;;
end
