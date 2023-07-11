open! Stdune

let ( / ) = Filename.concat

type t = { packages_dir_path : Filename.t }

let packages_dir_path { packages_dir_path } = packages_dir_path

let minimum_opam_version = OpamVersion.of_string "2.0"

let validate_repo_file opam_repo_dir_path =
  let opam_repo_file_path = opam_repo_dir_path / "repo" in
  let repo =
    try
      OpamFilename.raw opam_repo_file_path
      |> OpamFile.make |> OpamFile.Repo.read
    with
    | OpamSystem.Internal_error message -> User_error.raise [ Pp.text message ]
    | OpamPp.(Bad_format _ | Bad_format_list _ | Bad_version _) as
      bad_format_exn ->
      User_error.raise [ Pp.text (OpamPp.string_of_bad_format bad_format_exn) ]
    | unexpected_exn ->
      Code_error.raise
        (Printf.sprintf
           "Unexpected exception raised while validating opam repo file %s"
           (String.maybe_quoted opam_repo_file_path))
        [ ("exception", Exn.to_dyn unexpected_exn) ]
  in
  match OpamFile.Repo.opam_version repo with
  | None ->
    User_error.raise
      [ Pp.textf "The file %s lacks an \"opam-version\" field."
          (String.maybe_quoted opam_repo_file_path)
      ]
      ~hints:
        [ Pp.textf "Add `opam-version: \"%s\"` to the file."
            (OpamVersion.to_string minimum_opam_version)
        ]
  | Some opam_version ->
    if OpamVersion.compare opam_version minimum_opam_version < 0 then
      User_error.raise
        [ Pp.textf
            "The file %s specifies an opam-version which is too low (%s). The \
             minimum opam-version is %s."
            (String.maybe_quoted opam_repo_file_path)
            (OpamVersion.to_string opam_version)
            (OpamVersion.to_string minimum_opam_version)
        ]
        ~hints:
          [ Pp.textf "Change the opam-version field to `opam-version: \"%s\"`."
              (OpamVersion.to_string minimum_opam_version)
          ]

let of_opam_repo_dir_path opam_repo_dir_path =
  let opam_repo_dir_path = Path.to_string opam_repo_dir_path in
  if not (Sys.file_exists opam_repo_dir_path) then
    User_error.raise
      [ Pp.textf "%s does not exist" (String.maybe_quoted opam_repo_dir_path) ];
  if not (Sys.is_directory opam_repo_dir_path) then
    User_error.raise
      [ Pp.textf "%s is not a directory"
          (String.maybe_quoted opam_repo_dir_path)
      ];
  let packages_dir_path = opam_repo_dir_path / "packages" in
  if
    not (Sys.file_exists packages_dir_path && Sys.is_directory packages_dir_path)
  then
    User_error.raise
      [ Pp.textf
          "%s doesn't look like a path to an opam repository as it lacks a \
           subdirectory named \"packages\""
          (String.maybe_quoted opam_repo_dir_path)
      ];
  validate_repo_file opam_repo_dir_path;
  { packages_dir_path }

(* Return the path to the directory containing the version directories for a package name *)
let get_opam_package_version_dir_path t opam_package_name =
  t.packages_dir_path / OpamPackage.Name.to_string opam_package_name

(* Return the path to an "opam" file describing a particular package
   (name and version) from this opam repository. *)
let get_opam_file_path t opam_package =
  get_opam_package_version_dir_path t (OpamPackage.name opam_package)
  / OpamPackage.to_string opam_package
  / "opam"

(* Returns a list containing all versions of a package with a given name *)
let all_package_versions t opam_package_name =
  let version_dir_path =
    get_opam_package_version_dir_path t opam_package_name
  in
  if Sys.file_exists version_dir_path then
    try
      Sys.readdir version_dir_path
      |> Array.to_list
      |> List.map ~f:OpamPackage.of_string
      |> Result.ok
    with Sys_error msg ->
      User_error.raise
        [ Pp.textf "Unable to read package versions from %s: %s"
            (String.maybe_quoted version_dir_path)
            msg
        ]
  else Error `Package_not_found

(* Reads an opam package definition from an "opam" file in this repository
   corresponding to a package (name and version). *)
let load_opam_package t opam_package =
  let opam_file_path = get_opam_file_path t opam_package in
  if not (Sys.file_exists opam_file_path) then
    User_error.raise
      [ Pp.textf
          "Couldn't find package file for \"%s\". It was expected to be \
           located in %s but this file does not exist"
          (OpamPackage.to_string opam_package)
          (String.maybe_quoted opam_file_path)
      ];
  OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw opam_file_path))

let load_all_versions t opam_package_name =
  all_package_versions t opam_package_name
  |> Result.map ~f:(List.map ~f:(load_opam_package t))
