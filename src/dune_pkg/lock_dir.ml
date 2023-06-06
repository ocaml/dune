open Import
open Dune_lang

module Source = struct
  type t =
    | External_copy of Loc.t * Path.External.t
    | Fetch of
        { url : Loc.t * string
        ; checksum : (Loc.t * Checksum.t) option
        }

  let remove_locs = function
    | External_copy (_loc, path) -> External_copy (Loc.none, path)
    | Fetch { url = _loc, url; checksum } ->
      Fetch
        { url = (Loc.none, url)
        ; checksum =
            Option.map checksum ~f:(fun (_loc, checksum) ->
                (Loc.none, checksum))
        }

  let equal a b =
    match (a, b) with
    | External_copy (loc, path), External_copy (other_loc, other_path) ->
      Loc.equal loc other_loc && Path.External.equal path other_path
    | ( Fetch { url = loc, url; checksum }
      , Fetch { url = other_loc, other_url; checksum = other_checksum } ) ->
      Loc.equal loc other_loc && String.equal url other_url
      && Option.equal
           (fun (loc, checksum) (other_loc, other_checksum) ->
             Loc.equal loc other_loc && Checksum.equal checksum other_checksum)
           checksum other_checksum
    | _ -> false

  let equal_no_locs a b = equal (remove_locs a) (remove_locs b)

  let to_dyn = function
    | External_copy (_loc, path) ->
      Dyn.variant "External_copy" [ Path.External.to_dyn path ]
    | Fetch { url = _loc, url; checksum } ->
      Dyn.variant "Fetch"
        [ Dyn.string url
        ; Dyn.option (fun (_loc, checksum) -> Checksum.to_dyn checksum) checksum
        ]

  module Fields = struct
    let copy = "copy"

    let fetch = "fetch"

    let url = "url"

    let checksum = "checksum"
  end

  let decode =
    let open Dune_lang.Decoder in
    sum
      [ ( Fields.copy
        , located string >>| fun (loc, source) path ->
          External_copy
            ( loc
            , if Filename.is_relative source then
                Path.External.relative path source
              else Path.External.of_string source ) )
      ; ( Fields.fetch
        , fields
          @@ let+ url = field Fields.url (located string)
             and+ checksum = field_o Fields.checksum (located string) in
             let checksum =
               match checksum with
               | None -> None
               | Some ((loc, _) as checksum) -> (
                 match Checksum.of_string_user_error checksum with
                 | Ok checksum -> Some (loc, checksum)
                 | Error e -> raise (User_error.E e))
             in
             fun _ -> Fetch { url; checksum } )
      ]

  let encode t =
    let open Dune_lang.Encoder in
    match t with
    | External_copy (_loc, path) ->
      constr Fields.copy string (Path.External.to_string path)
    | Fetch { url = _loc, url; checksum } ->
      named_record_fields Fields.fetch
        [ field Fields.url string url
        ; field_o Fields.checksum Checksum.encode (Option.map checksum ~f:snd)
        ]
end

module Pkg_info = struct
  type t =
    { name : Package_name.t
    ; version : string
    ; dev : bool
    ; source : Source.t option
    }

  let equal { name; version; dev; source }
      { name = other_name
      ; version = other_version
      ; dev = other_dev
      ; source = other_source
      } =
    Package_name.equal name other_name
    && String.equal version other_version
    && Bool.equal dev other_dev
    && Option.equal Source.equal_no_locs source other_source

  let to_dyn { name; version; dev; source } =
    Dyn.record
      [ ("name", Package_name.to_dyn name)
      ; ("version", Dyn.string version)
      ; ("dev", Dyn.bool dev)
      ; ("source", Dyn.option Source.to_dyn source)
      ]
end

module Pkg = struct
  type t =
    { build_command : Action.t option
    ; install_command : Action.t option
    ; deps : Package_name.t list
    ; info : Pkg_info.t
    ; lock_dir : Path.Source.t
    ; exported_env : String_with_vars.t Dune_lang.Action.Env_update.t list
    }

  let equal
      { build_command; install_command; deps; info; lock_dir; exported_env }
      { build_command = other_build_command
      ; install_command = other_install_command
      ; deps = other_deps
      ; info = other_info
      ; lock_dir = other_lock_dir
      ; exported_env = other_exported_env
      } =
    Option.equal Action.equal_no_locs build_command other_build_command
    && Option.equal Action.equal_no_locs install_command other_install_command
    && List.equal Package_name.equal deps other_deps
    && Pkg_info.equal info other_info
    && Path.Source.equal lock_dir other_lock_dir
    && List.equal
         (Dune_lang.Action.Env_update.equal String_with_vars.equal_no_loc)
         exported_env other_exported_env

  let to_dyn
      { build_command; install_command; deps; info; lock_dir; exported_env } =
    Dyn.record
      [ ("build_command", Dyn.option Action.to_dyn build_command)
      ; ("install_command", Dyn.option Action.to_dyn install_command)
      ; ("deps", Dyn.list Package_name.to_dyn deps)
      ; ("info", Pkg_info.to_dyn info)
      ; ("lock_dir", Path.Source.to_dyn lock_dir)
      ; ( "exported_env"
        , Dyn.list
            (Dune_lang.Action.Env_update.to_dyn String_with_vars.to_dyn)
            exported_env )
      ]

  module Fields = struct
    let version = "version"

    let install = "install"

    let build = "build"

    let deps = "deps"

    let source = "source"

    let dev = "dev"

    let exported_env = "exported_env"
  end

  let decode =
    let open Dune_lang.Decoder in
    enter @@ fields
    @@ let+ version = field ~default:"dev" Fields.version string
       and+ install_command = field_o Fields.install Dune_lang.Action.decode_pkg
       and+ build_command = field_o Fields.build Dune_lang.Action.decode_pkg
       and+ deps = field ~default:[] Fields.deps (repeat Package_name.decode)
       and+ source = field_o Fields.source Source.decode
       and+ dev = field_b Fields.dev
       and+ exported_env =
         field Fields.exported_env ~default:[]
           (repeat Dune_lang.Action.Env_update.decode)
       in
       fun ~lock_dir name ->
         let info =
           let source =
             Option.map source ~f:(fun f ->
                 Path.source lock_dir |> Path.to_absolute_filename
                 |> Path.External.of_string |> f)
           in
           { Pkg_info.name; version; dev; source }
         in
         { build_command; deps; install_command; info; exported_env; lock_dir }

  let encode
      { build_command
      ; install_command
      ; deps
      ; info = { Pkg_info.name = _; version; dev; source }
      ; lock_dir = _
      ; exported_env
      } =
    let open Dune_lang.Encoder in
    record_fields
      [ field Fields.version string version
      ; field_o Fields.install Dune_lang.Action.encode install_command
      ; field_o Fields.build Dune_lang.Action.encode build_command
      ; field_l Fields.deps Package_name.encode deps
      ; field_o Fields.source Source.encode source
      ; field_b Fields.dev dev
      ; field_l Fields.exported_env Dune_lang.Action.Env_update.encode
          exported_env
      ]
end

type t =
  { version : Syntax.Version.t
  ; packages : Pkg.t Package_name.Map.t
  }

let equal { version; packages }
    { version = other_version; packages = other_packages } =
  Syntax.Version.equal version other_version
  && Package_name.Map.equal ~equal:Pkg.equal packages other_packages

let to_dyn { version; packages } =
  Dyn.record
    [ ("version", Syntax.Version.to_dyn version)
    ; ("packages", Package_name.Map.to_dyn Pkg.to_dyn packages)
    ]

let create_latest_version packages =
  let version = Syntax.greatest_supported_version Dune_lang.Pkg.syntax in
  { version; packages }

let default_path = Path.Source.(relative root "dune.lock")

let metadata = "lock.dune"

module Metadata = Dune_sexp.Versioned_file.Make (Unit)

let () = Metadata.Lang.register Dune_lang.Pkg.syntax ()

let encode_metadata t =
  let open Dune_lang.Encoder in
  list sexp
    [ string "lang"
    ; string (Syntax.name Dune_lang.Pkg.syntax)
    ; Dune_lang.Syntax.Version.encode t.version
    ]

let file_contents_by_path t =
  (metadata, [ encode_metadata t ])
  :: (Package_name.Map.to_list t.packages
     |> List.map ~f:(fun (name, pkg) ->
            (Package_name.to_string name, Pkg.encode pkg)))

(* Checks whether path refers to a valid lock directory and returns a value
   indicating the status of the lock directory. [Ok _] values indicate that
   it's safe to proceed with regenerating the lock directory. [Error _]
   values indicate that it's unsafe to remove the existing directory and lock
   directory regeneration should not proceed. *)
let check_existing_lock_dir path =
  match Path.exists path with
  | false -> Ok `Non_existant
  | true -> (
    match Path.is_directory path with
    | false -> Error `Not_directory
    | true -> (
      let metadata_path = Path.relative path metadata in
      match
        Path.exists metadata_path && not (Path.is_directory metadata_path)
      with
      | false -> Error `No_metadata_file
      | true -> (
        match
          Metadata.load metadata_path
            ~f:(Fun.const (Dune_lang.Decoder.return ()))
        with
        | Ok () -> Ok `Is_existing_lock_dir
        | Error exn -> Error (`Failed_to_parse_metadata exn))))

(* Removes the exitsing lock directory at the specified path if it exists and
   is a valid lock directory *)
let safely_remove_lock_dir_if_exists path =
  match check_existing_lock_dir path with
  | Ok `Non_existant -> ()
  | Ok `Is_existing_lock_dir -> Path.rm_rf path
  | Error e ->
    let error_reason_pp =
      match e with
      | `Not_directory -> Pp.text "Specified lock dir path is not a directory"
      | `No_metadata_file ->
        Pp.textf "Specified lock dir lacks metadata file (%s)" metadata
      | `Failed_to_parse_metadata exn -> Exn.pp exn
    in
    User_error.raise
      [ Pp.textf "Refusing to regenerate lock directory %s"
          (Path.to_string_maybe_quoted path)
      ; error_reason_pp
      ]

let write_disk ~lock_dir_path t =
  let lock_dir_path = Path.source lock_dir_path in
  let () = safely_remove_lock_dir_if_exists lock_dir_path in
  Path.mkdir_p lock_dir_path;
  file_contents_by_path t
  |> List.iter ~f:(fun (path_within_lock_dir, contents) ->
         let path = Path.relative lock_dir_path path_within_lock_dir in
         Option.iter (Path.parent path) ~f:Path.mkdir_p;
         let contents_string =
           List.map contents ~f:Dune_lang.to_string |> String.concat ~sep:"\n"
         in
         Io.write_file path contents_string)

let load_metadata_version source_path =
  let open Or_exn.O in
  let* syntax, version =
    Metadata.load (Path.source source_path)
      ~f:(fun { Metadata.Lang.Instance.syntax; data = (); version } ->
        Dune_lang.Decoder.return (syntax, version))
  in
  if String.equal (Syntax.name syntax) (Syntax.name Dune_lang.Pkg.syntax) then
    Ok version
  else
    Error
      User_error.(
        E
          (make
             [ Pp.textf "In %s, expected language to be %s, but found %s"
                 (Path.Source.to_string source_path)
                 (Syntax.name Dune_lang.Pkg.syntax)
                 (Syntax.name syntax)
             ]))

let load_pkg ~parser_context ~lock_dir_path package_name =
  let open Or_exn.O in
  let+ mk_pkg =
    Result.try_with (fun () ->
        let source_path =
          Path.Source.relative lock_dir_path
            (Package_name.to_string package_name)
        in
        let pkg_string = Io.read_file (Path.source source_path) in
        let ast =
          Dune_lang.Parser.parse_string pkg_string ~mode:Many_as_one
            ~fname:(Path.Source.to_string source_path)
        in
        Dune_lang.Decoder.parse Pkg.decode parser_context ast)
  in
  mk_pkg ~lock_dir:lock_dir_path package_name

let read_disk ~lock_dir_path =
  let open Or_exn.O in
  let* () =
    if Path.is_directory (Path.source lock_dir_path) then Ok ()
    else
      Error
        User_error.(
          E
            (make
               [ Pp.textf "%s is not a directory"
                   (Path.Source.to_string lock_dir_path)
               ]))
  in
  let* version =
    load_metadata_version (Path.Source.relative lock_dir_path metadata)
  in
  let parser_context =
    Univ_map.singleton String_with_vars.decoding_env_key
      (Pform.Env.initial version)
  in
  let+ packages =
    Sys.readdir (Path.Source.to_string lock_dir_path)
    |> Array.to_list
    |> List.filter_map ~f:(fun filename ->
           if Filename.equal filename metadata then None
           else
             let package_path = Path.Source.relative lock_dir_path filename in
             if Path.is_directory (Path.source package_path) then None
             else
               let package_name = Package_name.of_string filename in
               Some
                 ( load_pkg ~parser_context ~lock_dir_path package_name
                 >>| fun pkg -> (package_name, pkg) ))
    |> Result.List.all >>| Package_name.Map.of_list_exn
  in
  { version; packages }
