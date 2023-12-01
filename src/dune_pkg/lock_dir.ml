open Import

module Source = struct
  type fetch =
    { url : Loc.t * string
    ; checksum : (Loc.t * Checksum.t) option
    }

  type t =
    | External_copy of Loc.t * Path.External.t
    | Fetch of fetch

  let remove_locs = function
    | External_copy (_loc, path) -> External_copy (Loc.none, path)
    | Fetch { url = _loc, url; checksum } ->
      Fetch
        { url = Loc.none, url
        ; checksum = Option.map checksum ~f:(fun (_loc, checksum) -> Loc.none, checksum)
        }
  ;;

  let equal a b =
    match a, b with
    | External_copy (loc, path), External_copy (other_loc, other_path) ->
      Loc.equal loc other_loc && Path.External.equal path other_path
    | ( Fetch { url = loc, url; checksum }
      , Fetch { url = other_loc, other_url; checksum = other_checksum } ) ->
      Loc.equal loc other_loc
      && String.equal url other_url
      && Option.equal
           (fun (loc, checksum) (other_loc, other_checksum) ->
             Loc.equal loc other_loc && Checksum.equal checksum other_checksum)
           checksum
           other_checksum
    | _ -> false
  ;;

  let to_dyn = function
    | External_copy (_loc, path) ->
      Dyn.variant "External_copy" [ Path.External.to_dyn path ]
    | Fetch { url = _loc, url; checksum } ->
      Dyn.variant
        "Fetch"
        [ Dyn.string url
        ; Dyn.option (fun (_loc, checksum) -> Checksum.to_dyn checksum) checksum
        ]
  ;;

  module Fields = struct
    let copy = "copy"
    let fetch = "fetch"
    let url = "url"
    let checksum = "checksum"
  end

  let decode_fetch =
    let open Decoder in
    let+ url = field Fields.url (located string)
    and+ checksum = field_o Fields.checksum (located string) in
    let checksum =
      match checksum with
      | None -> None
      | Some ((loc, _) as checksum) ->
        let checksum = Checksum.of_string_user_error checksum |> User_error.ok_exn in
        Some (loc, checksum)
    in
    { url; checksum }
  ;;

  let decode =
    let open Decoder in
    sum
      [ ( Fields.copy
        , located string
          >>| fun (loc, source) path ->
          External_copy
            ( loc
            , if Filename.is_relative source
              then Path.External.relative path source
              else Path.External.of_string source ) )
      ; ( Fields.fetch
        , let+ fetch = fields decode_fetch in
          fun _ -> Fetch fetch )
      ]
  ;;

  let encode_fetch_field { url = _loc, url; checksum } =
    let open Encoder in
    [ field Fields.url string url
    ; field_o Fields.checksum Checksum.encode (Option.map checksum ~f:snd)
    ]
  ;;

  let encode t =
    let open Encoder in
    match t with
    | External_copy (_loc, path) ->
      constr Fields.copy string (Path.External.to_string path)
    | Fetch fetch -> named_record_fields Fields.fetch (encode_fetch_field fetch)
  ;;
end

module Pkg_info = struct
  type t =
    { name : Package_name.t
    ; version : Package_version.t
    ; dev : bool
    ; source : Source.t option
    ; extra_sources : (Path.Local.t * Source.t) list
    }

  let equal { name; version; dev; source; extra_sources } t =
    Package_name.equal name t.name
    && Package_version.equal version t.version
    && Bool.equal dev t.dev
    && Option.equal Source.equal source t.source
    && List.equal
         (Tuple.T2.equal Path.Local.equal Source.equal)
         extra_sources
         t.extra_sources
  ;;

  let remove_locs t =
    { t with
      source = Option.map ~f:Source.remove_locs t.source
    ; extra_sources =
        List.map t.extra_sources ~f:(fun (local, source) ->
          local, Source.remove_locs source)
    }
  ;;

  let to_dyn { name; version; dev; source; extra_sources } =
    Dyn.record
      [ "name", Package_name.to_dyn name
      ; "version", Package_version.to_dyn version
      ; "dev", Dyn.bool dev
      ; "source", Dyn.option Source.to_dyn source
      ; "extra_sources", Dyn.list (Dyn.pair Path.Local.to_dyn Source.to_dyn) extra_sources
      ]
  ;;

  let default_version = Package_version.of_string "dev"
end

module Pkg = struct
  type t =
    { build_command : Action.t option
    ; install_command : Action.t option
    ; deps : (Loc.t * Package_name.t) list
    ; info : Pkg_info.t
    ; exported_env : String_with_vars.t Action.Env_update.t list
    }

  let equal { build_command; install_command; deps; info; exported_env } t =
    Option.equal Action.equal_no_locs build_command t.build_command
    && Option.equal Action.equal_no_locs install_command t.install_command
    && List.equal (Tuple.T2.equal Loc.equal Package_name.equal) deps t.deps
    && Pkg_info.equal info t.info
    && List.equal
         (Action.Env_update.equal String_with_vars.equal)
         exported_env
         t.exported_env
  ;;

  let remove_locs t =
    { t with
      info = Pkg_info.remove_locs t.info
    ; exported_env =
        List.map t.exported_env ~f:(Action.Env_update.map ~f:String_with_vars.remove_locs)
    ; deps = List.map t.deps ~f:(fun (_, pkg) -> Loc.none, pkg)
    }
  ;;

  let to_dyn { build_command; install_command; deps; info; exported_env } =
    Dyn.record
      [ "build_command", Dyn.option Action.to_dyn build_command
      ; "install_command", Dyn.option Action.to_dyn install_command
      ; "deps", Dyn.list (Dyn.pair Loc.to_dyn_hum Package_name.to_dyn) deps
      ; "info", Pkg_info.to_dyn info
      ; ( "exported_env"
        , Dyn.list (Action.Env_update.to_dyn String_with_vars.to_dyn) exported_env )
      ]
  ;;

  module Fields = struct
    let version = "version"
    let install = "install"
    let build = "build"
    let deps = "deps"
    let source = "source"
    let dev = "dev"
    let exported_env = "exported_env"
    let extra_sources = "extra_sources"
  end

  let decode =
    let open Decoder in
    enter
    @@ fields
    @@ let+ version =
         field ~default:Pkg_info.default_version Fields.version Package_version.decode
       and+ install_command = field_o Fields.install Action.decode_pkg
       and+ build_command = field_o Fields.build Action.decode_pkg
       and+ deps = field ~default:[] Fields.deps (repeat (located Package_name.decode))
       and+ source = field_o Fields.source Source.decode
       and+ dev = field_b Fields.dev
       and+ exported_env =
         field Fields.exported_env ~default:[] (repeat Action.Env_update.decode)
       and+ extra_sources =
         field
           Fields.extra_sources
           ~default:[]
           (repeat (pair (plain_string Path.Local.parse_string_exn) Source.decode))
       in
       fun ~lock_dir name ->
         let info =
           let make_source f =
             Path.source lock_dir
             |> Path.to_absolute_filename
             |> Path.External.of_string
             |> f
           in
           let source = Option.map source ~f:make_source in
           let extra_sources =
             List.map extra_sources ~f:(fun (path, source) -> path, make_source source)
           in
           { Pkg_info.name; version; dev; source; extra_sources }
         in
         { build_command; deps; install_command; info; exported_env }
  ;;

  let encode_extra_source (local, source) : Dune_sexp.t =
    List
      [ Dune_sexp.atom_or_quoted_string (Path.Local.to_string local)
      ; Source.encode source
      ]
  ;;

  let encode
    { build_command
    ; install_command
    ; deps
    ; info = { Pkg_info.name = _; extra_sources; version; dev; source }
    ; exported_env
    }
    =
    let open Encoder in
    record_fields
      [ field Fields.version Package_version.encode version
      ; field_o Fields.install Action.encode install_command
      ; field_o Fields.build Action.encode build_command
      ; field_l Fields.deps Package_name.encode (List.map deps ~f:snd)
      ; field_o Fields.source Source.encode source
      ; field_b Fields.dev dev
      ; field_l Fields.exported_env Action.Env_update.encode exported_env
      ; field_l Fields.extra_sources encode_extra_source extra_sources
      ]
  ;;
end

module Repositories = struct
  type t =
    { complete : bool
    ; used : Opam_repo.Serializable.t list option
    }

  let default = { complete = false; used = None }

  let equal { complete; used } t =
    Bool.equal complete t.complete
    && Option.equal (List.equal Opam_repo.Serializable.equal) used t.used
  ;;

  let to_dyn { complete; used } =
    Dyn.record
      [ "complete", Dyn.bool complete
      ; "used", Dyn.option (Dyn.list Opam_repo.Serializable.to_dyn) used
      ]
  ;;

  let encode_used used =
    let open Encoder in
    List.map ~f:(fun repo -> list sexp @@ Opam_repo.Serializable.encode repo) used
  ;;

  let encode { complete; used } =
    let open Encoder in
    let base = list sexp [ string "complete"; bool complete ] in
    [ base ]
    @
    match used with
    | None -> []
    | Some [] -> [ list sexp [ string "used" ] ]
    | Some used -> [ list sexp (string "used" :: encode_used used) ]
  ;;

  let decode =
    let open Decoder in
    fields
      (let+ complete = field "complete" bool
       and+ used = field_o "used" (repeat (enter Opam_repo.Serializable.decode)) in
       { complete; used })
  ;;
end

type t =
  { version : Syntax.Version.t
  ; dependency_hash : (Loc.t * Local_package.Dependency_hash.t) option
  ; packages : Pkg.t Package_name.Map.t
  ; ocaml : (Loc.t * Package_name.t) option
  ; repos : Repositories.t
  ; expanded_solver_variable_bindings : Solver_stats.Expanded_variable_bindings.t
  }

let remove_locs t =
  { t with
    packages = Package_name.Map.map t.packages ~f:Pkg.remove_locs
  ; ocaml = Option.map t.ocaml ~f:(fun (_, ocaml) -> Loc.none, ocaml)
  }
;;

let equal
  { version; dependency_hash; packages; ocaml; repos; expanded_solver_variable_bindings }
  t
  =
  Syntax.Version.equal version t.version
  && Option.equal
       (Tuple.T2.equal Loc.equal Local_package.Dependency_hash.equal)
       dependency_hash
       t.dependency_hash
  && Option.equal (Tuple.T2.equal Loc.equal Package_name.equal) ocaml t.ocaml
  && Repositories.equal repos t.repos
  && Package_name.Map.equal packages t.packages ~equal:Pkg.equal
  && Solver_stats.Expanded_variable_bindings.equal
       expanded_solver_variable_bindings
       t.expanded_solver_variable_bindings
;;

let to_dyn
  { version; dependency_hash; packages; ocaml; repos; expanded_solver_variable_bindings }
  =
  Dyn.record
    [ "version", Syntax.Version.to_dyn version
    ; ( "dependency_hash"
      , Dyn.option
          (Tuple.T2.to_dyn Loc.to_dyn_hum Local_package.Dependency_hash.to_dyn)
          dependency_hash )
    ; "packages", Package_name.Map.to_dyn Pkg.to_dyn packages
    ; "ocaml", Dyn.option (Tuple.T2.to_dyn Loc.to_dyn_hum Package_name.to_dyn) ocaml
    ; "repos", Repositories.to_dyn repos
    ; ( "expanded_solver_variable_bindings"
      , Solver_stats.Expanded_variable_bindings.to_dyn expanded_solver_variable_bindings )
    ]
;;

type missing_dependency =
  { dependant_package : Pkg.t
  ; dependency : Package_name.t
  ; loc : Loc.t
  }

(* [validate_packages packages] returns
   [Error (`Missing_dependencies missing_dependencies)] where
   [missing_dependencies] is a non-empty list with an element for each package
   dependency which doesn't have a corresponding entry in [packages]. *)
let validate_packages packages =
  let missing_dependencies =
    Package_name.Map.values packages
    |> List.concat_map ~f:(fun (dependant_package : Pkg.t) ->
      List.filter_map dependant_package.deps ~f:(fun (loc, dependency) ->
        if Package_name.Map.mem packages dependency
        then None
        else Some { dependant_package; dependency; loc }))
  in
  if List.is_empty missing_dependencies
  then Ok ()
  else Error (`Missing_dependencies missing_dependencies)
;;

let create_latest_version
  packages
  ~local_packages
  ~ocaml
  ~repos
  ~expanded_solver_variable_bindings
  =
  (match validate_packages packages with
   | Ok () -> ()
   | Error (`Missing_dependencies missing_dependencies) ->
     Code_error.raise
       "Invalid package table"
       (List.map
          missing_dependencies
          ~f:(fun { dependant_package; dependency; loc = _ } ->
            ( "missing dependency"
            , Dyn.record
                [ "missing package", Package_name.to_dyn dependency
                ; "dependency of", Package_name.to_dyn dependant_package.info.name
                ] ))));
  let version = Syntax.greatest_supported_version Dune_lang.Pkg.syntax in
  let dependency_hash =
    Local_package.(
      Dependency_set.hash (For_solver.list_non_local_dependency_set local_packages))
    |> Option.map ~f:(fun dependency_hash -> Loc.none, dependency_hash)
  in
  let complete, used =
    match repos with
    | None -> true, None
    | Some repos ->
      let used = List.filter_map repos ~f:Opam_repo.serializable in
      let complete = Int.equal (List.length repos) (List.length used) in
      complete, Some used
  in
  let repos : Repositories.t = { complete; used } in
  let t =
    { version
    ; dependency_hash
    ; packages
    ; ocaml
    ; repos
    ; expanded_solver_variable_bindings
    }
  in
  t
;;

let default_path = Path.Source.(relative root "dune.lock")
let metadata_filename = "lock.dune"

module Metadata = Dune_sexp.Versioned_file.Make (Unit)

let () = Metadata.Lang.register Dune_lang.Pkg.syntax ()

let encode_metadata
  { version
  ; dependency_hash
  ; ocaml
  ; repos
  ; packages = _
  ; expanded_solver_variable_bindings
  }
  =
  let open Encoder in
  let base =
    list
      sexp
      [ string "lang"
      ; string (Syntax.name Dune_lang.Pkg.syntax)
      ; Syntax.Version.encode version
      ]
  in
  [ base ]
  @ (match dependency_hash with
     | None -> []
     | Some (_loc, dependency_hash) ->
       [ list
           sexp
           [ string "dependency_hash"
           ; Local_package.Dependency_hash.encode dependency_hash
           ]
       ])
  @ (match ocaml with
     | None -> []
     | Some ocaml -> [ list sexp [ string "ocaml"; Package_name.encode (snd ocaml) ] ])
  @ [ list sexp (string "repositories" :: Repositories.encode repos) ]
  @
  if Solver_stats.Expanded_variable_bindings.is_empty expanded_solver_variable_bindings
  then []
  else
    [ list
        sexp
        (string "expanded_solver_variable_bindings"
         :: Solver_stats.Expanded_variable_bindings.encode
              expanded_solver_variable_bindings)
    ]
;;

let decode_metadata =
  let open Decoder in
  fields
    (let+ ocaml = field_o "ocaml" (located Package_name.decode)
     and+ dependency_hash =
       field_o "dependency_hash" (located Local_package.Dependency_hash.decode)
     and+ repos = field "repositories" ~default:Repositories.default Repositories.decode
     and+ expanded_solver_variable_bindings =
       field
         "expanded_solver_variable_bindings"
         ~default:Solver_stats.Expanded_variable_bindings.empty
         Solver_stats.Expanded_variable_bindings.decode
     in
     ocaml, dependency_hash, repos, expanded_solver_variable_bindings)
;;

module Package_filename = struct
  let file_extension = ".pkg"
  let of_package_name package_name = Package_name.to_string package_name ^ file_extension

  let to_package_name package_filename =
    if String.equal (Filename.extension package_filename) file_extension
    then Ok (Filename.chop_extension package_filename |> Package_name.of_string)
    else Error `Bad_extension
  ;;
end

let file_contents_by_path t =
  (metadata_filename, encode_metadata t)
  :: (Package_name.Map.to_list t.packages
      |> List.map ~f:(fun (name, pkg) ->
        Package_filename.of_package_name name, Pkg.encode pkg))
;;

module Write_disk = struct
  (* Checks whether path refers to a valid lock directory and returns a value
     indicating the status of the lock directory. [Ok _] values indicate that
     it's safe to proceed with regenerating the lock directory. [Error _]
     values indicate that it's unsafe to remove the existing directory and lock
     directory regeneration should not proceed. *)
  let check_existing_lock_dir path =
    match Path.stat path with
    | Ok { st_kind = S_DIR; _ } ->
      let metadata_path = Path.relative path metadata_filename in
      (match Path.stat metadata_path with
       | Ok { st_kind = S_REG; _ } ->
         (match Metadata.load metadata_path ~f:(Fun.const decode_metadata) with
          | Ok _unused -> Ok `Is_existing_lock_dir
          | Error exn -> Error (`Failed_to_parse_metadata (metadata_path, exn)))
       | _ -> Error `No_metadata_file)
    | Error (Unix.ENOENT, _, _) -> Ok `Non_existant
    | Error _ -> Error `Unreadable
    | Ok _ -> Error `Not_directory
  ;;

  (* Removes the exitsing lock directory at the specified path if it exists and
     is a valid lock directory. Checks the validity of the existing lockdir (if
     any) and raises if it's invalid before constructing the returned thunk, so
     validation can happen separately from executing the side effect that removes
     the directory. *)
  let safely_remove_lock_dir_if_exists_thunk path =
    match check_existing_lock_dir path with
    | Ok `Non_existant -> Fun.const ()
    | Ok `Is_existing_lock_dir -> fun () -> Path.rm_rf path
    | Error e ->
      let error_reason_pp =
        match e with
        | `Unreadable -> Pp.text "Unable to read lock directory"
        | `Not_directory -> Pp.text "Specified lock dir path is not a directory"
        | `No_metadata_file ->
          Pp.textf "Specified lock dir lacks metadata file (%s)" metadata_filename
        | `Failed_to_parse_metadata (path, exn) ->
          Pp.concat
            ~sep:Pp.newline
            [ Pp.textf
                "Unable to parse lock directory metadata file (%s):"
                (Path.to_string_maybe_quoted path)
            ; Exn.pp exn
            ]
      in
      User_error.raise
        [ Pp.textf
            "Refusing to regenerate lock directory %s"
            (Path.to_string_maybe_quoted path)
        ; error_reason_pp
        ]
  ;;

  type t = unit -> unit

  let prepare ~lock_dir_path ~files lock_dir =
    let lock_dir_path = Path.source lock_dir_path in
    let remove_dir_if_exists = safely_remove_lock_dir_if_exists_thunk lock_dir_path in
    fun () ->
      remove_dir_if_exists ();
      Path.mkdir_p lock_dir_path;
      file_contents_by_path lock_dir
      |> List.iter ~f:(fun (path_within_lock_dir, contents) ->
        let path = Path.relative lock_dir_path path_within_lock_dir in
        Option.iter (Path.parent path) ~f:Path.mkdir_p;
        let cst =
          List.map contents ~f:(fun sexp ->
            Dune_sexp.Ast.add_loc ~loc:Loc.none sexp |> Dune_sexp.Cst.concrete)
        in
        let pp = Dune_lang.Format.pp_top_sexps ~version:(3, 11) cst in
        Format.asprintf "%a" Pp.to_fmt pp |> Io.write_file path;
        Package_name.Map.iteri files ~f:(fun package_name files ->
          let files_dir =
            Path.relative lock_dir_path (Package_name.to_string package_name ^ ".files")
          in
          Path.mkdir_p files_dir;
          List.iter files ~f:(fun { File_entry.original; local_file } ->
            let dst = Path.append_local files_dir local_file in
            Path.mkdir_p (Path.parent_exn dst);
            match original with
            | Path src -> Io.copy_file ~src ~dst ()
            | Content content -> Io.write_file dst content)))
  ;;

  let commit t = t ()
end

module Make_load (Io : sig
    include Monad.S

    val parallel_map : 'a list -> f:('a -> 'b t) -> 'b list t
    val readdir_with_kinds : Path.Source.t -> (Filename.t * Unix.file_kind) list t
    val with_lexbuf_from_file : Path.Source.t -> f:(Lexing.lexbuf -> 'a) -> 'a t
    val stats_kind : Path.Source.t -> (File_kind.t, Unix_error.Detailed.t) result t
  end) =
struct
  let load_metadata metadata_file_path =
    let open Io.O in
    let+ syntax, version, dependency_hash, ocaml, repos, expanded_solver_variable_bindings
      =
      Io.with_lexbuf_from_file metadata_file_path ~f:(fun lexbuf ->
        Metadata.parse_contents
          lexbuf
          ~f:(fun { Metadata.Lang.Instance.syntax; data = (); version } ->
            let open Decoder in
            let+ ocaml, dependency_hash, repos, expanded_solver_variable_bindings =
              decode_metadata
            in
            ( syntax
            , version
            , dependency_hash
            , ocaml
            , repos
            , expanded_solver_variable_bindings )))
    in
    if String.equal (Syntax.name syntax) (Syntax.name Dune_lang.Pkg.syntax)
    then version, dependency_hash, ocaml, repos, expanded_solver_variable_bindings
    else
      User_error.raise
        [ Pp.textf
            "In %s, expected language to be %s, but found %s"
            (Path.Source.to_string metadata_file_path)
            (Syntax.name Dune_lang.Pkg.syntax)
            (Syntax.name syntax)
        ]
  ;;

  let load_pkg ~version ~lock_dir_path package_name =
    let open Io.O in
    let pkg_file_path =
      Path.Source.relative lock_dir_path (Package_filename.of_package_name package_name)
    in
    let+ sexp =
      Io.with_lexbuf_from_file pkg_file_path ~f:(Dune_sexp.Parser.parse ~mode:Many)
    in
    let parser =
      let env = Pform.Env.pkg version in
      let decode =
        Syntax.set Dune_lang.Pkg.syntax (Active version) Pkg.decode
        |> Syntax.set Dune_lang.Stanza.syntax (Active Dune_lang.Stanza.latest_version)
      in
      String_with_vars.set_decoding_env env decode
    in
    (Decoder.parse parser Univ_map.empty (List (Loc.none, sexp)))
      ~lock_dir:lock_dir_path
      package_name
  ;;

  let check_path lock_dir_path =
    let open Io.O in
    Io.stats_kind lock_dir_path
    >>| function
    | Ok S_DIR -> ()
    | Error (Unix.ENOENT, _, _) ->
      User_error.raise
        ~hints:
          [ Pp.concat
              ~sep:Pp.space
              [ Pp.text "Run"
              ; User_message.command "dune pkg lock"
              ; Pp.text "to generate it."
              ]
            |> Pp.hovbox
          ]
        [ Pp.textf "%s does not exist." (Path.Source.to_string lock_dir_path) ]
    | Error e ->
      User_error.raise
        [ Pp.textf "%s is not accessible" (Path.Source.to_string lock_dir_path)
        ; Pp.textf "reason: %s" (Unix_error.Detailed.to_string_hum e)
        ]
    | _ ->
      User_error.raise
        [ Pp.textf "%s is not a directory." (Path.Source.to_string lock_dir_path) ]
  ;;

  let check_packages packages ~lock_dir_path =
    match validate_packages packages with
    | Ok () -> ()
    | Error (`Missing_dependencies missing_dependencies) ->
      List.iter missing_dependencies ~f:(fun { dependant_package; dependency; loc } ->
        User_message.prerr
          (User_message.make
             ~loc
             [ Pp.textf
                 "The package %S depends on the package %S, but %S does not appear in \
                  the lockdir %s."
                 (Package_name.to_string dependant_package.info.name)
                 (Package_name.to_string dependency)
                 (Package_name.to_string dependency)
                 (Path.Source.to_string_maybe_quoted lock_dir_path)
             ]));
      User_error.raise
        ~hints:
          [ Pp.concat
              ~sep:Pp.space
              [ Pp.text
                  "This could indicate that the lockdir is corrupted. Delete it and then \
                   regenerate it by running:"
              ; User_message.command "dune pkg lock"
              ]
          ]
        [ Pp.textf
            "At least one package dependency is itself not present as a package in the \
             lockdir %s."
            (Path.Source.to_string_maybe_quoted lock_dir_path)
        ]
  ;;

  let load lock_dir_path =
    let open Io.O in
    let* () = check_path lock_dir_path in
    let* version, dependency_hash, ocaml, repos, expanded_solver_variable_bindings =
      load_metadata (Path.Source.relative lock_dir_path metadata_filename)
    in
    let+ packages =
      Io.readdir_with_kinds lock_dir_path
      >>| List.filter_map ~f:(fun (name, (kind : Unix.file_kind)) ->
        match kind with
        | S_REG -> Package_filename.to_package_name name |> Result.to_option
        | _ ->
          (* TODO *)
          None)
      >>= Io.parallel_map ~f:(fun package_name ->
        let+ pkg = load_pkg ~version ~lock_dir_path package_name in
        package_name, pkg)
      >>| Package_name.Map.of_list_exn
    in
    check_packages packages ~lock_dir_path;
    { version
    ; dependency_hash
    ; packages
    ; ocaml
    ; repos
    ; expanded_solver_variable_bindings
    }
  ;;
end

module Load_immediate = Make_load (struct
    include Monad.Id

    let stats_kind file =
      Path.source file |> Path.stat |> Result.map ~f:(fun { Unix.st_kind; _ } -> st_kind)
    ;;

    let parallel_map xs ~f = List.map xs ~f

    let readdir_with_kinds path =
      match Path.readdir_unsorted_with_kinds (Path.source path) with
      | Ok entries -> entries
      | Error e ->
        User_error.raise
          [ Pp.text (Dune_filesystem_stubs.Unix_error.Detailed.to_string_hum e) ]
    ;;

    let with_lexbuf_from_file path ~f = Io.with_lexbuf_from_file (Path.source path) ~f
  end)

let read_disk = Load_immediate.load

let transitive_dependency_closure t start =
  let missing_packages =
    let all_packages_in_lock_dir = Package_name.Set.of_keys t.packages in
    Package_name.Set.diff start all_packages_in_lock_dir
  in
  match Package_name.Set.is_empty missing_packages with
  | false -> Error (`Missing_packages missing_packages)
  | true ->
    let to_visit = Queue.create () in
    let push_set = Package_name.Set.iter ~f:(Queue.push to_visit) in
    push_set start;
    let rec loop seen =
      match Queue.pop to_visit with
      | None -> seen
      | Some node ->
        let unseen_deps =
          (* Note that the call to find_exn won't raise because [t] guarantees
             that its map of dependencies is closed under "depends on". *)
          Package_name.Set.(
            diff
              (of_list_map (Package_name.Map.find_exn t.packages node).deps ~f:snd)
              seen)
        in
        push_set unseen_deps;
        loop (Package_name.Set.union seen unseen_deps)
    in
    Ok (loop start)
;;
