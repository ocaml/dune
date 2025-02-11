open Import

module Conditional = struct
  type 'a t =
    { condition : Solver_env.t
    ; value : 'a
    }

  let make condition value =
    let condition = Solver_env.retain condition Package_variable_name.platform_specific in
    { condition; value }
  ;;

  let equal value_equal { condition; value } t =
    Solver_env.equal condition t.condition && value_equal value t.value
  ;;

  let to_dyn value_to_dyn { condition; value } =
    Dyn.record [ "condition", Solver_env.to_dyn condition; "value", value_to_dyn value ]
  ;;

  let decode value_decode =
    let open Decoder in
    enter
      (let+ condition = enter Solver_env.decode
       and+ value = value_decode in
       { condition; value })
  ;;

  let encode value_encode { condition; value } =
    Dune_lang.List [ Solver_env.encode condition; value_encode value ]
  ;;

  let map t ~f = { t with value = f t.value }
  let condition { condition; _ } = condition
  let get { value; _ } = value

  let matches t ~query =
    Solver_env.fold t.condition ~init:true ~f:(fun variable stored_value acc ->
      acc
      &&
      match Solver_env.get query variable with
      | None ->
        (* The stored env has a field missing from the query. Don't match in this case. *)
        false
      | Some query_value -> Variable_value.equal query_value stored_value)
  ;;
end

module Conditional_choice = struct
  type 'a t = 'a Conditional.t list

  let empty = []
  let singleton condition value = [ Conditional.make condition value ]
  let singleton_all_platforms value = singleton Solver_env.empty value
  let equal value_equal = List.equal (Conditional.equal value_equal)
  let map ~f = List.map ~f:(Conditional.map ~f)
  let to_dyn value_to_dyn = Dyn.list (Conditional.to_dyn value_to_dyn)

  let find t query =
    List.find_map t ~f:(fun conditional ->
      if Conditional.matches conditional ~query
      then Some (Conditional.get conditional)
      else None)
  ;;

  let condition_exists t query =
    List.exists t ~f:(fun conditional -> Conditional.matches conditional ~query)
  ;;

  let encode_field field_name value_encode t =
    Encoder.field_l field_name (Conditional.encode value_encode) t
  ;;

  (* Concatenates a pair of sets of choices, raising a code error if the pair
     has a condition in common. *)
  let merge a b =
    let merged = a @ b in
    let () =
      List.map merged ~f:(fun { Conditional.condition; _ } -> condition, ())
      |> Solver_env.Map.of_list_fold ~init:0 ~f:(fun count _ -> count + 1)
      |> Solver_env.Map.iteri ~f:(fun solver_env count ->
        if count > 1
        then
          Code_error.raise
            "Both sets of conditional choices had a condition in common."
            [ "condition", Solver_env.to_dyn solver_env ])
    in
    merged
  ;;

  (* To support encoding in the non-portable format, this function extracts the
     sole value from a conditional choice, raising a code error if there are
     multiple choices. *)
  let get_value_ensuring_at_most_one_choice t =
    if List.length t > 1
    then
      Code_error.raise
        "Expected at most one conditional choice"
        [ "conditions", List.map t ~f:Conditional.condition |> Dyn.list Solver_env.to_dyn
        ];
    List.hd_opt t |> Option.map ~f:Conditional.get
  ;;

  let decode_backwards_compatible decode_value =
    let open Decoder in
    decode_value >>| singleton_all_platforms <|> repeat (Conditional.decode decode_value)
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

  let variables t =
    let module Variable = OpamVariable in
    Package_variable_name.Map.of_list_exn
      [ Package_variable_name.name, Variable.S (Package_name.to_string t.name)
      ; Package_variable_name.version, S (Package_version.to_string t.version)
      ; Package_variable_name.dev, B t.dev
      ]
  ;;
end

module Build_command = struct
  type t =
    | Action of Action.t
    | Dune

  let equal x y =
    match x, y with
    | Dune, Dune -> true
    | Action x, Action y -> Action.equal x y
    | _, _ -> false
  ;;

  let remove_locs = function
    | Dune -> Dune
    | Action a -> Action (Action.remove_locs a)
  ;;

  let to_dyn = function
    | Dune -> Dyn.variant "Dune" []
    | Action a -> Dyn.variant "Action" [ Action.to_dyn a ]
  ;;

  module Fields = struct
    let dune = "dune"
    let action = "action"
    let build = "build"
  end

  let encode_non_portable t =
    let open Encoder in
    match t with
    | None -> field_o Fields.build Encoder.unit None
    | Some Dune -> field_b Fields.dune true
    | Some (Action a) -> field Fields.build Action.encode a
  ;;

  let encode_portable t =
    let open Encoder in
    Dune_lang.List
      (record_fields
         [ (match t with
            | Dune -> field_b Fields.dune true
            | Action a -> field Fields.action Action.encode a)
         ])
  ;;

  let decode_portable =
    let open Decoder in
    enter
    @@ fields
    @@ fields_mutually_exclusive
         [ ( Fields.action
           , let+ pkg = Action.decode_pkg in
             Action pkg )
         ; ( Fields.dune
           , let+ () = return () in
             Dune )
         ]
  ;;

  let decode_fields_backwards_compatible =
    let open Decoder in
    let parse_action =
      (let+ action = Action.decode_pkg in
       Conditional_choice.singleton_all_platforms (Action action))
      <|> repeat (Conditional.decode decode_portable)
    in
    fields_mutually_exclusive
      ~default:Conditional_choice.empty
      [ Fields.build, parse_action
      ; ( Fields.dune
        , let+ () = return () in
          Conditional_choice.singleton_all_platforms Dune )
      ]
  ;;
end

module Depend = struct
  type t =
    { loc : Loc.t
    ; name : Package_name.t
    }

  let equal { loc; name } t = Loc.equal loc t.loc && Package_name.equal name t.name
  let remove_locs { name; loc = _ } = { name; loc = Loc.none }

  let to_dyn { loc; name } =
    Dyn.record [ "loc", Loc.to_dyn_hum loc; "name", Package_name.to_dyn name ]
  ;;

  let decode =
    let open Decoder in
    let+ loc, name = located Package_name.decode in
    { loc; name }
  ;;

  let encode { name; loc = _ } = Package_name.encode name
end

module Depends = struct
  type t = Depend.t list

  let equal = List.equal Depend.equal
  let remove_locs = List.map ~f:Depend.remove_locs
  let to_dyn = Dyn.list Depend.to_dyn

  let decode =
    let open Decoder in
    enter @@ repeat Depend.decode
  ;;

  let encode t = Dune_lang.List (List.map t ~f:Depend.encode)
end

module Pkg = struct
  type t =
    { build_command : Build_command.t Conditional_choice.t
    ; install_command : Action.t Conditional_choice.t
    ; depends : Depends.t Conditional_choice.t
    ; depexts : string list Conditional_choice.t
    ; info : Pkg_info.t
    ; exported_env : String_with_vars.t Action.Env_update.t list
    }

  let equal { build_command; install_command; depends; depexts; info; exported_env } t =
    Conditional_choice.equal Build_command.equal build_command t.build_command
    (* CR-rgrinberg: why do we ignore locations? *)
    && Conditional_choice.equal Action.equal_no_locs install_command t.install_command
    && Conditional_choice.equal Depends.equal depends t.depends
    && Conditional_choice.equal (List.equal String.equal) depexts t.depexts
    && Pkg_info.equal info t.info
    && List.equal
         (Action.Env_update.equal String_with_vars.equal)
         exported_env
         t.exported_env
  ;;

  let remove_locs { build_command; install_command; depends; depexts; info; exported_env }
    =
    { info = Pkg_info.remove_locs info
    ; exported_env =
        List.map exported_env ~f:(Action.Env_update.map ~f:String_with_vars.remove_locs)
    ; depends = Conditional_choice.map depends ~f:Depends.remove_locs
    ; depexts
    ; build_command = Conditional_choice.map build_command ~f:Build_command.remove_locs
    ; install_command = Conditional_choice.map install_command ~f:Action.remove_locs
    }
  ;;

  let to_dyn { build_command; install_command; depends; depexts; info; exported_env } =
    Dyn.record
      [ "build_command", Conditional_choice.to_dyn Build_command.to_dyn build_command
      ; "install_command", Conditional_choice.to_dyn Action.to_dyn install_command
      ; "depends", Conditional_choice.to_dyn Depends.to_dyn depends
      ; "depexts", Conditional_choice.to_dyn (Dyn.list String.to_dyn) depexts
      ; "info", Pkg_info.to_dyn info
      ; ( "exported_env"
        , Dyn.list (Action.Env_update.to_dyn String_with_vars.to_dyn) exported_env )
      ]
  ;;

  let compute_missing_checksum t ~pinned =
    let open Fiber.O in
    let+ source =
      match t.info.source with
      | None -> Fiber.return None
      | Some source ->
        Source.compute_missing_checksum source t.info.name ~pinned >>| Option.some
    in
    { t with info = { t.info with source } }
  ;;

  module Fields = struct
    let version = "version"
    let build = "build"
    let install = "install"
    let depends = "depends"
    let depexts = "depexts"
    let source = "source"
    let dev = "dev"
    let exported_env = "exported_env"
    let extra_sources = "extra_sources"
  end

  let decode =
    let open Decoder in
    let parse_install_command_backwards_compatible =
      Conditional_choice.decode_backwards_compatible Action.decode_pkg
    in
    let parse_depends_backwards_compatible =
      repeat Depend.decode
      >>| Conditional_choice.singleton_all_platforms
      <|> repeat (Conditional.decode Depends.decode)
    in
    let parse_depexts_backwards_compatible =
      repeat string
      >>| Conditional_choice.singleton_all_platforms
      <|> repeat (Conditional.decode (enter @@ repeat string))
    in
    enter
    @@ fields
    @@ let+ version = field Fields.version Package_version.decode
       and+ install_command =
         field ~default:[] Fields.install parse_install_command_backwards_compatible
       and+ build_command = Build_command.decode_fields_backwards_compatible
       and+ depends =
         field
           ~default:(Conditional_choice.singleton_all_platforms [])
           Fields.depends
           parse_depends_backwards_compatible
       and+ depexts = field ~default:[] Fields.depexts parse_depexts_backwards_compatible
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
         { build_command; depends; depexts; install_command; info; exported_env }
  ;;

  let encode_extra_source (local, source) : Dune_sexp.t =
    List
      [ Dune_sexp.atom_or_quoted_string (Path.Local.to_string local)
      ; Source.encode source
      ]
  ;;

  let encode
        ~portable
        { build_command
        ; install_command
        ; depends
        ; depexts
        ; info = { Pkg_info.name = _; extra_sources; version; dev; source }
        ; exported_env
        }
    =
    let open Encoder in
    let install_command, build_command, depends, depexts =
      if portable
      then
        ( Conditional_choice.encode_field Fields.install Action.encode install_command
        , Conditional_choice.encode_field
            Fields.build
            Build_command.encode_portable
            build_command
        , Conditional_choice.encode_field Fields.depends Depends.encode depends
        , Conditional_choice.encode_field Fields.depexts (list string) depexts )
      else
        ( field_o
            Fields.install
            Action.encode
            (Conditional_choice.get_value_ensuring_at_most_one_choice install_command)
        , Build_command.encode_non_portable
            (Conditional_choice.get_value_ensuring_at_most_one_choice build_command)
        , field_l
            Fields.depends
            Package_name.encode
            (Conditional_choice.get_value_ensuring_at_most_one_choice depends
             |> Option.value ~default:[]
             |> List.map ~f:(fun { Depend.name; _ } -> name))
        , field_l
            Fields.depexts
            string
            (Conditional_choice.get_value_ensuring_at_most_one_choice depexts
             |> Option.value ~default:[]) )
    in
    record_fields
      [ field Fields.version Package_version.encode version
      ; install_command
      ; build_command
      ; depends
      ; depexts
      ; field_o Fields.source Source.encode source
      ; field_b Fields.dev dev
      ; field_l Fields.exported_env Action.Env_update.encode exported_env
      ; field_l Fields.extra_sources encode_extra_source extra_sources
      ]
  ;;

  let files_dir package_name ~lock_dir =
    Path.Source.relative lock_dir (Package_name.to_string package_name ^ ".files")
  ;;

  (* Combine the platform-specific parts of a pair of [t]s, raising a code
     error if the packages differ in any way apart from their platform-specific
     fields. *)
  let merge_conditionals a b =
    let build_command = Conditional_choice.merge a.build_command b.build_command in
    let install_command = Conditional_choice.merge a.install_command b.install_command in
    let depends = Conditional_choice.merge a.depends b.depends in
    let depexts = Conditional_choice.merge a.depexts b.depexts in
    let ret = { a with build_command; install_command; depends; depexts } in
    if not (equal ret { b with build_command; install_command; depends; depexts })
    then
      Code_error.raise
        "Packages differ in a non-platform-specific field"
        [ "package_1", to_dyn a; "package_2", to_dyn b ];
    ret
  ;;

  let is_available_under_condition t condition =
    Conditional_choice.condition_exists t.depends condition
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
      { version
      ; dependency_hash
      ; packages
      ; ocaml
      ; repos
      ; expanded_solver_variable_bindings
      }
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
      { version
      ; dependency_hash
      ; packages
      ; ocaml
      ; repos
      ; expanded_solver_variable_bindings
      }
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
      List.concat_map dependant_package.depends ~f:(fun conditional_depends ->
        List.filter_map conditional_depends.value ~f:(fun depend ->
          (* CR-someday rgrinberg: do we need the dune check? aren't
             we supposed to filter these upfront? *)
          if
            Package_name.Map.mem packages depend.name
            || Package_name.equal depend.name Dune_dep.name
          then None
          else Some { dependant_package; dependency = depend.name; loc = depend.loc })))
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
     List.map missing_dependencies ~f:(fun { dependant_package; dependency; loc = _ } ->
       ( "missing dependency"
       , Dyn.record
           [ "missing package", Package_name.to_dyn dependency
           ; "dependency of", Package_name.to_dyn dependant_package.info.name
           ] ))
     |> Code_error.raise "Invalid package table");
  let version = Syntax.greatest_supported_version_exn Dune_lang.Pkg.syntax in
  let dependency_hash =
    local_packages
    |> Local_package.For_solver.non_local_dependencies
    |> Local_package.Dependency_hash.of_dependency_formula
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
  { version
  ; dependency_hash
  ; packages
  ; ocaml
  ; repos = { complete; used }
  ; expanded_solver_variable_bindings
  }
;;

let dev_tools_path = Path.Source.(relative root "dev-tools.locks")

let dev_tool_lock_dir_path dev_tool =
  Path.Source.relative
    dev_tools_path
    (Package_name.to_string (Dev_tool.package_name dev_tool))
;;

let default_path = Path.Source.(relative root "dune.lock")
let metadata_filename = "lock.dune"

module Metadata = Dune_sexp.Versioned_file.Make (Unit)

let () = Metadata.Lang.register Dune_lang.Pkg.syntax ()

let encode_metadata
      ~portable
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
  if
    portable
    || Solver_stats.Expanded_variable_bindings.is_empty expanded_solver_variable_bindings
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
    then Ok (Filename.remove_extension package_filename |> Package_name.of_string)
    else Error `Bad_extension
  ;;
end

let file_contents_by_path ~portable t =
  (metadata_filename, encode_metadata ~portable t)
  :: (Package_name.Map.to_list t.packages
      |> List.map ~f:(fun (name, pkg) ->
        Package_filename.of_package_name name, Pkg.encode ~portable pkg))
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

  let raise_user_error_on_check_existance path e =
    let error_reason =
      match e with
      | `Unreadable ->
        Pp.textf "Unable to read lock directory (%s)" (Path.to_string_maybe_quoted path)
      | `Not_directory ->
        Pp.textf
          "Specified lock dir path (%s) is not a directory"
          (Path.to_string_maybe_quoted path)
      | `No_metadata_file ->
        Pp.textf "Specified lock dir lacks metadata file (%s)" metadata_filename
      | `Failed_to_parse_metadata (path, exn) ->
        Pp.concat
          ~sep:Pp.cut
          [ Pp.textf
              "Unable to parse lock directory metadata file (%s):"
              (Path.to_string_maybe_quoted path)
            |> Pp.hovbox
          ; Exn.pp exn |> Pp.hovbox
          ]
        |> Pp.vbox
    in
    User_error.raise
      [ Pp.textf
          "Refusing to regenerate lock directory %s"
          (Path.to_string_maybe_quoted path)
      ; error_reason
      ]
  ;;

  (* Removes the existing lock directory at the specified path if it exists and
     is a valid lock directory. Checks the validity of the existing lockdir (if
     any) and raises if it's invalid before constructing the returned thunk, so
     validation can happen separately from executing the side effect that removes
     the directory. *)
  let safely_remove_lock_dir_if_exists_thunk path =
    match check_existing_lock_dir path with
    | Ok `Non_existant -> Fun.const ()
    | Ok `Is_existing_lock_dir -> fun () -> Path.rm_rf path
    | Error e -> raise_user_error_on_check_existance path e
  ;;

  (* Does the same checks as [safely_remove_lock_dir_if_exists_thunk] but it raises an
     error if the lock dir already exists. [dst] is the new file name *)
  let safely_rename_lock_dir_thunk ~dst src =
    match check_existing_lock_dir src, check_existing_lock_dir dst with
    | Ok `Is_existing_lock_dir, Ok `Non_existant -> fun () -> Path.rename src dst
    | Ok `Non_existant, Ok `Non_existant -> Fun.const ()
    | _, Ok `Is_existing_lock_dir ->
      let error_reason_pp =
        Pp.textf
          "Directory %s already exists: can't rename safely"
          (Path.to_string_maybe_quoted src)
      in
      User_error.raise
        [ Pp.textf
            "Refusing to regenerate lock directory %s"
            (Path.to_string_maybe_quoted src)
        ; error_reason_pp
        ]
    | Error e, _ -> raise_user_error_on_check_existance src e
    | _, Error e -> raise_user_error_on_check_existance dst e
  ;;

  type t = unit -> unit

  let prepare
        ~portable
        ~lock_dir_path:lock_dir_path_src
        ~(files : File_entry.t Package_name.Map.Multi.t)
        lock_dir
    =
    let lock_dir_hidden_src =
      (* The original lockdir path with the lockdir renamed to begin with a ".". *)
      let hidden_basename = sprintf ".%s" (Path.Source.basename lock_dir_path_src) in
      Path.Source.relative (Path.Source.parent_exn lock_dir_path_src) hidden_basename
    in
    let lock_dir_hidden_src = Path.source lock_dir_hidden_src in
    let lock_dir_path_external = Path.source lock_dir_path_src in
    let remove_hidden_dir_if_exists () =
      safely_remove_lock_dir_if_exists_thunk lock_dir_hidden_src ()
    in
    let rename_old_lock_dir_to_hidden =
      safely_rename_lock_dir_thunk ~dst:lock_dir_hidden_src lock_dir_path_external
    in
    let build lock_dir_path =
      let lock_dir_path = Result.ok_exn lock_dir_path in
      file_contents_by_path ~portable lock_dir
      |> List.iter ~f:(fun (path_within_lock_dir, contents) ->
        let path = Path.relative lock_dir_path path_within_lock_dir in
        Option.iter (Path.parent path) ~f:Path.mkdir_p;
        let cst =
          List.map contents ~f:(fun sexp ->
            Dune_sexp.Ast.add_loc ~loc:Loc.none sexp |> Dune_sexp.Cst.concrete)
        in
        (* TODO the version should be chosen based on the version of the lock
           directory we're outputting *)
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
            | Content content -> Io.write_file dst content)));
      rename_old_lock_dir_to_hidden ();
      safely_rename_lock_dir_thunk ~dst:lock_dir_path_external lock_dir_path ();
      remove_hidden_dir_if_exists ()
    in
    match Path.(parent (source lock_dir_path_src)) with
    | Some parent_dir ->
      fun () ->
        Path.mkdir_p parent_dir;
        Temp.with_temp_dir ~parent_dir ~prefix:"dune" ~suffix:"lock" ~f:build
    | None ->
      User_error.raise
        [ Pp.textf "Temporary directory can't be created by deriving the lock dir path" ]
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
    | Ok S_DIR -> Ok ()
    | Error (Unix.ENOENT, _, _) ->
      Error
        (User_error.make
           ~hints:
             [ Pp.concat
                 ~sep:Pp.space
                 [ Pp.text "Run"
                 ; User_message.command "dune pkg lock"
                 ; Pp.text "to generate it."
                 ]
               |> Pp.hovbox
             ]
           [ Pp.textf "%s does not exist." (Path.Source.to_string lock_dir_path) ])
    | Error e ->
      Error
        (User_error.make
           [ Pp.textf "%s is not accessible" (Path.Source.to_string lock_dir_path)
           ; Pp.textf "reason: %s" (Unix_error.Detailed.to_string_hum e)
           ])
    | _ ->
      Error
        (User_error.make
           [ Pp.textf "%s is not a directory." (Path.Source.to_string lock_dir_path) ])
  ;;

  let check_packages packages ~lock_dir_path =
    match validate_packages packages with
    | Ok () -> Ok ()
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
      Error
        (User_error.make
           ~hints:
             [ Pp.concat
                 ~sep:Pp.space
                 [ Pp.text
                     "This could indicate that the lockdir is corrupted. Delete it and \
                      then regenerate it by running:"
                 ; User_message.command "dune pkg lock"
                 ]
             ]
           [ Pp.textf
               "At least one package dependency is itself not present as a package in \
                the lockdir %s."
               (Path.Source.to_string_maybe_quoted lock_dir_path)
           ])
  ;;

  let load lock_dir_path =
    let open Io.O in
    let* result = check_path lock_dir_path in
    match result with
    | Error e -> Io.return (Error e)
    | Ok () ->
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
      check_packages packages ~lock_dir_path
      |> Result.map ~f:(fun () ->
        { version
        ; dependency_hash
        ; packages
        ; ocaml
        ; repos
        ; expanded_solver_variable_bindings
        })
  ;;

  let load_exn lock_dir_path =
    let open Io.O in
    load lock_dir_path >>| User_error.ok_exn
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
let read_disk_exn = Load_immediate.load_exn

let transitive_dependency_closure t condition start =
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
              (of_list_map
                 (let pkg = Package_name.Map.find_exn t.packages node in
                  match Conditional_choice.find pkg.depends condition with
                  | Some depends -> depends
                  | None ->
                    User_error.raise
                      [ Pp.textf
                          "Lockfile does not contain dependencies for %s under the \
                           condition"
                          (Package_name.to_string pkg.info.name)
                      ; Solver_env.pp condition
                      ])
                 ~f:(fun depend -> depend.name))
              seen)
        in
        push_set unseen_deps;
        loop (Package_name.Set.union seen unseen_deps)
    in
    Ok (loop start)
;;

let compute_missing_checksums t ~pinned_packages =
  let open Fiber.O in
  let+ packages =
    Package_name.Map.to_list t.packages
    |> Fiber.parallel_map ~f:(fun (name, pkg) ->
      let pinned = Package_name.Set.mem pinned_packages name in
      let+ pkg = Pkg.compute_missing_checksum pkg ~pinned in
      name, pkg)
    >>| Package_name.Map.of_list_exn
  in
  { t with packages }
;;

let merge_conditionals a b =
  let packages =
    Package_name.Map.merge a.packages b.packages ~f:(fun _ a b ->
      match a, b with
      | Some a, Some b ->
        (* The package exists in both lockdirs. *)
        Some (Pkg.merge_conditionals a b)
      | Some x, None | None, Some x ->
        (* The package only exists in one of the lockdirs. *)
        Some x
      | None, None ->
        (* unreachable *)
        None)
  in
  let normalize t =
    { t with
      packages = Package_name.Map.empty
    ; expanded_solver_variable_bindings = Solver_stats.Expanded_variable_bindings.empty
    }
  in
  if not (equal (normalize a) (normalize b))
  then
    Code_error.raise
      "Platform-specific lockdirs differ in a non-platform-specific way"
      [ "lockdir_1", to_dyn a; "lockdir_2", to_dyn b ];
  { a with packages }
;;

let packages_under_condition { packages; _ } condition =
  Package_name.Map.filter packages ~f:(fun package ->
    Pkg.is_available_under_condition package condition)
;;
