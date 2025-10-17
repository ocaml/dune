open Import
module Digest_feed = Dune_digest.Feed

module Solver_env_disjunction = struct
  (* A disjunction of solver envs consisting of only platform-specific solver variables. *)
  type t = Solver_env.t list

  let singleton solver_env : t =
    let solver_env_with_only_platform_specific_vars =
      Solver_env.remove_all_except_platform_specific solver_env
    in
    [ solver_env_with_only_platform_specific_vars ]
  ;;

  let to_dyn = Dyn.list Solver_env.to_dyn

  let equal a b =
    let a = List.sort a ~compare:Solver_env.compare in
    let b = List.sort b ~compare:Solver_env.compare in
    List.equal Solver_env.equal a b
  ;;

  let hash t = List.hash Solver_env.hash t
  let digest_feed = Digest_feed.list Solver_env.digest_feed

  let encode t =
    let open Encoder in
    list sexp (List.map ~f:Solver_env.encode t)
  ;;

  let decode =
    let open Decoder in
    enter @@ repeat (enter Solver_env.decode)
  ;;

  (* [matches_platform t ~platform] is true iff there exists a solver env in
     [t] whose bindings are a subset of those in [platform] *)
  let matches_platform t ~platform = List.exists t ~f:(Solver_env.is_subset ~of_:platform)
end

module Conditional = struct
  type 'a t =
    { condition : Solver_env_disjunction.t
    ; value : 'a
    }

  let make solver_env value =
    let condition = Solver_env_disjunction.singleton solver_env in
    { condition; value }
  ;;

  let equal value_equal { condition; value } t =
    Solver_env_disjunction.equal condition t.condition && value_equal value t.value
  ;;

  let hash { condition; value } ~f =
    Tuple.T2.hash Solver_env_disjunction.hash f (condition, value)
  ;;

  let digest_feed feed_value =
    Digest_feed.tuple2 Solver_env_disjunction.digest_feed feed_value
    |> Digest_feed.contramap ~f:(fun { condition; value } -> condition, value)
  ;;

  let to_dyn value_to_dyn { condition; value } =
    Dyn.record
      [ "condition", Solver_env_disjunction.to_dyn condition
      ; "value", value_to_dyn value
      ]
  ;;

  let decode value_decode =
    let open Decoder in
    enter
      (let+ condition = Solver_env_disjunction.decode
       and+ value = value_decode in
       { condition; value })
  ;;

  let encode encode_value { condition; value } =
    Dune_lang.List [ Solver_env_disjunction.encode condition; encode_value value ]
  ;;

  let map t ~f = { t with value = f t.value }

  let evaluate { condition; value } ~platform =
    if Solver_env_disjunction.matches_platform condition ~platform
    then Some value
    else None
  ;;
end

module Conditional_choice = struct
  type 'a t = 'a Conditional.t list

  let empty = []
  let singleton condition value = [ Conditional.make condition value ]

  (* A choice where a given value will be chosen unconditionally. This is only
     used to help support both portable and non-portable lockdirs with the same
     codebase and can be removed when portable lockdirs is the only option. *)
  let singleton_all_platforms value = singleton Solver_env.empty value
  let equal value_equal = List.equal (Conditional.equal value_equal)
  let hash t ~f = List.hash (Conditional.hash ~f) t
  let digest_feed feed_value = Digest_feed.list (Conditional.digest_feed feed_value)
  let map ~f = List.map ~f:(Conditional.map ~f)
  let to_dyn value_to_dyn = Dyn.list (Conditional.to_dyn value_to_dyn)

  let choose_for_platform t ~platform =
    List.find_map t ~f:(Conditional.evaluate ~platform)
  ;;

  (* [contains_solver_env t solver_env] is true iff [solver_env] is part of the
     disjunction in any conditional in [t]. *)
  let contains_solver_env t solver_env =
    List.exists t ~f:(fun (conditional : _ Conditional.t) ->
      List.mem conditional.condition solver_env ~equal:Solver_env.equal)
  ;;

  (* Append a [_ Conditional.t] to the choice [t]. If the new conditional has a
     value which is already present in one of the possible choices in [t],
     combine the condition of the new conditional with the condition associated
     with the matching value in [t]. This prevents multiple different choices
     with the same value from appearing in lockfiles. *)
  let append_combining_conditions
        ~value_equal
        t
        ({ Conditional.condition; value } as conditional)
    =
    (match List.find condition ~f:(contains_solver_env t) with
     | Some solver_env ->
       Code_error.raise
         "Tried to add duplicate solver env to lockdir conditional choice"
         [ "solver_env", Solver_env.to_dyn solver_env ]
     | None -> ());
    let rec loop t =
      match t with
      | [] -> [ conditional ]
      | (x : _ Conditional.t) :: xs ->
        if value_equal x.value value
        then { Conditional.condition = x.condition @ condition; value } :: xs
        else x :: loop xs
    in
    loop t
  ;;

  (* Combines two sets of conditionals. If there are values in common between
     the two sets then the conditions corresponding to those values are
     combined to avoid duplication. *)
  let merge_combining_conditions ~value_equal a b =
    List.fold_left b ~init:a ~f:(append_combining_conditions ~value_equal)
  ;;

  (* To support encoding in the non-portable format, this function extracts the
     sole value from a conditional choice, raising a code error if there are
     multiple choices. *)
  let get_value_ensuring_at_most_one_choice t =
    if List.length t > 1
    then
      Code_error.raise
        "Expected at most one conditional choice"
        [ ( "conditions"
          , List.map t ~f:(fun { Conditional.condition; _ } -> condition)
            |> Dyn.list Solver_env_disjunction.to_dyn )
        ];
    List.hd_opt t |> Option.map ~f:(fun { Conditional.value; _ } -> value)
  ;;
end

module Pkg_info = struct
  type t =
    { name : Package_name.t
    ; version : Package_version.t
    ; dev : bool
    ; avoid : bool
    ; source : Source.t option
    ; extra_sources : (Path.Local.t * Source.t) list
    }

  let equal { name; version; dev; avoid; source; extra_sources } t =
    Package_name.equal name t.name
    && Package_version.equal version t.version
    && Bool.equal dev t.dev
    && Bool.equal avoid t.avoid
    && Option.equal Source.equal source t.source
    && List.equal
         (Tuple.T2.equal Path.Local.equal Source.equal)
         extra_sources
         t.extra_sources
  ;;

  let hash { name; version; dev; avoid; source; extra_sources } =
    Poly.hash
      ( Package_name.hash name
      , Package_version.hash version
      , Bool.hash dev
      , Bool.hash avoid
      , Option.hash Source.hash source
      , List.hash (Tuple.T2.hash Path.Local.hash Source.hash) extra_sources )
  ;;

  let digest_feed hasher { name; version; dev; avoid; source; extra_sources } =
    Package_name.digest_feed hasher name;
    Package_version.digest_feed hasher version;
    Digest_feed.bool hasher dev;
    Digest_feed.bool hasher avoid;
    Digest_feed.option Source.digest_feed hasher source;
    Digest_feed.list
      (Digest_feed.tuple2 Digest_feed.generic Source.digest_feed)
      hasher
      extra_sources
  ;;

  let remove_locs t =
    { t with
      source = Option.map ~f:Source.remove_locs t.source
    ; extra_sources =
        List.map t.extra_sources ~f:(fun (local, source) ->
          local, Source.remove_locs source)
    }
  ;;

  let to_dyn { name; version; dev; avoid; source; extra_sources } =
    Dyn.record
      [ "name", Package_name.to_dyn name
      ; "version", Package_version.to_dyn version
      ; "dev", Dyn.bool dev
      ; "avoid", Dyn.bool avoid
      ; "source", Dyn.option Source.to_dyn source
      ; "extra_sources", Dyn.list (Dyn.pair Path.Local.to_dyn Source.to_dyn) extra_sources
      ]
  ;;

  let default_version = Package_version.dev

  let variables t =
    let module Variable = OpamVariable in
    Package_variable_name.Map.of_list_exn
      [ Package_variable_name.name, Variable.S (Package_name.to_string t.name)
      ; Package_variable_name.version, S (Package_version.to_string t.version)
      ; Package_variable_name.dev, B t.dev
      ]
  ;;
end

module Conditional_choice_or_all_platforms = struct
  (* Either a choice of value or a single value to use in all cases. The
     [All_platforms _] case will be used to reduce the verbosity of lockfiles
     where a value is the same for all solver environments under which the
     lockdir is valid. This type is a convenience for encoding and decoding
     lockfiles but doesn't appear in the representation of a package. *)
  type 'a t =
    | Choice of 'a Conditional_choice.t
    | All_platforms of 'a

  let of_conditional_choice ~solved_for_platforms = function
    | [] -> None
    | [ { Conditional.condition; value } ] as choice ->
      if Solver_env_disjunction.equal condition solved_for_platforms
      then Some (All_platforms value)
      else Some (Choice choice)
    | choice -> Some (Choice choice)
  ;;

  let to_conditional_choice ~solved_for_platforms = function
    | Choice choice -> choice
    | All_platforms value -> [ { Conditional.value; condition = solved_for_platforms } ]
  ;;

  let decode decode_value =
    let open Decoder in
    sum
      [ ( "choice"
        , let+ choice = repeat (Conditional.decode decode_value) in
          Choice choice )
      ; ( "all_platforms"
        , let+ value = decode_value in
          All_platforms value )
      ]
  ;;

  let encode encode_value t =
    let open Encoder in
    match t with
    | Choice choice ->
      Dune_lang.List
        (string "choice" :: List.map ~f:(Conditional.encode encode_value) choice)
    | All_platforms value -> Dune_lang.List [ string "all_platforms"; encode_value value ]
  ;;

  let encode_field ~solved_for_platforms name encode_value conditional_choice =
    let open Encoder in
    field_o
      name
      (encode encode_value)
      (of_conditional_choice ~solved_for_platforms conditional_choice)
  ;;
end

module Enabled_on_platforms = struct
  (* A package's availability on various platforms. Either it's available on
     all platforms the lockdir wsa solved for or it's only available on a
     subset of these platforms. *)
  type t =
    | All
    | Only of Solver_env_disjunction.t

  let of_solver_env_disjunction ~solved_for_platforms solver_env_disjunction =
    if Solver_env_disjunction.equal solver_env_disjunction solved_for_platforms
    then All
    else Only solver_env_disjunction
  ;;

  let to_solver_env_disjunction ~solved_for_platforms = function
    | All -> solved_for_platforms
    | Only solver_envs -> solver_envs
  ;;

  let encode t =
    let open Encoder in
    match t with
    | All -> string "all"
    | Only solver_envs ->
      Dune_lang.List (string "only" :: List.map ~f:Solver_env.encode solver_envs)
  ;;

  let decode =
    let open Decoder in
    sum
      [ "all", return All
      ; ( "only"
        , let+ solver_envs = repeat (enter Solver_env.decode) in
          Only solver_envs )
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

  let decode_fields ~portable_lock_dir =
    let open Decoder in
    let parse_action =
      if portable_lock_dir
      then Conditional_choice_or_all_platforms.decode decode_portable
      else
        let+ action = Action.decode_pkg in
        Conditional_choice_or_all_platforms.Choice
          (Conditional_choice.singleton_all_platforms (Action action))
    in
    fields_mutually_exclusive
      ~default:None
      [ ( Fields.build
        , let+ action = parse_action in
          Some action )
      ; ( Fields.dune
        , let+ () = return () in
          Some
            (Conditional_choice_or_all_platforms.Choice
               (Conditional_choice.singleton_all_platforms Dune)) )
      ]
  ;;
end

module Dependency = struct
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

module Dependencies = struct
  type t = Dependency.t list

  let equal = List.equal Dependency.equal
  let remove_locs = List.map ~f:Dependency.remove_locs
  let to_dyn = Dyn.list Dependency.to_dyn
  let encode t = Dune_lang.List (List.map t ~f:Dependency.encode)
end

module Depexts = struct
  module Enabled_if = struct
    type t =
      [ `Always
      | `Conditional of Slang.Blang.t
      ]

    let to_dyn = function
      | `Always -> Dyn.variant "Always" []
      | `Conditional condition ->
        Dyn.variant "Conditional" [ Slang.Blang.to_dyn condition ]
    ;;

    let equal a b =
      match a, b with
      | `Always, `Always -> true
      | `Conditional a, `Conditional b -> Slang.Blang.equal a b
      | _, _ -> false
    ;;

    let remove_locs = function
      | `Always -> `Always
      | `Conditional condition -> `Conditional (Slang.Blang.remove_locs condition)
    ;;
  end

  type t =
    { external_package_names : string list
    ; enabled_if : Enabled_if.t
    }

  let to_dyn { external_package_names; enabled_if } =
    Dyn.record
      [ "external_package_names", Dyn.list Dyn.string external_package_names
      ; "enabled_if", Enabled_if.to_dyn enabled_if
      ]
  ;;

  let equal { external_package_names; enabled_if } t =
    List.equal String.equal external_package_names t.external_package_names
    && Enabled_if.equal enabled_if t.enabled_if
  ;;

  let encode { external_package_names; enabled_if } =
    let open Encoder in
    let external_package_names = list string external_package_names in
    match enabled_if with
    | `Always -> external_package_names
    | `Conditional condition ->
      Dune_lang.List [ external_package_names; Slang.Blang.encode condition ]
  ;;

  let decode =
    let open Decoder in
    enter
      ((let+ external_package_names = enter @@ repeat string
        and+ condition = Slang.Blang.decode in
        { external_package_names; enabled_if = `Conditional condition })
       <|>
       let+ external_package_names = repeat string in
       { external_package_names; enabled_if = `Always })
  ;;

  let remove_locs t = { t with enabled_if = Enabled_if.remove_locs t.enabled_if }
end

let in_source_tree path =
  match (path : Path.t) with
  | In_source_tree s -> Some s
  | In_build_dir b ->
    let in_source = Path.drop_build_context_exn path in
    (match Path.Source.explode in_source with
     | "default" :: ".lock" :: components ->
       let candidate = Path.Source.L.relative Path.Source.root components in
       (match Path.exists (Path.source candidate) with
        | true -> Some candidate
        | false -> None)
     | _otherwise ->
       Code_error.raise
         "Unexpected location of lock directory in build directory"
         [ "path", Path.Build.to_dyn b; "in_source", Path.Source.to_dyn in_source ])
  | External e ->
    Code_error.raise
      "External path returned when loading a lock dir"
      [ "path", Path.External.to_dyn e ]
;;

module Pkg = struct
  type t =
    { build_command : Build_command.t Conditional_choice.t
    ; install_command : Action.t Conditional_choice.t
    ; depends : Dependencies.t Conditional_choice.t
    ; depexts : Depexts.t list
    ; info : Pkg_info.t
    ; exported_env : String_with_vars.t Action.Env_update.t list
    ; enabled_on_platforms : Solver_env_disjunction.t
    }

  let equal
        { build_command
        ; install_command
        ; depends
        ; depexts
        ; info
        ; exported_env
        ; enabled_on_platforms
        }
        t
    =
    Conditional_choice.equal Build_command.equal build_command t.build_command
    (* CR-rgrinberg: why do we ignore locations? *)
    && Conditional_choice.equal Action.equal_no_locs install_command t.install_command
    && Conditional_choice.equal Dependencies.equal depends t.depends
    && List.equal Depexts.equal depexts t.depexts
    && Pkg_info.equal info t.info
    && List.equal
         (Action.Env_update.equal String_with_vars.equal)
         exported_env
         t.exported_env
    && Solver_env_disjunction.equal enabled_on_platforms t.enabled_on_platforms
  ;;

  let hash
        { build_command
        ; install_command
        ; depends
        ; depexts
        ; info
        ; exported_env
        ; enabled_on_platforms
        }
    =
    Poly.hash
      ( Conditional_choice.hash ~f:Poly.hash build_command
      , Conditional_choice.hash ~f:Poly.hash install_command
      , Conditional_choice.hash ~f:Poly.hash depends
      , depexts
      , Pkg_info.hash info
      , exported_env
      , Solver_env_disjunction.hash enabled_on_platforms )
  ;;

  let digest_feed
        hasher
        { build_command
        ; install_command
        ; depends
        ; depexts
        ; info
        ; exported_env
        ; enabled_on_platforms
        }
    =
    Conditional_choice.digest_feed Digest_feed.generic hasher build_command;
    Conditional_choice.digest_feed Digest_feed.generic hasher install_command;
    Conditional_choice.digest_feed Digest_feed.generic hasher depends;
    Digest_feed.generic hasher depexts;
    Pkg_info.digest_feed hasher info;
    Digest_feed.generic hasher exported_env;
    Solver_env_disjunction.digest_feed hasher enabled_on_platforms
  ;;

  let remove_locs
        { build_command
        ; install_command
        ; depends
        ; depexts
        ; info
        ; exported_env
        ; enabled_on_platforms
        }
    =
    { info = Pkg_info.remove_locs info
    ; exported_env =
        List.map exported_env ~f:(Action.Env_update.map ~f:String_with_vars.remove_locs)
    ; depends = Conditional_choice.map depends ~f:Dependencies.remove_locs
    ; depexts = List.map depexts ~f:Depexts.remove_locs
    ; build_command = Conditional_choice.map build_command ~f:Build_command.remove_locs
    ; install_command = Conditional_choice.map install_command ~f:Action.remove_locs
    ; enabled_on_platforms
    }
  ;;

  let to_dyn
        { build_command
        ; install_command
        ; depends
        ; depexts
        ; info
        ; exported_env
        ; enabled_on_platforms
        }
    =
    Dyn.record
      [ "build_command", Conditional_choice.to_dyn Build_command.to_dyn build_command
      ; "install_command", Conditional_choice.to_dyn Action.to_dyn install_command
      ; "depends", Conditional_choice.to_dyn Dependencies.to_dyn depends
      ; "depexts", Dyn.list Depexts.to_dyn depexts
      ; "info", Pkg_info.to_dyn info
      ; ( "exported_env"
        , Dyn.list (Action.Env_update.to_dyn String_with_vars.to_dyn) exported_env )
      ; "enabled_on_platforms", Solver_env_disjunction.to_dyn enabled_on_platforms
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
    let avoid = "avoid"
    let exported_env = "exported_env"
    let extra_sources = "extra_sources"
    let enabled_on_platforms = "enabled_on_platforms"
  end

  let decode ~portable_lock_dir =
    let open Decoder in
    let parse_install_command =
      if portable_lock_dir
      then Conditional_choice_or_all_platforms.decode Action.decode_pkg
      else
        let+ action = Action.decode_pkg in
        Conditional_choice_or_all_platforms.Choice
          (Conditional_choice.singleton_all_platforms action)
    in
    let parse_depends =
      if portable_lock_dir
      then Conditional_choice_or_all_platforms.decode (enter @@ repeat Dependency.decode)
      else
        let+ depends = repeat Dependency.decode in
        Conditional_choice_or_all_platforms.Choice
          (Conditional_choice.singleton_all_platforms depends)
    in
    let parse_depexts =
      if portable_lock_dir
      then repeat Depexts.decode
      else
        let+ external_package_names = repeat string in
        [ { Depexts.external_package_names; enabled_if = `Always } ]
    in
    let empty_choice = Conditional_choice_or_all_platforms.Choice [] in
    enter
    @@ fields
    @@ let+ version = field Fields.version Package_version.decode
       and+ install_command =
         field ~default:empty_choice Fields.install parse_install_command
       and+ build_command = Build_command.decode_fields ~portable_lock_dir
       and+ depends = field ~default:empty_choice Fields.depends parse_depends
       and+ depexts = field ~default:[] Fields.depexts parse_depexts
       and+ source = field_o Fields.source Source.decode
       and+ dev = field_b Fields.dev
       and+ avoid = field_b Fields.avoid
       and+ exported_env =
         field Fields.exported_env ~default:[] (repeat Action.Env_update.decode)
       and+ extra_sources =
         field
           Fields.extra_sources
           ~default:[]
           (repeat (pair (plain_string Path.Local.parse_string_exn) Source.decode))
       and+ enabled_on_platforms =
         field
           Fields.enabled_on_platforms
           ~default:Enabled_on_platforms.All
           Enabled_on_platforms.decode
       in
       fun ~lock_dir ~solved_for_platforms name ->
         let install_command =
           Conditional_choice_or_all_platforms.to_conditional_choice
             ~solved_for_platforms
             install_command
         in
         let build_command =
           match build_command with
           | None -> []
           | Some build_command ->
             Conditional_choice_or_all_platforms.to_conditional_choice
               ~solved_for_platforms
               build_command
         in
         let depends =
           Conditional_choice_or_all_platforms.to_conditional_choice
             ~solved_for_platforms
             depends
         in
         let info =
           let make_source f =
             lock_dir |> Path.to_absolute_filename |> Path.External.of_string |> f
           in
           let source = Option.map source ~f:make_source in
           let extra_sources =
             List.map extra_sources ~f:(fun (path, source) -> path, make_source source)
           in
           { Pkg_info.name; version; dev; avoid; source; extra_sources }
         in
         let enabled_on_platforms =
           Enabled_on_platforms.to_solver_env_disjunction
             ~solved_for_platforms
             enabled_on_platforms
         in
         { build_command
         ; depends
         ; depexts
         ; install_command
         ; info
         ; exported_env
         ; enabled_on_platforms
         }
  ;;

  let encode_extra_source (local, source) : Dune_sexp.t =
    List
      [ Dune_sexp.atom_or_quoted_string (Path.Local.to_string local)
      ; Source.encode source
      ]
  ;;

  let encode
        ~portable_lock_dir
        ~solved_for_platforms
        { build_command
        ; install_command
        ; depends
        ; depexts
        ; info = { Pkg_info.name = _; extra_sources; version; dev; avoid; source }
        ; exported_env
        ; enabled_on_platforms
        }
    =
    let open Encoder in
    let install_command, build_command, depends, depexts, enabled_on_platforms =
      if portable_lock_dir
      then (
        let encode_field n v c =
          Conditional_choice_or_all_platforms.encode_field ~solved_for_platforms n v c
        in
        ( encode_field Fields.install Action.encode install_command
        , encode_field Fields.build Build_command.encode_portable build_command
        , (let depends =
             match depends with
             | [ { Conditional.value = []; _ } ] ->
               (* Omit the dependencies field to reduce noise in the case
                  where there is explictly an empty list of dependencies. *)
               []
             | other -> other
           in
           encode_field Fields.depends Dependencies.encode depends)
        , field_l Fields.depexts Depexts.encode depexts
        , match
            Enabled_on_platforms.of_solver_env_disjunction
              ~solved_for_platforms
              enabled_on_platforms
          with
          | All ->
            (* Omit the field if it's enabled everywhere to reduce noise. The
               parser will assume [All] by default. *)
            []
          | other ->
            [ field Fields.enabled_on_platforms Enabled_on_platforms.encode other ] ))
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
             |> List.map ~f:(fun { Dependency.name; _ } -> name))
        , field_l
            Fields.depexts
            string
            (match depexts with
             | [] -> []
             | [ { Depexts.external_package_names; _ } ] -> external_package_names
             | _ ->
               Code_error.raise
                 "When using non-portable lockdirs it's expected that at most a single \
                  set of depexts will be stored in each lockfile."
                 [ "depexts", Dyn.list Depexts.to_dyn depexts ])
        , [] )
    in
    record_fields
      ([ field Fields.version Package_version.encode version
       ; install_command
       ; build_command
       ; depends
       ; depexts
       ; field_o Fields.source Source.encode source
       ; field_b Fields.dev dev
       ; field_b Fields.avoid avoid
       ; field_l Fields.exported_env Action.Env_update.encode exported_env
       ; field_l Fields.extra_sources encode_extra_source extra_sources
       ]
       @ enabled_on_platforms)
  ;;

  (* More general version of [files_dir] which works on generic paths *)
  let files_dir_generic package_name maybe_package_version ~lock_dir =
    (* TODO(steve): Once portable lockdirs are enabled by default, make the
       package version non-optional *)
    let extension = ".files" in
    match maybe_package_version with
    | None -> Path.relative lock_dir (Package_name.to_string package_name ^ extension)
    | Some package_version ->
      Path.relative
        lock_dir
        (Package_name.to_string package_name
         ^ "."
         ^ Package_version.to_string package_version
         ^ extension)
  ;;

  let files_dir package_name maybe_package_version ~lock_dir =
    match files_dir_generic package_name maybe_package_version ~lock_dir with
    | In_source_tree _ as path -> path
    | In_build_dir _ as path -> path
    | External e ->
      Code_error.raise
        "file_dir is an external path, this is unsupported"
        [ "path", Path.External.to_dyn e ]
  ;;

  let source_files_dir package_name maybe_package_version ~lock_dir =
    let open Option.O in
    let* source = in_source_tree lock_dir in
    let package_name = Package_name.to_string package_name in
    let candidate =
      match maybe_package_version with
      | Some package_version ->
        Path.Source.relative
          source
          (sprintf "%s.%s.files" package_name (Package_version.to_string package_version))
      | None -> Path.Source.relative source (sprintf "%s.files" package_name)
    in
    match Path.exists (Path.source candidate) with
    | true -> Some candidate
    | false -> None
  ;;

  (* Combine the platform-specific parts of a pair of [t]s, raising a code
     error if the packages differ in any way apart from their platform-specific
     fields. *)
  let merge_conditionals a b =
    let build_command =
      Conditional_choice.merge_combining_conditions
        ~value_equal:Build_command.equal
        a.build_command
        b.build_command
    in
    let install_command =
      Conditional_choice.merge_combining_conditions
        ~value_equal:Action.equal
        a.install_command
        b.install_command
    in
    let depends =
      Conditional_choice.merge_combining_conditions
        ~value_equal:Dependencies.equal
        a.depends
        b.depends
    in
    let enabled_on_platforms = a.enabled_on_platforms @ b.enabled_on_platforms in
    let ret = { a with build_command; install_command; depends; enabled_on_platforms } in
    if
      not
        (equal
           ret
           { b with build_command; install_command; depends; enabled_on_platforms })
    then
      Code_error.raise
        "Packages differ in a non-platform-specific field"
        [ "package_1", to_dyn a; "package_2", to_dyn b ];
    ret
  ;;

  let is_enabled_on_platform t ~platform =
    (* XXX: currently treat empty lists of platforms as if the platform is
       enabled on all platforms to simplify supporting both portable and
       non-portable lockdirs with the same code. *)
    List.is_empty t.enabled_on_platforms
    || Solver_env_disjunction.matches_platform t.enabled_on_platforms ~platform
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

module Packages = struct
  type t = Pkg.t Package_version.Map.t Package_name.Map.t

  let remove_locs = Package_name.Map.map ~f:(Package_version.Map.map ~f:Pkg.remove_locs)
  let equal = Package_name.Map.equal ~equal:(Package_version.Map.equal ~equal:Pkg.equal)
  let to_dyn = Package_name.Map.to_dyn (Package_version.Map.to_dyn Pkg.to_dyn)

  let to_pkg_list t =
    Package_name.Map.values t |> List.concat_map ~f:Package_version.Map.values
  ;;

  let of_pkg_list pkgs =
    List.map pkgs ~f:(fun (pkg : Pkg.t) -> pkg.info.name, (pkg.info.version, pkg))
    |> Package_name.Map.of_list_multi
    |> Package_name.Map.map ~f:Package_version.Map.of_list_exn
  ;;

  (* [choose_pkg_for_platform t name ~platform] returns the package whose name
     is [name] which is enabled on [platform], and returns [None] if either no
     package exists whose name is [name] or if there is no version of the
     package that is enabled on [platform]. *)
  let choose_pkg_for_platform t name ~platform =
    let open Option.O in
    Package_name.Map.find t name
    >>| Package_version.Map.values
    >>= List.find ~f:(Pkg.is_enabled_on_platform ~platform)
  ;;

  let pkgs_on_platform_by_name t ~platform =
    Package_name.Map.filter_map t ~f:(fun version_map ->
      Package_version.Map.values version_map
      |> List.find ~f:(Pkg.is_enabled_on_platform ~platform))
  ;;

  let merge a b =
    Package_name.Map.merge a b ~f:(fun _ a b ->
      match a, b with
      | None, None ->
        (* unreachable *)
        None
      | Some x, None | None, Some x -> Some x
      | Some a, Some b ->
        Some
          (Package_version.Map.merge a b ~f:(fun _ a b ->
             match a, b with
             | None, None ->
               (* unreachable *)
               None
             | Some x, None | None, Some x -> Some x
             | Some a, Some b -> Some (Pkg.merge_conditionals a b))))
  ;;
end

type t =
  { version : Syntax.Version.t
  ; dependency_hash : (Loc.t * Local_package.Dependency_hash.t) option
  ; packages : Packages.t
  ; ocaml : (Loc.t * Package_name.t) option
  ; repos : Repositories.t
  ; expanded_solver_variable_bindings : Solver_stats.Expanded_variable_bindings.t
  ; solved_for_platforms : Loc.t * Solver_env.t list
  }

let remove_locs t =
  { t with
    packages = Packages.remove_locs t.packages
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
      ; solved_for_platforms
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
  && Packages.equal packages t.packages
  && Solver_stats.Expanded_variable_bindings.equal
       expanded_solver_variable_bindings
       t.expanded_solver_variable_bindings
  && (Tuple.T2.equal Loc.equal (List.equal Solver_env.equal))
       solved_for_platforms
       t.solved_for_platforms
;;

let to_dyn
      { version
      ; dependency_hash
      ; packages
      ; ocaml
      ; repos
      ; expanded_solver_variable_bindings
      ; solved_for_platforms
      }
  =
  Dyn.record
    [ "version", Syntax.Version.to_dyn version
    ; ( "dependency_hash"
      , Dyn.option
          (Tuple.T2.to_dyn Loc.to_dyn_hum Local_package.Dependency_hash.to_dyn)
          dependency_hash )
    ; "packages", Packages.to_dyn packages
    ; "ocaml", Dyn.option (Tuple.T2.to_dyn Loc.to_dyn_hum Package_name.to_dyn) ocaml
    ; "repos", Repositories.to_dyn repos
    ; ( "expanded_solver_variable_bindings"
      , Solver_stats.Expanded_variable_bindings.to_dyn expanded_solver_variable_bindings )
    ; ( "solved_for_platforms"
      , Tuple.T2.to_dyn Loc.to_dyn_hum (Dyn.list Solver_env.to_dyn) solved_for_platforms )
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
    Packages.to_pkg_list packages
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
      ~solved_for_platform
  =
  let packages =
    Package_name.Map.map packages ~f:(fun (pkg : Pkg.t) ->
      Package_version.Map.singleton pkg.info.version pkg)
  in
  (match validate_packages packages with
   | Ok () -> ()
   | Error (`Missing_dependencies missing_dependencies) ->
     List.map missing_dependencies ~f:(fun { dependant_package; dependency; loc = _ } ->
       ( "missing dependency"
       , Dyn.record
           [ "missing package", Package_name.to_dyn dependency
           ; "dependency of", Package_name.to_dyn dependant_package.info.name
           ] ))
     @ [ "packages", Packages.to_dyn packages ]
     |> Code_error.raise "Invalid package table");
  let version = Syntax.greatest_supported_version_exn Dune_lang.Pkg.syntax in
  let dependency_hash =
    Local_package.For_solver.non_local_dependencies local_packages
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
  let solved_for_platform_platform_specific_only =
    Option.map solved_for_platform ~f:Solver_env.remove_all_except_platform_specific
  in
  { version
  ; dependency_hash
  ; packages
  ; ocaml
  ; repos = { complete; used }
  ; expanded_solver_variable_bindings
  ; solved_for_platforms =
      Loc.none, Option.to_list solved_for_platform_platform_specific_only
  }
;;

let metadata_filename = "lock.dune"

module Metadata = Dune_sexp.Versioned_file.Make (Unit)

let () = Metadata.Lang.register Dune_lang.Pkg.syntax ()

let encode_metadata
      ~portable_lock_dir
      { version
      ; dependency_hash
      ; ocaml
      ; repos
      ; packages = _
      ; expanded_solver_variable_bindings
      ; solved_for_platforms
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
  @ (if
       portable_lock_dir
       || Solver_stats.Expanded_variable_bindings.is_empty
            expanded_solver_variable_bindings
     then []
     else
       [ list
           sexp
           (string "expanded_solver_variable_bindings"
            :: Solver_stats.Expanded_variable_bindings.encode
                 expanded_solver_variable_bindings)
       ])
  @
  if portable_lock_dir
  then (
    let _loc, solved_for_platforms = solved_for_platforms in
    [ list
        sexp
        (string "solved_for_platforms"
         :: List.map ~f:Solver_env.encode solved_for_platforms)
    ])
  else []
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
     and+ solved_for_platforms =
       field_o "solved_for_platforms" (located (repeat (enter Solver_env.decode)))
     in
     ( ocaml
     , dependency_hash
     , repos
     , expanded_solver_variable_bindings
     , solved_for_platforms ))
;;

module Package_filename = struct
  let file_extension = ".pkg"

  let make package_name maybe_package_version =
    (* TODO(steve): the [maybe_package_version] argument is an [_ option]
       because if portable lockdirs is not enabled then we want to fall back to
       the behaviour where version numbers are not included in lockfile names.
       Make it non-optional when lockdirs become portable by default. *)
    match maybe_package_version with
    | None -> Package_name.to_string package_name ^ file_extension
    | Some package_version ->
      Package_name.to_string package_name
      ^ "."
      ^ Package_version.to_string package_version
      ^ file_extension
  ;;

  let to_package_name_and_version package_filename =
    if String.equal (Filename.extension package_filename) file_extension
    then (
      let without_extension = Filename.remove_extension package_filename in
      match String.lsplit2 without_extension ~on:'.' with
      | Some (left, right) ->
        Ok (Package_name.of_string left, Some (Package_version.of_string right))
      | None -> Ok (Package_name.of_string without_extension, None))
    else Error `Bad_extension
  ;;
end

let file_contents_by_path ~portable_lock_dir t =
  (metadata_filename, encode_metadata ~portable_lock_dir t)
  :: (Packages.to_pkg_list t.packages
      |> List.map ~f:(fun (pkg : Pkg.t) ->
        let _loc, solved_for_platforms = t.solved_for_platforms in
        let package_filename =
          if portable_lock_dir
          then Package_filename.make pkg.info.name (Some pkg.info.version)
          else Package_filename.make pkg.info.name None
        in
        package_filename, Pkg.encode ~portable_lock_dir ~solved_for_platforms pkg))
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
        ~portable_lock_dir
        ~lock_dir_path:lock_dir_path_external
        ~(files : File_entry.t Package_version.Map.Multi.t Package_name.Map.t)
        lock_dir
    =
    let lock_dir_hidden =
      (* The original lockdir path with the lockdir renamed to begin with a ".". *)
      let hidden_basename = sprintf ".%s" (Path.basename lock_dir_path_external) in
      Path.relative (Path.parent_exn lock_dir_path_external) hidden_basename
    in
    let remove_hidden_dir_if_exists () =
      safely_remove_lock_dir_if_exists_thunk lock_dir_hidden ()
    in
    let rename_old_lock_dir_to_hidden =
      safely_rename_lock_dir_thunk ~dst:lock_dir_hidden lock_dir_path_external
    in
    let build lock_dir_path =
      let lock_dir_path = Result.ok_exn lock_dir_path in
      file_contents_by_path ~portable_lock_dir lock_dir
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
        Package_name.Map.iteri files ~f:(fun package_name files_by_version ->
          Package_version.Map.iteri files_by_version ~f:(fun package_version files ->
            let files_dir =
              let maybe_package_version =
                if portable_lock_dir then Some package_version else None
              in
              Pkg.files_dir_generic
                package_name
                maybe_package_version
                ~lock_dir:lock_dir_path
            in
            Path.mkdir_p files_dir;
            List.iter files ~f:(fun { File_entry.original; local_file } ->
              let dst = Path.append_local files_dir local_file in
              Path.mkdir_p (Path.parent_exn dst);
              match original with
              | Path src -> Io.copy_file ~src ~dst ()
              | Content content -> Io.write_file dst content))));
      rename_old_lock_dir_to_hidden ();
      safely_rename_lock_dir_thunk ~dst:lock_dir_path_external lock_dir_path ();
      remove_hidden_dir_if_exists ()
    in
    match Path.parent lock_dir_path_external with
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
    val readdir_with_kinds : Path.t -> (Filename.t * Unix.file_kind) list t
    val with_lexbuf_from_file : Path.t -> f:(Lexing.lexbuf -> 'a) -> 'a t
  end) =
struct
  let load_metadata metadata_file_path =
    let open Io.O in
    let+ ( syntax
         , version
         , dependency_hash
         , ocaml
         , repos
         , expanded_solver_variable_bindings
         , solved_for_platforms )
      =
      Io.with_lexbuf_from_file metadata_file_path ~f:(fun lexbuf ->
        Metadata.parse_contents
          lexbuf
          ~f:(fun { Metadata.Lang.Instance.syntax; data = (); version } ->
            let open Decoder in
            let+ ( ocaml
                 , dependency_hash
                 , repos
                 , expanded_solver_variable_bindings
                 , solved_for_platforms )
              =
              decode_metadata
            in
            ( syntax
            , version
            , dependency_hash
            , ocaml
            , repos
            , expanded_solver_variable_bindings
            , solved_for_platforms )))
    in
    if String.equal (Syntax.name syntax) (Syntax.name Dune_lang.Pkg.syntax)
    then
      ( version
      , dependency_hash
      , ocaml
      , repos
      , expanded_solver_variable_bindings
      , solved_for_platforms )
    else
      User_error.raise
        [ Pp.textf
            "In %s, expected language to be %s, but found %s"
            (Path.to_string metadata_file_path)
            (Syntax.name Dune_lang.Pkg.syntax)
            (Syntax.name syntax)
        ]
  ;;

  let load_pkg
        ~portable_lock_dir
        ~version
        ~lock_dir_path
        ~solved_for_platforms
        package_name
        maybe_package_version
    =
    let open Io.O in
    let pkg_file_path =
      Path.relative
        lock_dir_path
        (Package_filename.make package_name maybe_package_version)
    in
    let+ sexp =
      Io.with_lexbuf_from_file pkg_file_path ~f:(Dune_sexp.Parser.parse ~mode:Many)
    in
    let parser =
      let env = Pform.Env.pkg Dune_lang.Pkg.syntax version in
      let decode =
        Syntax.set Dune_lang.Pkg.syntax (Active version) (Pkg.decode ~portable_lock_dir)
        |> Syntax.set Dune_lang.Stanza.syntax (Active Dune_lang.Stanza.latest_version)
      in
      String_with_vars.set_decoding_env env decode
    in
    (Decoder.parse parser Univ_map.empty (List (Loc.none, sexp)))
      ~lock_dir:lock_dir_path
      ~solved_for_platforms
      package_name
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
                 (Path.to_string_maybe_quoted lock_dir_path)
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
               (Path.to_string_maybe_quoted lock_dir_path)
           ])
  ;;

  let load lock_dir_path =
    let open Io.O in
    let* ( version
         , dependency_hash
         , ocaml
         , repos
         , expanded_solver_variable_bindings
         , solved_for_platforms )
      =
      load_metadata (Path.relative lock_dir_path metadata_filename)
    in
    let portable_lock_dir, solved_for_platforms =
      match solved_for_platforms with
      | Some x -> true, x
      | None -> false, (Loc.none, [])
    in
    let+ packages =
      Io.readdir_with_kinds lock_dir_path
      >>| List.filter_map ~f:(fun (name, (kind : Unix.file_kind)) ->
        match kind with
        | S_REG -> Package_filename.to_package_name_and_version name |> Result.to_option
        | _ ->
          (* TODO *)
          None)
      >>= Io.parallel_map ~f:(fun (package_name, maybe_package_version) ->
        let _loc, solved_for_platforms = solved_for_platforms in
        let+ pkg =
          load_pkg
            ~portable_lock_dir
            ~version
            ~lock_dir_path
            ~solved_for_platforms
            package_name
            maybe_package_version
        in
        pkg)
      >>| Packages.of_pkg_list
    in
    check_packages packages ~lock_dir_path
    |> Result.map ~f:(fun () ->
      { version
      ; dependency_hash
      ; packages
      ; ocaml
      ; repos
      ; expanded_solver_variable_bindings
      ; solved_for_platforms
      })
  ;;

  let load_exn lock_dir_path =
    let open Io.O in
    load lock_dir_path >>| User_error.ok_exn
  ;;
end

module Load_immediate = Make_load (struct
    include Monad.Id

    let parallel_map xs ~f = List.map xs ~f

    let readdir_with_kinds path =
      match Path.readdir_unsorted_with_kinds path with
      | Ok entries -> entries
      | Error e -> User_error.raise [ Pp.text (Unix_error.Detailed.to_string_hum e) ]
    ;;

    let with_lexbuf_from_file = Io.with_lexbuf_from_file
  end)

let read_disk = Load_immediate.load
let read_disk_exn = Load_immediate.load_exn

let transitive_dependency_closure t ~platform start =
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
          (* Note that the call to [Packages.choose_pkg_for_platform] won't
             in general return [None] because [t] guarantees that its map of
             dependencies is closed under "depends on". Sometimes a package
             (such as dune) is explicitly removed from the closure in which
             case we will have a [None] and ignore it. *)
          Package_name.Set.(
            diff
              (of_list_map
                 ~f:(fun depend -> depend.name)
                 ((let open Option.O in
                   let* pkg =
                     Packages.choose_pkg_for_platform t.packages node ~platform
                   in
                   Conditional_choice.choose_for_platform pkg.depends ~platform)
                  |> Option.value ~default:[]))
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
    Packages.to_pkg_list t.packages
    |> Fiber.parallel_map ~f:(fun (pkg : Pkg.t) ->
      let pinned = Package_name.Set.mem pinned_packages pkg.info.name in
      Pkg.compute_missing_checksum pkg ~pinned)
    >>| Packages.of_pkg_list
  in
  { t with packages }
;;

let merge_conditionals a b =
  let packages = Packages.merge a.packages b.packages in
  let solved_for_platforms =
    let a_loc, a_solved_for_platforms = a.solved_for_platforms in
    let b_loc, b_solved_for_platforms = b.solved_for_platforms in
    Loc.span a_loc b_loc, a_solved_for_platforms @ b_solved_for_platforms
  in
  let normalize t =
    { t with
      packages = Package_name.Map.empty
    ; expanded_solver_variable_bindings = Solver_stats.Expanded_variable_bindings.empty
    ; solved_for_platforms = Loc.none, []
    }
  in
  if not (equal (normalize a) (normalize b))
  then
    Code_error.raise
      "Platform-specific lockdirs differ in a non-platform-specific way"
      [ "lockdir_1", to_dyn a; "lockdir_2", to_dyn b ];
  { a with packages; solved_for_platforms }
;;

let loc_in_source_tree loc =
  loc
  |> Loc.map_pos ~f:(fun ({ pos_fname; _ } as pos) ->
    let path = Path.of_string pos_fname in
    match in_source_tree path with
    | Some new_path ->
      let pos_fname = Path.Source.to_string new_path in
      { pos with pos_fname }
    | None -> pos)
;;

let check_if_solved_for_platform { solved_for_platforms; _ } ~platform =
  let loc, solved_for_platforms = solved_for_platforms in
  if List.is_empty solved_for_platforms
  then
    (* TODO(steve): this case is only necessary while supporting non-portable
       lockdirs. Once portable lockdirs are always enabled then remove this case. *)
    ()
  else (
    match Solver_env_disjunction.matches_platform solved_for_platforms ~platform with
    | true -> ()
    | false ->
      let loc = loc_in_source_tree loc in
      User_error.raise
        ~loc
        [ Pp.text
            "The lockdir does not contain a solution compatible with the current \
             platform."
        ; Pp.text "The current platform is:"
        ; Solver_env.pp platform
        ]
        ~hints:
          [ Pp.text "Try adding the following to dune-workspace:"
          ; (let open Dune_sexp in
             let suggested_env =
               let variables_to_keep =
                 (* These variables have the most impact on solutions. Leave
                    out other variables like distro and version as they often
                    don't contribute to solutions and just bloat the config and
                    increase solve times for no benefit. *)
                 Package_variable_name.Set.of_list
                   [ Package_variable_name.arch; Package_variable_name.os ]
               in
               Solver_env.remove_all_except platform variables_to_keep
             in
             let sexp_for_dune_workspace =
               List
                 [ Atom (Atom.of_string "lock_dir")
                 ; List
                     [ Atom (Atom.of_string "solve_for_platforms")
                     ; Solver_env.encode suggested_env
                     ]
                 ]
             in
             Dune_sexp.pp sexp_for_dune_workspace)
          ; Pp.concat
              ~sep:Pp.space
              [ Pp.text "...and then rerun"; User_message.command "dune pkg lock" ]
          ])
;;

let packages_on_platform t ~platform =
  check_if_solved_for_platform t ~platform;
  Packages.pkgs_on_platform_by_name t.packages ~platform
;;
