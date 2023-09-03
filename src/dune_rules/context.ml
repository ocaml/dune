open Import
open Memo.O

module Kind = struct
  module Opam = struct
    type t =
      { root : string option
      ; switch : string
      }
  end

  type t =
    | Default
    | Opam of Opam.t

  let to_dyn : t -> Dyn.t = function
    | Default -> Dyn.string "default"
    | Opam o -> Dyn.(record [ "root", option string o.root; "switch", string o.switch ])
  ;;
end

module Env_nodes = struct
  type t =
    { context : Dune_env.Stanza.t option
    ; workspace : Dune_env.Stanza.t option
    }

  let extra_env { context; workspace } profile =
    let make env =
      Option.value
        ~default:Env.empty
        (let open Option.O in
         let+ (env : Dune_env.Stanza.config) =
           env >>= Dune_env.Stanza.find_opt ~profile
         in
         env.env_vars)
    in
    Env.extend_env (make context) (make workspace)
  ;;
end

type t =
  { name : Context_name.t
  ; kind : Kind.t
  ; profile : Profile.t
  ; merlin : bool
  ; fdo_target_exe : Path.t option
  ; dynamically_linked_foreign_archives : bool
  ; for_host : t option
  ; implicit : bool
  ; build_dir : Path.Build.t
  ; env_nodes : Env_nodes.t
  ; path : Path.t list
  ; ocaml : Ocaml_toolchain.t
  ; installed_env : Env.t
  ; findlib_paths : Path.t list
  ; findlib_toolchain : Context_name.t option
  ; default_ocamlpath : Path.t list
  ; supports_shared_libraries : Dynlink_supported.By_the_os.t
  ; build_context : Build_context.t
  ; instrument_with : Lib_name.t list
  }

let equal x y = Context_name.equal x.name y.name
let hash t = Context_name.hash t.name
let build_context t = t.build_context

let to_dyn t : Dyn.t =
  let open Dyn in
  let path = Path.to_dyn in
  record
    [ "name", Context_name.to_dyn t.name
    ; "kind", Kind.to_dyn t.kind
    ; "profile", Profile.to_dyn t.profile
    ; "merlin", Bool t.merlin
    ; "for_host", option Context_name.to_dyn (Option.map t.for_host ~f:(fun t -> t.name))
    ; "fdo_target_exe", option path t.fdo_target_exe
    ; "build_dir", Path.Build.to_dyn t.build_dir
    ; "ocaml_bin", path t.ocaml.bin_dir
    ; "ocaml", Action.Prog.to_dyn t.ocaml.ocaml
    ; "ocamlc", path t.ocaml.ocamlc
    ; "ocamlopt", Action.Prog.to_dyn t.ocaml.ocamlopt
    ; "ocamldep", Action.Prog.to_dyn t.ocaml.ocamldep
    ; "ocamlmklib", Action.Prog.to_dyn t.ocaml.ocamlmklib
    ; "installed_env", Env.to_dyn (Env.diff t.installed_env Env.initial)
    ; "findlib_paths", list path t.findlib_paths
    ; ( "natdynlink_supported"
      , Bool (Dynlink_supported.By_the_os.get t.ocaml.lib_config.natdynlink_supported) )
    ; ( "supports_shared_libraries"
      , Bool (Dynlink_supported.By_the_os.get t.supports_shared_libraries) )
    ; "ocaml_config", Ocaml_config.to_dyn t.ocaml.ocaml_config
    ; "instrument_with", (list Lib_name.to_dyn) t.instrument_with
    ]
;;

let to_dyn_concise t : Dyn.t = Context_name.to_dyn t.name
let compare a b = Context_name.compare a.name b.name

(** Wrap calls to the opam binary *)
module Opam : sig
  (** Environment for this opam switch *)
  val env : env:Env.t -> root:string option -> switch:string -> string Env.Map.t Memo.t
end = struct
  let opam =
    Memo.Lazy.create ~name:"context-opam" (fun () ->
      Which.which ~path:(Env_path.path Env.initial) "opam"
      >>= function
      | None -> Utils.program_not_found "opam" ~loc:None
      | Some opam ->
        let+ version =
          Memo.of_reproducible_fiber
            (Process.run_capture_line
               ~display:Quiet
               Strict
               opam
               [ "--version"; "--color=never" ])
        in
        (match Scanf.sscanf version "%d.%d.%d" (fun a b c -> a, b, c) with
         | Ok ((a, b, c) as v) ->
           if v < (2, 0, 0)
           then
             User_error.raise
               [ Pp.textf
                   "The version of opam installed on your system is too old. Dune \
                    requires at least version 2.0.0, however version %d.%d.%d is \
                    installed."
                   a
                   b
                   c
               ];
           opam
         | Error () ->
           User_error.raise
             [ Pp.textf
                 "`%s config --version' returned invalid output:"
                 (Path.to_string_maybe_quoted opam)
             ; Pp.verbatim version
             ]))
  ;;

  let opam_binary_exn () = Memo.Lazy.force opam

  let env =
    let impl (env, root, switch) =
      let* opam = opam_binary_exn () in
      let args =
        List.concat
          [ [ "config"; "env" ]
          ; (match root with
             | None -> []
             | Some root -> [ "--root"; root ])
          ; [ "--switch"; switch; "--sexp"; "--set-switch" ]
          ]
      in
      let+ s =
        Memo.of_reproducible_fiber
          (Process.run_capture ~display:Quiet ~env Strict opam args)
      in
      Dune_lang.Parser.parse_string ~fname:"<opam output>" ~mode:Single s
      |> Dune_lang.Decoder.(parse (enter (repeat (pair string string))) Univ_map.empty)
      |> Env.Map.of_list_multi
      |> Env.Map.mapi ~f:(fun var values ->
        match List.rev values with
        | [] -> assert false
        | [ x ] -> x
        | x :: _ ->
          User_warning.emit
            [ Pp.textf "variable %S present multiple times in the output of:" var
            ; Pp.tag
                User_message.Style.Details
                (Pp.text (String.quote_list_for_shell (Path.to_string opam :: args)))
            ];
          x)
    in
    let module Input = struct
      type t = Env.t * string option * string

      let equal (env_a, root_a, switch_a) (env_b, root_b, switch_b) =
        Env.equal env_a env_b
        && Option.equal String.equal root_a root_b
        && String.equal switch_a switch_b
      ;;

      let hash = Tuple.T3.hash Env.hash Poly.hash Poly.hash

      let to_dyn (env, root, switch) =
        Dyn.Tuple [ Env.to_dyn env; Dyn.(option string root); String switch ]
      ;;
    end
    in
    let memo =
      Memo.create
        "opam-env"
        impl
        ~cutoff:(Env.Map.equal ~equal:String.equal)
        ~input:(module Input)
    in
    fun ~env ~root ~switch -> Memo.exec memo (env, root, switch)
  ;;
end

module Build_environment_kind = struct
  (* Heuristics to detect the current environment *)

  type t =
    | Cross_compilation_using_findlib_toolchain of Context_name.t
    | Hardcoded_path of string list
    | Opam2_environment of string (* opam switch prefix *)
    | Unknown

  let opam_switch_prefix_var_name = "OPAM_SWITCH_PREFIX"

  let query ~(kind : Kind.t) ~findlib_toolchain ~env =
    match findlib_toolchain with
    | Some s -> Cross_compilation_using_findlib_toolchain s
    | None ->
      let opam_prefix = Env.get env opam_switch_prefix_var_name in
      (match kind with
       | Opam _ ->
         (match opam_prefix with
          | Some s -> Opam2_environment s
          | None ->
            (* This is unreachable because we check in [create_for_opam] that opam
               sets this variable *)
            assert false)
       | Default ->
         (match Setup.library_path with
          | _ :: _ as l -> Hardcoded_path l
          | [] ->
            (match opam_prefix with
             | Some s -> Opam2_environment s
             | None -> Unknown)))
  ;;

  let findlib_paths t ~findlib ~ocaml_bin =
    match findlib, t with
    | ( Some findlib
      , (Cross_compilation_using_findlib_toolchain _ | Opam2_environment _ | Unknown) ) ->
      Findlib_config.ocamlpath findlib
    | None, Cross_compilation_using_findlib_toolchain toolchain ->
      User_error.raise
        [ Pp.textf
            "Could not find `ocamlfind' in PATH or an environment variable \
             `OCAMLFIND_CONF' while cross-compiling with toolchain `%s'"
            (Context_name.to_string toolchain)
        ]
        ~hints:
          [ Pp.enumerate
              [ "`opam install ocamlfind' and/or:"
              ; "Point `OCAMLFIND_CONF' to the findlib configuration that defines this \
                 toolchain"
              ]
              ~f:Pp.text
          ]
    | _, Hardcoded_path l -> List.map l ~f:Path.of_filename_relative_to_initial_cwd
    | None, Opam2_environment opam_prefix ->
      let p = Path.of_filename_relative_to_initial_cwd opam_prefix in
      let p = Path.relative p "lib" in
      [ p ]
    | None, Unknown -> [ Path.relative (Path.parent_exn ocaml_bin) "lib" ]
  ;;
end

type instance =
  { native : t
  ; targets : t list
  }

let ocamlpath (kind : Kind.t) ~env ~findlib_toolchain =
  let env_ocamlpath = Findlib_config.ocamlpath_of_env env in
  match kind, findlib_toolchain with
  | Default, None -> Option.value ~default:[] env_ocamlpath
  | _, _ ->
    let initial_ocamlpath = Findlib_config.ocamlpath_of_env Env.initial in
    (* If we are not in the default context, we can only use the OCAMLPATH
       variable if it is specific to this build context *)
    (* CR-someday diml: maybe we should actually clear OCAMLPATH in other
       build contexts *)
    (match env_ocamlpath, initial_ocamlpath with
     | None, None -> []
     | Some s, None ->
       (* [OCAMLPATH] set for the target context, unset in the
          [initial_env]. This means it's the [OCAMLPATH] specific to this
          build context. *)
       s
     | None, Some _ ->
       (* Clear [OCAMLPATH] for this build context if it's defined
          initially but not for this build context. *)
       []
     | Some env_ocamlpath, Some initial_ocamlpath ->
       (* Clear [OCAMLPATH] for this build context Unless it's different
          from the initial [OCAMLPATH] variable. *)
       (match List.compare ~compare:Path.compare env_ocamlpath initial_ocamlpath with
        | Eq -> []
        | _ -> env_ocamlpath))
;;

let installed_env env name findlib env_nodes version profile =
  let env =
    (* See comment in ansi_color.ml for setup_env_for_colors. For versions
       where OCAML_COLOR is not supported, but 'color' is in OCAMLPARAM, use
       the latter. If 'color' is not supported, we just don't force colors
       with 4.02. *)
    if !Clflags.capture_outputs
       && Lazy.force Ansi_color.stderr_supports_color
       && Ocaml.Version.supports_color_in_ocamlparam version
       && not (Ocaml.Version.supports_ocaml_color version)
    then Ocaml.Env.with_color env
    else env
  in
  let vars =
    Env.Map.singleton
      Execution_env.Inside_dune.var
      (Execution_env.Inside_dune.value (In_context (Context_name.build_dir name)))
  in
  Env.extend env ~vars
  |> Env.extend_env
       (Option.value ~default:Env.empty (Option.map findlib ~f:Findlib_config.env))
  |> Env.extend_env (Env_nodes.extra_env env_nodes profile)
;;

let create
  ~(kind : Kind.t)
  ~path
  ~env
  ~env_nodes
  ~name
  ~merlin
  ~targets
  ~host_context
  ~host_toolchain
  ~profile
  ~fdo_target_exe
  ~dynamically_linked_foreign_archives
  ~instrument_with
  =
  let which = Which.which ~path in
  let create_one ~(name : Context_name.t) ~implicit ~findlib_toolchain ~host ~merlin =
    let ocamlpath = ocamlpath kind ~env ~findlib_toolchain in
    let* findlib =
      let findlib_toolchain = Option.map findlib_toolchain ~f:Context_name.to_string in
      Findlib_config.discover_from_env ~env ~which ~ocamlpath ~findlib_toolchain
    in
    let* ocaml = Ocaml_toolchain.of_env_with_findlib name env findlib ~which in
    let default_ocamlpath =
      let default_ocamlpath =
        let build_env_kind = Build_environment_kind.query ~kind ~findlib_toolchain ~env in
        Build_environment_kind.findlib_paths
          build_env_kind
          ~findlib
          ~ocaml_bin:ocaml.bin_dir
      in
      if Ocaml.Version.has_META_files ocaml.version
      then ocaml.lib_config.stdlib_dir :: default_ocamlpath
      else default_ocamlpath
    in
    let installed_env = installed_env env name findlib env_nodes ocaml.version profile in
    if Option.is_some fdo_target_exe then Ocaml_toolchain.check_fdo_support ocaml name;
    let supports_shared_libraries =
      Ocaml_config.supports_shared_libraries ocaml.ocaml_config
    in
    let dynamically_linked_foreign_archives =
      supports_shared_libraries && dynamically_linked_foreign_archives
    in
    Ocaml_toolchain.register_response_file_support ocaml;
    Memo.return
      { name
      ; implicit
      ; kind
      ; profile
      ; merlin
      ; fdo_target_exe
      ; dynamically_linked_foreign_archives
      ; env_nodes
      ; for_host = host
      ; build_dir = Context_name.build_dir name
      ; path
      ; ocaml
      ; installed_env
      ; findlib_paths = ocamlpath @ default_ocamlpath
      ; findlib_toolchain
      ; default_ocamlpath
      ; supports_shared_libraries =
          Dynlink_supported.By_the_os.of_bool supports_shared_libraries
      ; build_context = Build_context.create ~name
      ; instrument_with
      }
  in
  let implicit =
    not
      (List.mem
         targets
         ~equal:Workspace.Context.Target.equal
         Workspace.Context.Target.Native)
  in
  let* native =
    create_one
      ~host:host_context
      ~findlib_toolchain:host_toolchain
      ~implicit
      ~name
      ~merlin
  in
  let+ others =
    Memo.parallel_map targets ~f:(function
      | Native -> Memo.return None
      | Named findlib_toolchain ->
        let name = Context_name.target name ~toolchain:findlib_toolchain in
        create_one
          ~implicit:false
          ~name
          ~host:(Some native)
          ~merlin:false
          ~findlib_toolchain:(Some findlib_toolchain)
        >>| Option.some)
  in
  { native; targets = List.filter_opt others }
;;

let which t fname = Which.which ~path:t.path fname

let extend_paths t ~env =
  let t =
    let f (var, t) =
      let parse ~loc:_ s = s in
      let standard = Env_path.path env |> List.map ~f:Path.to_string in
      var, Ordered_set_lang.eval t ~parse ~standard ~eq:String.equal
    in
    List.map ~f t
  in
  let vars =
    let to_absolute_filename s = Path.of_string s |> Path.to_absolute_filename in
    let sep = String.make 1 Bin.path_sep in
    let env = Env.Map.of_list_exn t in
    let f l = String.concat ~sep (List.map ~f:to_absolute_filename l) in
    Env.Map.map ~f env
  in
  Env.extend ~vars env
;;

let default
  ~merlin
  ~env_nodes
  ~env
  ~targets
  ~fdo_target_exe
  ~dynamically_linked_foreign_archives
  ~instrument_with
  =
  let path = Env_path.path env in
  create
    ~kind:Default
    ~path
    ~env
    ~env_nodes
    ~merlin
    ~targets
    ~fdo_target_exe
    ~dynamically_linked_foreign_archives
    ~instrument_with
;;

let create_for_opam
  ~loc
  ~root
  ~env
  ~env_nodes
  ~targets
  ~profile
  ~switch
  ~name
  ~merlin
  ~host_context
  ~host_toolchain
  ~fdo_target_exe
  ~dynamically_linked_foreign_archives
  ~instrument_with
  =
  let* vars = Opam.env ~env ~root ~switch in
  if not (Env.Map.mem vars Build_environment_kind.opam_switch_prefix_var_name)
  then
    User_error.raise
      ~loc
      [ Pp.textf
          "opam doesn't set the environment variable %s. I cannot create an opam build \
           context without opam setting this variable."
          Build_environment_kind.opam_switch_prefix_var_name
      ];
  let path =
    match Env.Map.find vars Env_path.var with
    | None -> Env_path.path env
    | Some s -> Bin.parse_path s
  in
  let env = Env.extend env ~vars in
  create
    ~kind:(Opam { root; switch })
    ~profile
    ~targets
    ~path
    ~env
    ~env_nodes
    ~name
    ~merlin
    ~host_context
    ~host_toolchain
    ~fdo_target_exe
    ~dynamically_linked_foreign_archives
    ~instrument_with
;;

module rec Instantiate : sig
  val instantiate : Context_name.t -> instance Memo.t
end = struct
  let instantiate_impl name : instance Memo.t =
    let env = Global.env () in
    let* workspace = Workspace.workspace () in
    let context =
      List.find_exn workspace.contexts ~f:(fun ctx ->
        Context_name.equal (Workspace.Context.name ctx) name)
    in
    let* host_context =
      match Workspace.Context.host_context context with
      | None -> Memo.return None
      | Some context_name ->
        let+ { native; targets = _ } = Instantiate.instantiate context_name in
        Some native
    in
    let env_nodes =
      let context = Workspace.Context.env context in
      { Env_nodes.context; workspace = workspace.env }
    in
    match context with
    | Default
        { lock = _
        ; version_preference = _
        ; solver_env = _
        ; base =
            { targets
            ; name
            ; host_context = _
            ; profile
            ; env = _
            ; toolchain
            ; paths
            ; loc = _
            ; fdo_target_exe
            ; dynamically_linked_foreign_archives
            ; instrument_with
            ; merlin = _
            }
        } ->
      let merlin = workspace.merlin_context = Some (Workspace.Context.name context) in
      let host_toolchain : Context_name.t option =
        match toolchain with
        | Some _ -> toolchain
        | None ->
          let open Option.O in
          let+ name = Env.get env "OCAMLFIND_TOOLCHAIN" in
          Context_name.parse_string_exn (Loc.none, name)
      in
      let env = extend_paths ~env paths in
      default
        ~env
        ~env_nodes
        ~profile
        ~targets
        ~name
        ~merlin
        ~host_context
        ~host_toolchain
        ~fdo_target_exe
        ~dynamically_linked_foreign_archives
        ~instrument_with
    | Opam
        { base =
            { targets
            ; name
            ; host_context = _
            ; profile
            ; env = _
            ; toolchain
            ; paths
            ; loc
            ; fdo_target_exe
            ; dynamically_linked_foreign_archives
            ; instrument_with
            ; merlin
            }
        ; switch
        ; root
        } ->
      let env = extend_paths ~env paths in
      create_for_opam
        ~loc
        ~root
        ~env_nodes
        ~env
        ~profile
        ~switch
        ~name
        ~merlin
        ~targets
        ~host_context
        ~host_toolchain:toolchain
        ~fdo_target_exe
        ~dynamically_linked_foreign_archives
        ~instrument_with
  ;;

  let memo =
    Memo.create "instantiate-context" ~input:(module Context_name) instantiate_impl
  ;;

  let instantiate name = Memo.exec memo name
end

module DB = struct
  let all =
    let impl () =
      let* workspace = Workspace.workspace () in
      let+ contexts =
        Memo.parallel_map workspace.contexts ~f:(fun c ->
          let+ { native; targets } = Instantiate.instantiate (Workspace.Context.name c) in
          native :: targets)
      in
      let all = List.concat contexts in
      List.iter all ~f:(fun t ->
        let open Pp.O in
        Log.info
          [ Pp.box ~indent:1 (Pp.text "Dune context:" ++ Pp.cut ++ Dyn.pp (to_dyn t)) ]);
      all
    in
    let memo = Memo.lazy_ ~name:"build-contexts" impl in
    fun () -> Memo.Lazy.force memo
  ;;

  let get =
    let memo =
      Memo.create
        "context-db-get"
        ~input:(module Context_name)
        (fun name ->
          let+ contexts = all () in
          List.find_exn contexts ~f:(fun c -> Context_name.equal name c.name))
    in
    Memo.exec memo
  ;;

  let create_db ~name f =
    let map =
      Memo.lazy_ ~name (fun () ->
        let+ map = all () in
        Context_name.Map.of_list_map_exn map ~f:(fun context ->
          context.name, Memo.lazy_ (fun () -> f context)))
    in
    Staged.stage (fun context ->
      let* map = Memo.Lazy.force map in
      match Context_name.Map.find map context with
      | Some v -> Memo.Lazy.force v
      | None ->
        Code_error.raise "invalid context" [ "context", Context_name.to_dyn context ])
  ;;

  let by_dir dir =
    let context =
      match Install.Context.of_path dir with
      | Some name -> name
      | None ->
        Code_error.raise
          "directory does not have an associated context"
          [ "dir", Path.Build.to_dyn dir ]
    in
    get context
  ;;
end

let name t = t.name

let map_exe (context : t) =
  match context.for_host with
  | None -> fun exe -> exe
  | Some (host : t) ->
    fun exe ->
      (match Path.extract_build_context_dir exe with
       | Some (dir, exe) when Path.equal dir (Path.build context.build_dir) ->
         Path.append_source (Path.build host.build_dir) exe
       | _ -> exe)
;;

let host t = Option.value ~default:t t.for_host

let roots t =
  let module Roots = Install.Roots in
  let prefix_roots =
    match Env.get t.installed_env Build_environment_kind.opam_switch_prefix_var_name with
    | None ->
      { Roots.lib_root = None
      ; libexec_root = None
      ; bin = None
      ; sbin = None
      ; etc_root = None
      ; doc_root = None
      ; share_root = None
      ; man = None
      }
    | Some prefix ->
      let prefix = Path.of_filename_relative_to_initial_cwd prefix in
      Roots.opam_from_prefix prefix |> Roots.map ~f:(fun s -> Some s)
  in
  match t.kind with
  | Default ->
    let setup_roots = Roots.map ~f:(Option.map ~f:Path.of_string) Setup.roots in
    Roots.first_has_priority setup_roots prefix_roots
  | Opam _ -> prefix_roots
;;
