open Import
open Memo.O

include struct
  open Dune_pkg
  module Sys_poll = Sys_poll
  module Package_variable = Package_variable
  module Substs = Substs
  module Checksum = Checksum
  module Source = Source
  module Build_command = Lock_dir.Build_command
end

module Variable = struct
  type value = OpamVariable.variable_contents =
    | B of bool
    | S of string
    | L of string list

  type t = Package_variable_name.t * value

  let dyn_of_value : value -> Dyn.t =
    let open Dyn in
    function
    | B b -> variant "Bool" [ bool b ]
    | S s -> variant "String" [ string s ]
    | L xs -> variant "Strings" [ list string xs ]
  ;;

  let dune_value : value -> Value.t list = function
    | B b -> [ String (Bool.to_string b) ]
    | S s -> [ String s ]
    | L s -> List.map s ~f:(fun x -> Value.String x)
  ;;

  let of_values : dir:Path.t -> Value.t list -> value =
    fun ~dir xs ->
    match List.map xs ~f:(Value.to_string ~dir) with
    | [ x ] -> S x
    | xs -> L xs
  ;;

  let to_dyn (name, value) =
    Dyn.(pair Package_variable_name.to_dyn dyn_of_value (name, value))
  ;;
end

module Pkg_info = struct
  include Dune_pkg.Lock_dir.Pkg_info

  let variables t =
    Package_variable_name.Map.of_list_exn
      [ Package_variable_name.name, Variable.S (Package.Name.to_string t.name)
      ; Package_variable_name.version, S (Package_version.to_string t.version)
      ; Package_variable_name.dev, B t.dev
      ]
  ;;
end

module Paths = struct
  type t =
    { source_dir : Path.Build.t
    ; target_dir : Path.Build.t
    ; extra_sources : Path.Build.t
    ; name : Package.Name.t
    ; install_roots : Path.Build.t Install.Roots.t Lazy.t
    ; install_paths : Path.Build.t Install.Paths.t Lazy.t
    }

  let install_roots ~target_dir =
    Install.Roots.opam_from_prefix ~relative:Path.Build.relative target_dir
  ;;

  let install_paths roots package =
    Install.Paths.make ~relative:Path.Build.relative ~package ~roots
  ;;

  let of_root name ~root =
    let source_dir = Path.Build.relative root "source" in
    let target_dir = Path.Build.relative root "target" in
    let extra_sources = Path.Build.relative root "extra_source" in
    let install_roots = lazy (install_roots ~target_dir) in
    let install_paths = lazy (install_paths (Lazy.force install_roots) name) in
    { source_dir; target_dir; extra_sources; name; install_paths; install_roots }
  ;;

  let extra_source t extra_source = Path.Build.append_local t.extra_sources extra_source

  let make name (ctx : Context_name.t) =
    let build_dir =
      Path.Build.relative Private_context.t.build_dir (Context_name.to_string ctx)
    in
    let root = Path.Build.L.relative build_dir [ ".pkg"; Package.Name.to_string name ] in
    of_root name ~root
  ;;

  let install_cookie' target_dir = Path.Build.relative target_dir "cookie"
  let install_cookie t = install_cookie' t.target_dir

  let install_file t =
    Path.Build.relative
      t.source_dir
      (sprintf "%s.install" (Package.Name.to_string t.name))
  ;;

  let config_file t =
    Path.Build.relative t.source_dir (sprintf "%s.config" (Package.Name.to_string t.name))
  ;;

  let install_paths t = Lazy.force t.install_paths
  let install_roots t = Lazy.force t.install_roots
end

module Install_cookie = struct
  (* The install cookie represents a serialized representation of all the
     installed artifacts and variables. *)

  type t =
    { files : Path.t list Section.Map.t
    ; variables : Variable.t list
    }

  let to_dyn { files; variables } =
    let open Dyn in
    record
      [ "files", Section.Map.to_dyn (list Path.to_dyn) files
      ; "variables", list Variable.to_dyn variables
      ]
  ;;

  include Dune_util.Persistent.Make (struct
      type nonrec t = t

      let name = "INSTALL-COOKIE"
      let version = 1
      let to_dyn = to_dyn
      let test_example () = { files = Section.Map.empty; variables = [] }
    end)

  let load_exn f =
    match load f with
    | Some f -> f
    | None -> User_error.raise ~loc:(Loc.in_file f) [ Pp.text "unable to load" ]
  ;;
end

module Value_list_env = struct
  (* A representation of an environment where each variable can hold a
     list of [Value.t]. Each variable will be encoded into a delimited
     string (in the style of the PATH variable). *)
  type t = Value.t list Env.Map.t

  let parse_strings s = Bin.parse s |> List.map ~f:(fun s -> Value.String s)
  let of_env env : t = Env.to_map env |> Env.Map.map ~f:parse_strings

  (* Concatenate a list of values in the style of lists found in
     environment variables, such as PATH *)
  let string_of_env_values values =
    List.map values ~f:(function
      | Value.String s -> s
      | Dir s | Path s -> Path.to_absolute_filename s)
    |> Bin.encode_strings
  ;;

  let to_env (t : t) = Env.Map.map t ~f:string_of_env_values |> Env.of_map
  let get_path t = Env.Map.find t Env_path.var

  (* [extend_concat_path a b] adds all variables from [b] to [a]
     overwriting any existing values of those variables in [a] except for PATH
     which is set to the concatenation of the PATH variables from [a] and [b]
     with the PATH entries from [b] preceding the PATH entries from
     [a]. If only one of the arguments contains a PATH variable then
     its value will be the value of PATH in the result, however if
     neither argument contains a PATH variable then PATH will be unset
     in the result. *)
  let extend_concat_path a b =
    let extended = Env.Map.superpose b a in
    let concated_path =
      match get_path a, get_path b with
      | None, None -> None
      | Some x, None | None, Some x -> Some x
      | Some a, Some b -> Some (b @ a)
    in
    match concated_path with
    | None -> extended
    | Some concated_path -> Env.Map.set extended Env_path.var concated_path
  ;;

  (* Adds a path to an env where variables are associated with lists
     of paths. The path is prepended to the list associated with the
     given variable and a new binding is added to the env if the
     variable is not yet part of the env. *)
  let add_path (t : t) var path : t =
    Env.Map.update t var ~f:(fun paths ->
      let paths = Option.value paths ~default:[] in
      Some (Value.Dir (Path.build path) :: paths))
  ;;
end

module Env_update = struct
  include Dune_lang.Action.Env_update

  (* Handle the :=, +=, =:, and =+ opam environment update operators.

     The operators with colon character update a variable, adding a
     leading/trailing separator (e.g. the ':' chars in PATH on unix)
     if the variable was initially unset or empty, while the operators
     with a plus character add no leading/trailing separator in such a
     case.

     Updates where the newly added value is the empty string are
     ignored since opam refuses to add empty strings to list
     variables.*)
  let update kind ~new_v ~old_v ~f =
    if new_v = ""
    then old_v
    else (
      match kind with
      | `Colon ->
        let old_v = Option.value ~default:[] old_v in
        Some (f ~old_v ~new_v)
      | `Plus ->
        (match old_v with
         | None | Some [] -> Some [ Value.String new_v ]
         | Some old_v -> Some (f ~old_v ~new_v)))
  ;;

  let append = update ~f:(fun ~old_v ~new_v -> old_v @ [ Value.String new_v ])
  let prepend = update ~f:(fun ~old_v ~new_v -> Value.String new_v :: old_v)

  let set env { op; var = k; value = new_v } =
    Env.Map.update env k ~f:(fun old_v ->
      let append = append ~new_v ~old_v in
      let prepend = prepend ~new_v ~old_v in
      match op with
      | Eq ->
        if new_v = ""
        then if Sys.win32 then None else Some [ String "" ]
        else Some [ Value.String new_v ]
      | PlusEq -> prepend `Plus
      | ColonEq -> prepend `Colon
      | EqPlus -> append `Plus
      | EqColon -> append `Colon
      | EqPlusEq ->
        (* TODO nobody uses this AFAIK *)
        assert false)
  ;;
end

module Pkg = struct
  module Id = Id.Make ()

  type t =
    { id : Id.t
    ; build_command : Build_command.t option
    ; install_command : Dune_lang.Action.t option
    ; depends : t list
    ; info : Pkg_info.t
    ; paths : Paths.t
    ; files_dir : Path.Build.t
    ; mutable exported_env : string Env_update.t list
    }

  module Top_closure = Top_closure.Make (Id.Set) (Monad.Id)

  let top_closure depends =
    match
      Top_closure.top_closure depends ~key:(fun t -> t.id) ~deps:(fun t -> t.depends)
    with
    | Ok s -> s
    | Error cycle ->
      User_error.raise
        [ Pp.text "the following packages form a cycle:"
        ; Pp.chain cycle ~f:(fun pkg ->
            Pp.verbatim (Package.Name.to_string pkg.info.name))
        ]
  ;;

  let deps_closure t = top_closure t.depends

  let source_files t ~loc =
    let skip_dir = function
      | ".hg" | ".git" | "_darcs" | "_opam" | "_build" | "_esy" -> true
      | _ -> false
    in
    let skip_file = String.is_prefix ~prefix:".#" in
    let rec loop root acc path =
      let full_path = Path.External.append_local root path in
      Fs_memo.dir_contents (External full_path)
      >>= function
      | Error e ->
        User_error.raise
          ~loc
          [ Pp.textf "Unable to read %s" (Path.External.to_string_maybe_quoted full_path)
          ; Unix_error.Detailed.pp e
          ]
      | Ok contents ->
        let files, dirs =
          let contents = Fs_cache.Dir_contents.to_list contents in
          List.rev_filter_partition_map contents ~f:(fun (name, kind) ->
            (* TODO handle links and cycles correctly *)
            match kind with
            | S_DIR -> if skip_dir name then Skip else Right name
            | _ -> if skip_file name then Skip else Left name)
        in
        let acc =
          Path.Local.Set.of_list_map files ~f:(Path.Local.relative path)
          |> Path.Local.Set.union acc
        in
        let+ dirs =
          Memo.parallel_map dirs ~f:(fun dir ->
            let dir = Path.Local.relative path dir in
            loop root Path.Local.Set.empty dir)
        in
        Path.Local.Set.union_all (acc :: dirs)
    in
    (match t.info.source with
     | None -> Memo.return None
     | Some source ->
       Lock_dir.source_kind source
       >>| (function
        | `Local (`File, _) | `Fetch -> None
        | `Local (`Directory, root) -> Some root))
    >>= function
    | None -> Memo.return Path.Local.Set.empty
    | Some root -> loop root Path.Local.Set.empty Path.Local.root
  ;;

  let dep t = Dep.file (Path.build t.paths.target_dir)

  let package_deps t =
    deps_closure t
    |> List.fold_left ~init:Dep.Set.empty ~f:(fun acc t -> dep t |> Dep.Set.add acc)
  ;;

  (* Given a list of packages, construct an env containing variables
     set by each package. Variables containing delimited lists of
     paths (e.g. PATH) which appear in multiple package's envs are
     concatenated in the reverse order of their associated packages in
     the input list. Environment updates via the `exported_env` field
     (equivalent to opam's `setenv` field) are applied for each
     package in the same order as the argument list. *)
  let build_env_of_deps ts =
    List.fold_left ts ~init:Env.Map.empty ~f:(fun env t ->
      let env =
        let roots = Paths.install_roots t.paths in
        let init = Value_list_env.add_path env Env_path.var roots.bin in
        let vars = Install.Roots.to_env_without_path roots in
        List.fold_left vars ~init ~f:(fun acc (var, path) ->
          Value_list_env.add_path acc var path)
      in
      List.fold_left t.exported_env ~init:env ~f:Env_update.set)
  ;;

  (* [build_env t] returns an env containing paths containing all the
     tools and libraries required to build the package [t] inside the
     faux opam directory contained in the _build dir. *)
  let build_env t = build_env_of_deps @@ deps_closure t

  let base_env t =
    Env.Map.of_list_exn
      [ ( Opam_switch.opam_switch_prefix_var_name
        , [ Value.Path (Path.build t.paths.target_dir) ] )
      ; "CDPATH", [ Value.String "" ]
      ; "MAKELEVEL", [ Value.String "" ]
      ; "OPAM_PACKAGE_NAME", [ Value.String (Package.Name.to_string t.info.name) ]
      ; ( "OPAM_PACKAGE_VERSION"
        , [ Value.String (Package_version.to_string t.info.version) ] )
      ; "OPAMCLI", [ Value.String "2.0" ]
      ]
  ;;

  (* [exported_value_env t] returns the complete env that will be used
     to build the package [t] *)
  let exported_value_env t =
    let package_env = build_env t |> Env.Map.superpose (base_env t) in
    (* TODO: Run actions in a constrained environment. [Global.env ()] is the
       environment from which dune was executed, and some of the environment
       variables may affect builds in unintended ways and make builds less
       reproducible. However other environment variables must be set in order
       for build actions to run successfully, such as $PATH on systems where the
       shell's default $PATH variable doesn't include the location of standard
       programs or build tools (e.g. NixOS). *)
    Value_list_env.extend_concat_path (Value_list_env.of_env (Global.env ())) package_env
  ;;

  let exported_env t = Value_list_env.to_env @@ exported_value_env t
end

module Pkg_installed = struct
  type t = { cookie : Install_cookie.t Action_builder.t }

  let of_paths (paths : Paths.t) =
    let cookie =
      let open Action_builder.O in
      let path = Path.build @@ Paths.install_cookie paths in
      let+ () = path |> Dep.file |> Action_builder.dep in
      Install_cookie.load_exn path
    in
    { cookie }
  ;;
end

module Expander0 = struct
  include Expander0

  type t =
    { paths : Paths.t
    ; artifacts : Path.t Filename.Map.t
    ; depends : (Variable.value Package_variable_name.Map.t * Paths.t) Package.Name.Map.t
    ; context : Context_name.t
    ; version : Package_version.t
    ; env : Value.t list Env.Map.t
    }

  let expand_pform_fdecl
    : (t
       -> source:Dune_sexp.Template.Pform.t
       -> Pform.t
       -> (Value.t list, [ `Undefined_pkg_var of Package_variable_name.t ]) result Memo.t)
        Fdecl.t
    =
    Fdecl.create Dyn.opaque
  ;;
end

module Substitute = struct
  include Substs.Make (Memo)
  module Expander = Expander0

  module Spec = struct
    type ('src, 'dst) t =
      { (* XXX it's not good to serialize the substitution map like this. We're
           essentially implementing the same substitution procedure but in two
           different places: action geeneration, and action execution.

           The two implementations are bound to drift. Better would be to
           reconstruct everything that is needed to call our one and only
           substitution function. *)
        expander : Expander.t
      ; src : 'src
      ; dst : 'dst
      }

    let name = "substitute"
    let version = 1
    let bimap t f g = { t with src = f t.src; dst = g t.dst }
    let is_useful_to ~memoize = memoize

    let encode { expander; src; dst } input output : Dune_lang.t =
      let e =
        let paths (p : Paths.t) = p.source_dir, p.target_dir, p.name in
        ( paths expander.paths
        , expander.artifacts
        , Package.Name.Map.to_list_map expander.depends ~f:(fun _ (m, p) -> m, paths p)
        , expander.version
        , expander.context
        , expander.env )
        |> Digest.generic
        |> Digest.to_string_raw
        |> Dune_sexp.atom_or_quoted_string
      in
      List [ Dune_lang.atom_or_quoted_string name; e; input src; output dst ]
    ;;

    let action { expander; src; dst } ~ectx:_ ~eenv:_ =
      let open Fiber.O in
      let* () = Fiber.return () in
      let env (var : Substs.Variable.t) =
        let open Memo.O in
        ((* TODO loc *)
         let loc = Loc.none in
         let source =
           (* TODO it's rather ugly that we're going through the pform machinery
              to do this *)
           { Dune_sexp.Template.Pform.loc; name = ""; payload = None }
         in
         match
           match var with
           | Package var -> Some (Package_variable.to_pform var)
           | Global n ->
             Package_variable_name.to_string n
             |> Pform.Var.of_opam_global_variable_name
             |> Option.map ~f:(fun v -> Pform.Var v)
         with
         | None -> Memo.return @@ Variable.S ""
         | Some pform ->
           (Fdecl.get Expander.expand_pform_fdecl) expander ~source pform
           >>| (function
            | Error (`Undefined_pkg_var _) ->
              (* these are opam's semantics as far as I understand. *)
              Variable.S ""
            | Ok v ->
              let dir = Path.parent_exn src |> Path.drop_optional_sandbox_root in
              Variable.of_values v ~dir))
        >>| Option.some
      in
      subst env expander.paths.name ~src ~dst |> Memo.run
    ;;
  end

  let action expander ~src ~dst =
    let module M = struct
      type path = Path.t
      type target = Path.Build.t

      module Spec = Spec

      let v = { Spec.expander; src; dst }
    end
    in
    Action.Extension (module M)
  ;;
end

module Run_with_path = struct
  module Spec = struct
    type 'path chunk =
      | String of string
      | Path of 'path

    type 'path arg = 'path chunk Array.Immutable.t

    type ('path, 'target) t =
      { prog : Action.Prog.t
      ; args : 'path arg Array.Immutable.t
      ; ocamlfind_destdir : 'path
      }

    let name = "run-with-path"
    let version = 1

    let map_arg arg ~f =
      Array.Immutable.map arg ~f:(function
        | String _ as s -> s
        | Path p -> Path (f p))
    ;;

    let bimap t f _g =
      { t with
        args = Array.Immutable.map t.args ~f:(map_arg ~f)
      ; ocamlfind_destdir = f t.ocamlfind_destdir
      }
    ;;

    let is_useful_to ~memoize:_ = true

    let encode { prog; args; ocamlfind_destdir } path _ : Dune_lang.t =
      let prog =
        Dune_lang.atom_or_quoted_string
        @@
        match prog with
        | Ok p -> Path.reach p ~from:Path.root
        | Error e -> e.program
      in
      let args =
        Array.Immutable.to_list_map args ~f:(fun x ->
          Dune_lang.List
            (Array.Immutable.to_list_map x ~f:(function
              | String s -> Dune_lang.atom_or_quoted_string s
              | Path p -> path p)))
      in
      List
        [ List ([ Dune_lang.atom_or_quoted_string name; prog ] @ args)
        ; path ocamlfind_destdir
        ]
    ;;

    let action
      { prog; args; ocamlfind_destdir }
      ~(ectx : Action.Ext.context)
      ~(eenv : Action.Ext.env)
      =
      let open Fiber.O in
      match prog with
      | Error e -> Action.Prog.Not_found.raise e
      | Ok prog ->
        let args =
          Array.Immutable.to_list_map args ~f:(fun arg ->
            Array.Immutable.to_list_map arg ~f:(function
              | String s -> s
              | Path p -> Path.to_absolute_filename p)
            |> String.concat ~sep:"")
        in
        let metadata = Process.create_metadata ~purpose:ectx.purpose () in
        let env =
          Env.add
            eenv.env
            ~var:"OCAMLFIND_DESTDIR"
            ~value:(Path.to_absolute_filename ocamlfind_destdir)
        in
        Process.run
          (Accept eenv.exit_codes)
          prog
          args
          ~display:!Clflags.display
          ~metadata
          ~stdout_to:eenv.stdout_to
          ~stderr_to:eenv.stderr_to
          ~stdin_from:eenv.stdin_from
          ~dir:eenv.working_dir
          ~env
        >>= (function
         | Error _ -> Fiber.return ()
         | Ok () -> Fiber.return ())
    ;;
  end

  let action prog args ~ocamlfind_destdir =
    let module M = struct
      type path = Path.t
      type target = Path.Build.t

      module Spec = Spec

      let v = { Spec.prog; args; ocamlfind_destdir }
    end
    in
    Action.Extension (module M)
  ;;
end

module Action_expander = struct
  module Expander = struct
    include Expander0

    let map_exe _ x =
      (* TODO *)
      x
    ;;

    let dune_section_of_pform : Pform.Var.Pkg.Section.t -> Section.t = function
      | Lib -> Lib
      | Libexec -> Libexec
      | Bin -> Bin
      | Sbin -> Sbin
      | Toplevel -> Toplevel
      | Share -> Share
      | Etc -> Etc
      | Doc -> Doc
      | Stublibs -> Stublibs
      | Man -> Man
    ;;

    let section_dir_of_root
      (roots : _ Install.Roots.t)
      (section : Pform.Var.Pkg.Section.t)
      =
      match section with
      | Lib -> roots.lib_root
      | Libexec -> roots.libexec_root
      | Bin -> roots.bin
      | Sbin -> roots.sbin
      | Share -> roots.share_root
      | Etc -> roots.etc_root
      | Doc -> roots.doc_root
      | Man -> roots.man
      | Toplevel -> Path.Build.relative roots.lib_root "toplevel"
      | Stublibs -> Path.Build.relative roots.lib_root "stublibs"
    ;;

    let sys_poll_var accessor =
      accessor Lock_dir.Sys_vars.poll
      |> Memo.Lazy.force
      >>| function
      | Some v -> [ Value.String v ]
      | None ->
        (* TODO: in OPAM an unset variable evaluates to false, but we
           can't represent that in a string so it evaluates to an empty
           string instead *)
        [ Value.String "" ]
    ;;

    let expand_pkg (paths : Paths.t) (pform : Pform.Var.Pkg.t) =
      match pform with
      | Switch -> Memo.return [ Value.String "dune" ]
      | Os -> sys_poll_var (fun { os; _ } -> os)
      | Os_version -> sys_poll_var (fun { os_version; _ } -> os_version)
      | Os_distribution -> sys_poll_var (fun { os_distribution; _ } -> os_distribution)
      | Os_family -> sys_poll_var (fun { os_family; _ } -> os_family)
      | Sys_ocaml_version ->
        sys_poll_var (fun { sys_ocaml_version; _ } -> sys_ocaml_version)
      | Build -> Memo.return [ Value.Dir (Path.build paths.source_dir) ]
      | Prefix -> Memo.return [ Value.Dir (Path.build paths.target_dir) ]
      | User -> Memo.return [ Value.String (Unix.getlogin ()) ]
      | Jobs -> Memo.return [ Value.String "1" ]
      | Arch -> sys_poll_var (fun { arch; _ } -> arch)
      | Group ->
        let group = Unix.getgid () |> Unix.getgrgid in
        Memo.return [ Value.String group.gr_name ]
      | Section_dir section ->
        let roots = Paths.install_roots paths in
        let dir = section_dir_of_root roots section in
        Memo.return [ Value.Dir (Path.build dir) ]
    ;;

    let expand_pkg_macro ~loc (paths : Paths.t) deps macro_invocation =
      let { Package_variable.name = variable_name; scope } =
        match Package_variable.of_macro_invocation ~loc macro_invocation with
        | Ok package_variable -> package_variable
        | Error `Unexpected_macro ->
          Code_error.raise
            "Attempted to treat an unexpected macro invocation as a package variable \
             encoding"
            []
      in
      let variables, paths =
        let package_name =
          match scope with
          | Self -> paths.name
          | Package package_name -> package_name
        in
        match Package.Name.Map.find deps package_name with
        | None -> Package_variable_name.Map.empty, None
        | Some (var, paths) -> var, Some paths
      in
      match Package_variable_name.Map.find variables variable_name with
      | Some v -> Memo.return @@ Ok (Variable.dune_value v)
      | None ->
        let present = Option.is_some paths in
        (* TODO we should be looking it up in all packages now *)
        (match Package_variable_name.to_string variable_name with
         | "pinned" -> Memo.return @@ Ok [ Value.false_ ]
         | "enable" ->
           Memo.return @@ Ok [ Value.String (if present then "enable" else "disable") ]
         | "installed" -> Memo.return @@ Ok [ Value.String (Bool.to_string present) ]
         | _ ->
           (match paths with
            | None -> Memo.return (Error (`Undefined_pkg_var variable_name))
            | Some paths ->
              (match
                 Pform.Var.Pkg.Section.of_string
                   (Package_variable_name.to_string variable_name)
               with
               | None -> Memo.return (Error (`Undefined_pkg_var variable_name))
               | Some section ->
                 let section = dune_section_of_pform section in
                 let install_paths = Paths.install_paths paths in
                 Memo.return
                 @@ Ok
                      [ Value.Dir (Path.build (Install.Paths.get install_paths section)) ])))
    ;;

    let expand_pform
      { env = _; paths; artifacts = _; context; depends; version = _ }
      ~source
      (pform : Pform.t)
      : (Value.t list, [ `Undefined_pkg_var of Package_variable_name.t ]) result Memo.t
      =
      let loc = Dune_sexp.Template.Pform.loc source in
      match pform with
      | Var (Pkg var) -> expand_pkg paths var >>| Result.ok
      | Var Context_name ->
        Memo.return (Ok [ Value.String (Context_name.to_string context) ])
      | Var Make ->
        let+ make =
          let path = Env_path.path (Global.env ()) in
          Make_prog.which loc context ~path
        in
        Ok [ Value.Path make ]
      | Macro ({ macro = Pkg | Pkg_self; _ } as macro_invocation) ->
        expand_pkg_macro ~loc paths depends macro_invocation
      | _ -> Expander0.isn't_allowed_in_this_position ~source
    ;;

    let () = Fdecl.set expand_pform_fdecl expand_pform

    let expand_pform_gen t =
      String_expander.Memo.expand
        ~dir:(Path.build t.paths.source_dir)
        ~f:(fun ~source pform ->
          expand_pform t ~source pform
          >>| function
          | Ok x -> x
          | Error (`Undefined_pkg_var variable_name) ->
            User_error.raise
              ~loc:(Dune_sexp.Template.Pform.loc source)
              [ Pp.textf
                  "Undefined package variable: %s"
                  (Package_variable_name.to_string variable_name)
              ])
    ;;

    let expand_exe_value t value ~loc =
      let+ prog =
        match value with
        | Value.Dir p ->
          User_error.raise
            ~loc
            [ Pp.textf
                "%s is a directory and cannot be used as an executable"
                (Path.to_string_maybe_quoted p)
            ]
        | Path p -> Memo.return @@ Ok p
        | String program ->
          (match Filename.analyze_program_name program with
           | Relative_to_current_dir | Absolute ->
             let dir = Path.build t.paths.source_dir in
             Memo.return @@ Ok (Path.relative dir program)
           | In_path ->
             (match Filename.Map.find t.artifacts program with
              | Some s -> Memo.return @@ Ok s
              | None ->
                (let path = Global.env () |> Env_path.path in
                 Which.which ~path program)
                >>| (function
                 | Some p -> Ok p
                 | None ->
                   Error
                     (Action.Prog.Not_found.create
                        ~program
                        ~context:t.context
                        ~loc:(Some loc)
                        ()))))
      in
      Result.map prog ~f:(map_exe t)
    ;;

    let slang_expander t sw =
      String_expander.Memo.expand_result_deferred_concat sw ~mode:Many ~f:(expand_pform t)
    ;;

    let eval_blang t blang =
      Slang_expand.eval_blang
        blang
        ~dir:(Path.build t.paths.source_dir)
        ~f:(slang_expander t)
    ;;

    let eval_slangs_located t slangs =
      Slang_expand.eval_multi_located
        slangs
        ~dir:(Path.build t.paths.source_dir)
        ~f:(slang_expander t)
    ;;
  end

  let rec expand (action : Dune_lang.Action.t) ~(expander : Expander.t) =
    let dir = Path.build expander.paths.source_dir in
    match action with
    | Run args ->
      Expander.eval_slangs_located expander args
      >>= (function
       | [] ->
         let loc =
           let loc = function
             | Slang.Nil -> None
             | Literal sw -> Some (String_with_vars.loc sw)
             | Form (loc, _) -> Some loc
           in
           let start = List.find_map args ~f:loc in
           let stop =
             List.fold_left args ~init:None ~f:(fun last a ->
               match loc a with
               | None -> last
               | Some _ as s -> s)
           in
           Option.both start stop
           |> Option.map ~f:(fun (start, stop) -> Loc.span start stop)
         in
         User_error.raise
           ?loc
           [ Pp.text "\"run\" action must have at least one argument" ]
       | (prog_loc, prog) :: args ->
         let+ exe =
           let prog = Value.Deferred_concat.force prog ~dir in
           Expander.expand_exe_value expander prog ~loc:prog_loc
         in
         let args =
           Array.Immutable.of_list_map args ~f:(fun (_loc, arg) ->
             Value.Deferred_concat.parts arg
             |> Array.Immutable.of_list_map ~f:(fun (arg : Value.t) ->
               match arg with
               | String s -> Run_with_path.Spec.String s
               | Path p | Dir p -> Path p))
         in
         let ocamlfind_destdir =
           (Lazy.force expander.paths.install_roots).lib_root |> Path.build
         in
         Run_with_path.action exe args ~ocamlfind_destdir)
    | Progn t ->
      let+ args = Memo.parallel_map t ~f:(expand ~expander) in
      Action.Progn args
    | System arg ->
      let+ arg =
        Expander.expand_pform_gen ~mode:Single expander arg >>| Value.to_string ~dir
      in
      Action.System arg
    | Patch p ->
      let+ patch =
        Expander.expand_pform_gen ~mode:Single expander p >>| Value.to_path ~dir
      in
      Dune_patch.action ~patch
    | Substitute (src, dst) ->
      let+ src =
        Expander.expand_pform_gen ~mode:Single expander src >>| Value.to_path ~dir
      and+ dst =
        Expander.expand_pform_gen ~mode:Single expander dst
        >>| Value.to_path ~dir
        >>| Expander0.as_in_build_dir ~what:"substitute" ~loc:(String_with_vars.loc dst)
      in
      Substitute.action expander ~src ~dst
    | Withenv (updates, action) -> expand_withenv expander updates action
    | When (condition, action) ->
      Expander.eval_blang expander condition
      >>= (function
       | true -> expand action ~expander
       | false -> Memo.return (Action.progn []))
    | Write_file (path_sw, perm, contents_sw) ->
      let+ path =
        Expander.expand_pform_gen ~mode:Single expander path_sw
        >>| Value.to_path ~dir
        >>| Expander0.as_in_build_dir
              ~what:"write-file"
              ~loc:(String_with_vars.loc path_sw)
      and+ contents =
        Expander.expand_pform_gen ~mode:Single expander contents_sw
        >>| Value.to_string ~dir
      in
      Action.Write_file (path, perm, contents)
    | _ ->
      Code_error.raise
        "Pkg_rules.action_expander.expand: unsupported action"
        [ "action", Dune_lang.Action.to_dyn action ]

  and expand_withenv (expander : Expander.t) updates action =
    let* env, updates =
      let dir = Path.build expander.paths.source_dir in
      Memo.List.fold_left
        ~init:(expander.env, [])
        updates
        ~f:(fun (env, updates) ({ Env_update.op = _; var; value } as update) ->
          let+ value =
            let+ value =
              let expander = { expander with env } in
              Expander.expand_pform_gen expander value ~mode:Single
            in
            Value.to_string ~dir value
          in
          let env = Env_update.set env { update with value } in
          let update =
            let value =
              match Env.Map.find env var with
              | Some v -> Value_list_env.string_of_env_values v
              | None ->
                (* TODO *)
                ""
            in
            var, value
          in
          env, update :: updates)
    in
    let+ action =
      let expander = { expander with env } in
      expand action ~expander
    in
    List.fold_left updates ~init:action ~f:(fun action (k, v) ->
      Action.Setenv (k, v, action))
  ;;

  module Artifacts_and_deps = struct
    type artifacts_and_deps =
      { binaries : Path.t Filename.Map.t
      ; dep_info :
          (OpamVariable.variable_contents Package_variable_name.Map.t * Paths.t)
            Package.Name.Map.t
      }

    let empty = { binaries = Filename.Map.empty; dep_info = Package.Name.Map.empty }

    let of_closure closure =
      Memo.parallel_map closure ~f:(fun (pkg : Pkg.t) ->
        let cookie = (Pkg_installed.of_paths pkg.paths).cookie in
        Action_builder.evaluate_and_collect_facts cookie
        |> Memo.map ~f:(fun ((cookie : Install_cookie.t), _) -> pkg, cookie))
      |> Memo.map ~f:(fun cookies ->
        List.fold_left
          cookies
          ~init:empty
          ~f:(fun { binaries; dep_info } ((pkg : Pkg.t), (cookie : Install_cookie.t)) ->
            let binaries =
              Section.Map.Multi.find cookie.files Bin
              |> List.fold_left ~init:binaries ~f:(fun acc bin ->
                Filename.Map.set acc (Path.basename bin) bin)
            in
            let dep_info =
              let variables =
                Package_variable_name.Map.superpose
                  (Package_variable_name.Map.of_list_exn cookie.variables)
                  (Pkg_info.variables pkg.info)
              in
              Package.Name.Map.add_exn dep_info pkg.info.name (variables, pkg.paths)
            in
            { binaries; dep_info }))
    ;;
  end

  let expander context (pkg : Pkg.t) =
    let+ { Artifacts_and_deps.binaries; dep_info } =
      Pkg.deps_closure pkg |> Artifacts_and_deps.of_closure
    in
    let env = Pkg.exported_value_env pkg in
    let depends =
      Package.Name.Map.add_exn
        dep_info
        pkg.info.name
        (Pkg_info.variables pkg.info, pkg.paths)
    in
    { Expander.paths = pkg.paths
    ; artifacts = binaries
    ; context
    ; depends
    ; version = pkg.info.version
    ; env
    }
  ;;

  let sandbox = Sandbox_mode.Set.singleton Sandbox_mode.copy

  let expand context (pkg : Pkg.t) action =
    let+ action =
      let* expander = expander context pkg in
      expand action ~expander >>| Action.chdir (Path.build pkg.paths.source_dir)
    in
    (* TODO copying is needed for build systems that aren't dune and those
       with an explicit install step *)
    Action.Full.make ~sandbox action
    |> Action_builder.return
    |> Action_builder.with_no_targets
  ;;

  let dune_exe context =
    Which.which ~path:(Env_path.path Env.initial) "dune"
    >>| function
    | Some s -> Ok s
    | None -> Error (Action.Prog.Not_found.create ~loc:None ~context ~program:"dune" ())
  ;;

  let build_command context (pkg : Pkg.t) =
    Option.map pkg.build_command ~f:(function
      | Action action -> expand context pkg action
      | Dune ->
        (* CR-rgrinberg: respect [dune subst] settings. *)
        Command.run_dyn_prog
          (Action_builder.of_memo (dune_exe context))
          ~dir:(Path.build pkg.paths.source_dir)
          [ A "build"; A "-p"; A (Package.Name.to_string pkg.info.name) ]
        |> Memo.return)
  ;;

  let install_command context (pkg : Pkg.t) =
    Option.map pkg.install_command ~f:(fun action -> expand context pkg action)
  ;;

  let exported_env (expander : Expander.t) (env : _ Env_update.t) =
    let+ value =
      let+ value = Expander.expand_pform_gen expander env.value ~mode:Single in
      value |> Value.to_string ~dir:(Path.build expander.paths.source_dir)
    in
    { env with value }
  ;;
end

module DB = struct
  type t =
    { all : Lock_dir.Pkg.t Package.Name.Map.t
    ; system_provided : Package.Name.Set.t
    }

  let equal t { all; system_provided } =
    Package.Name.Map.equal ~equal:Lock_dir.Pkg.equal t.all all
    && Package.Name.Set.equal t.system_provided system_provided
  ;;

  let get =
    let dune = Package.Name.Set.singleton (Package.Name.of_string "dune") in
    fun context ->
      let+ all = Lock_dir.get context in
      { all = all.packages; system_provided = dune }
  ;;
end

module rec Resolve : sig
  val resolve
    :  DB.t
    -> Context_name.t
    -> Loc.t * Package.Name.t
    -> [ `Inside_lock_dir of Pkg.t | `System_provided ] Memo.t
end = struct
  open Resolve

  let resolve_impl ((db : DB.t), ctx, (name : Package.Name.t)) =
    match Package.Name.Map.find db.all name with
    | None -> Memo.return None
    | Some { Lock_dir.Pkg.build_command; install_command; depends; info; exported_env } ->
      assert (Package.Name.equal name info.name);
      let* depends =
        Memo.parallel_map depends ~f:(fun name ->
          resolve db ctx name
          >>| function
          | `Inside_lock_dir pkg -> Some pkg
          | `System_provided -> None)
        >>| List.filter_opt
      and+ files_dir =
        let+ lock_dir = Lock_dir.get_path ctx >>| Option.value_exn in
        Path.Build.append_source
          (Context_name.build_dir ctx)
          (Dune_pkg.Lock_dir.Pkg.files_dir info.name ~lock_dir)
      in
      let id = Pkg.Id.gen () in
      let paths = Paths.make name ctx in
      let t =
        { Pkg.id
        ; build_command
        ; install_command
        ; depends
        ; paths
        ; info
        ; files_dir
        ; exported_env = []
        }
      in
      let+ exported_env =
        let* expander = Action_expander.expander ctx t in
        Memo.parallel_map exported_env ~f:(Action_expander.exported_env expander)
      in
      t.exported_env <- exported_env;
      Some t
  ;;

  let resolve =
    let module Input = struct
      type t = DB.t * Context_name.t * Package.Name.t

      let equal = Tuple.T3.equal DB.equal Context_name.equal Package.Name.equal
      let hash = Tuple.T3.hash Poly.hash Context_name.hash Package.Name.hash
      let to_dyn = Dyn.opaque
    end
    in
    let memo =
      Memo.create
        "pkg-resolve"
        ~input:(module Input)
        ~human_readable_description:(fun (_db, _ctx, pkg) ->
          Pp.textf "- package %s" (Package.Name.to_string pkg))
        resolve_impl
    in
    fun (db : DB.t) ctx (loc, name) ->
      if Package.Name.Set.mem db.system_provided name
      then Memo.return `System_provided
      else
        Memo.exec memo (db, ctx, name)
        >>| function
        | Some s -> `Inside_lock_dir s
        | None ->
          User_error.raise
            ~loc
            [ Pp.textf "Unknown package %S" (Package.Name.to_string name) ]
  ;;
end

module Install_action = struct
  let installable_sections =
    Section.(Set.diff all (Set.of_list [ Misc; Libexec; Libexec_root ]))
    |> Section.Set.to_list
  ;;

  module Spec = struct
    type ('path, 'target) t =
      { install_file : 'path
      ; config_file : 'path
      ; target_dir : 'target
      ; install_action : [ `Has_install_action | `No_install_action ]
      ; package : Package.Name.t
      }

    let name = "install-file-run"
    let version = 1

    let bimap
      ({ install_file; config_file; target_dir; install_action = _; package = _ } as t)
      f
      g
      =
      { t with
        install_file = f install_file
      ; config_file = f config_file
      ; target_dir = g target_dir
      }
    ;;

    let is_useful_to ~memoize = memoize

    let encode
      { install_file; config_file; target_dir; install_action; package }
      path
      target
      : Dune_lang.t
      =
      List
        [ Dune_lang.atom_or_quoted_string name
        ; path install_file
        ; path config_file
        ; target target_dir
        ; Dune_lang.atom_or_quoted_string (Package.Name.to_string package)
        ; Dune_lang.atom
            (match install_action with
             | `Has_install_action -> "has_install_action"
             | `No_install_action -> "no_install_action")
        ]
    ;;

    let prepare_copy ~install_file ~target_dir entry =
      let dst =
        let paths =
          let package =
            Path.basename install_file
            |> Filename.remove_extension
            |> Package.Name.of_string
          in
          let roots =
            Path.build target_dir
            |> Install.Roots.opam_from_prefix ~relative:Path.relative
          in
          Install.Paths.make ~relative:Path.relative ~package ~roots
        in
        Install.Entry.relative_installed_path entry ~paths
      in
      Path.mkdir_p (Path.parent_exn dst);
      dst
    ;;

    let readdir path =
      match Path.Untracked.readdir_unsorted_with_kinds path with
      | Error _ -> [], []
      | Ok listing ->
        List.partition_map listing ~f:(fun (basename, kind) ->
          let path = Path.relative path basename in
          match kind with
          | S_DIR -> Right path
          | _ -> Left path)
    ;;

    let rec collect paths acc =
      match paths with
      | [] -> acc
      | path :: paths ->
        let files, dirs = readdir path in
        let acc = List.rev_append files acc in
        collect (List.rev_append dirs paths) acc
    ;;

    let skip path skip =
      List.iter skip ~f:(fun s -> assert (Path.equal path (Path.parent_exn s)));
      let files, dirs = readdir path in
      let dirs =
        List.filter_map dirs ~f:(fun path ->
          if List.mem skip path ~equal:Path.equal then None else Some path)
      in
      files, dirs
    ;;

    let maybe_drop_sandbox_dir path =
      match Path.extract_build_context_dir_maybe_sandboxed path with
      | None -> path
      | Some (sandbox, source) ->
        let ctx =
          let name = Path.basename sandbox in
          Path.relative (Path.build Path.Build.root) name
        in
        Path.append_source ctx source
    ;;

    let section_map_of_dir install_paths =
      let get = Install.Paths.get install_paths in
      List.concat_map installable_sections ~f:(fun section ->
        let path = get section in
        let acc, dirs =
          match section with
          | Lib_root -> skip path [ get Toplevel; get Stublibs; get Lib ]
          | Share_root -> skip path [ get Share ]
          | _ -> [], [ path ]
        in
        collect dirs acc
        |> List.rev_map ~f:(fun file ->
          let section =
            match
              match section with
              | Lib_root -> Some Section.Libexec_root
              | Lib -> Some Libexec
              | _ -> None
            with
            | None -> section
            | Some section' ->
              let perm = (Path.Untracked.stat_exn file).st_perm in
              if Path.Permissions.(test execute perm) then section' else section
          in
          section, maybe_drop_sandbox_dir file))
      |> Section.Map.of_list_multi
    ;;

    let maybe_set_executable section dst =
      match Section.should_set_executable_bit section with
      | false -> ()
      | true ->
        let permission =
          let perm = (Path.Untracked.stat_exn dst).st_perm in
          Path.Permissions.(add execute) perm
        in
        Path.chmod dst ~mode:permission
    ;;

    let read_variables config_file =
      match Path.Untracked.exists config_file with
      | false -> []
      | true ->
        let config =
          let filename = Path.to_string config_file in
          match
            Io.read_file config_file
            |> OpamFile.Dot_config.read_from_string
                 ~filename:(OpamFile.make (OpamFilename.of_string filename))
          with
          | s -> s
          | exception OpamPp.Bad_format (pos, message) ->
            let loc =
              Option.map
                pos
                ~f:(fun { OpamParserTypes.FullPos.filename = _; start; stop } ->
                  let file_contents = Io.read_file config_file in
                  let bols = ref [ 0 ] in
                  String.iteri file_contents ~f:(fun i ch ->
                    if ch = '\n' then bols := (i + 1) :: !bols);
                  let bols = Array.of_list (List.rev !bols) in
                  let make_pos (line, column) =
                    let pos_bol = bols.(line - 1) in
                    { Lexing.pos_fname = filename
                    ; pos_lnum = line
                    ; pos_bol
                    ; pos_cnum = pos_bol + column
                    }
                  in
                  let start = make_pos start in
                  let stop = make_pos stop in
                  Loc.create ~start ~stop)
            in
            let message_with_loc =
              (* The location is inlined b/c the original config file is going
                 to be deleted, so we don't be able to fetch the part of the
                 file that's bad *)
              let open Pp.O in
              let error = Pp.textf "Error parsing %s" (Path.basename config_file) in
              match loc with
              | None -> error
              | Some loc ->
                (Loc.pp loc |> Pp.map_tags ~f:(fun Loc.Loc -> User_message.Style.Loc))
                ++ error
            in
            User_error.raise
              [ message_with_loc; Pp.seq (Pp.text "Reason: ") (Pp.text message) ]
        in
        OpamFile.Dot_config.bindings config
        |> List.map ~f:(fun (name, value) -> Package_variable_name.of_opam name, value)
    ;;

    let install_entry ~src ~install_file ~target_dir (entry : Path.t Install.Entry.t) =
      match Path.Untracked.exists src, entry.optional with
      | false, true -> None
      | false, false ->
        User_error.raise
          (* TODO loc *)
          [ Pp.textf
              "entry %s in %s does not exist"
              (Path.to_string_maybe_quoted src)
              (Path.to_string install_file)
          ]
      | true, _ ->
        let dst = prepare_copy ~install_file ~target_dir entry in
        (let src =
           match Path.to_string src |> Unix.readlink with
           | exception Unix.Unix_error (_, _, _) -> src
           | link ->
             Path.external_
               (let base = Path.parent_exn src in
                Filename.concat (Path.to_absolute_filename base) link
                |> Path.External.of_string)
         in
         Io.portable_hardlink ~src ~dst);
        maybe_set_executable entry.section dst;
        Some (entry.section, dst)
    ;;

    let action
      { package; install_file; config_file; target_dir; install_action }
      ~ectx:_
      ~eenv:_
      =
      let open Fiber.O in
      let* () = Fiber.return () in
      let* files =
        let from_install_action =
          match install_action with
          | `No_install_action -> Section.Map.empty
          | `Has_install_action ->
            let install_paths =
              Paths.of_root package ~root:(Path.Build.parent_exn target_dir)
              |> Paths.install_paths
              |> Install.Paths.map ~f:Path.build
            in
            section_map_of_dir install_paths
        in
        let+ from_install_file =
          Async.async (fun () -> Path.Untracked.exists install_file)
          >>= function
          | false -> Fiber.return Section.Map.empty
          | true ->
            let* map =
              let install_entries =
                let dir = Path.parent_exn install_file in
                Install.Entry.load_install_file install_file (fun local ->
                  Path.append_local dir local)
              in
              let by_src =
                List.rev_map install_entries ~f:(fun (entry : _ Install.Entry.t) ->
                  entry.src, entry)
                |> Path.Map.of_list_multi
              in
              let+ install_entries =
                Path.Map.to_list_map by_src ~f:(fun src entries ->
                  List.map entries ~f:(fun entry -> src, entry))
                |> List.concat
                |> Fiber.parallel_map ~f:(fun (src, entry) ->
                  Async.async (fun () ->
                    install_entry ~src ~install_file ~target_dir entry))
                >>| List.filter_opt
              in
              List.rev_map install_entries ~f:(fun (section, file) ->
                let file = maybe_drop_sandbox_dir file in
                section, file)
              |> Section.Map.of_list_multi
            in
            let+ () = Async.async (fun () -> Path.unlink_exn install_file) in
            map
        in
        (* TODO we should make sure that overwrites aren't allowed *)
        Section.Map.union from_install_action from_install_file ~f:(fun _ x y ->
          Some (x @ y))
      in
      let* cookies =
        let+ variables = Async.async (fun () -> read_variables config_file) in
        { Install_cookie.files; variables }
      in
      let cookie_file = Path.build @@ Paths.install_cookie' target_dir in
      Async.async (fun () ->
        cookie_file |> Path.parent_exn |> Path.mkdir_p;
        Install_cookie.dump cookie_file cookies)
    ;;
  end

  let action (p : Paths.t) install_action =
    let module M = struct
      type path = Path.t
      type target = Path.Build.t

      module Spec = Spec

      let v =
        { Spec.install_file = Path.build @@ Paths.install_file p
        ; config_file = Path.build @@ Paths.config_file p
        ; target_dir = p.target_dir
        ; install_action
        ; package = p.name
        }
      ;;
    end
    in
    Action.Extension (module M)
  ;;
end

let add_env env action =
  Action_builder.With_targets.map action ~f:(Action.Full.add_env env)
;;

let rule ?loc { Action_builder.With_targets.build; targets } =
  (* TODO this ignores the workspace file *)
  Rule.make ~info:(Rule.Info.of_loc_opt loc) ~targets build |> Rules.Produce.rule
;;

let source_rules (pkg : Pkg.t) =
  let+ source_deps, copy_rules =
    match pkg.info.source with
    | None -> Memo.return (Dep.Set.empty, [])
    | Some source ->
      let loc = fst source.url in
      Lock_dir.source_kind source
      >>= (function
       | `Local (`File, _) | `Fetch ->
         let fetch = Fetch_rules.fetch ~target:pkg.paths.source_dir `Directory source in
         Memo.return (Dep.Set.of_files [ Path.build pkg.paths.source_dir ], [ loc, fetch ])
       | `Local (`Directory, source_root) ->
         let+ source_files, rules =
           let source_root = Path.external_ source_root in
           Pkg.source_files pkg ~loc
           >>| Path.Local.Set.fold ~init:([], []) ~f:(fun file (source_files, rules) ->
             let src = Path.append_local source_root file in
             let dst = Path.Build.append_local pkg.paths.source_dir file in
             let copy = loc, Action_builder.copy ~src ~dst in
             Path.build dst :: source_files, copy :: rules)
         in
         Dep.Set.of_files source_files, rules)
  in
  let extra_source_deps, extra_copy_rules =
    List.map pkg.info.extra_sources ~f:(fun (local, (fetch : Source.t)) ->
      let extra_source = Paths.extra_source pkg.paths local in
      let rule =
        let loc = fst fetch.url in
        (* We assume that [fetch] is always a file. Would be good
           to give a decent error message if it's not *)
        match Source.kind fetch with
        | `Directory_or_archive src ->
          loc, Action_builder.copy ~src:(Path.external_ src) ~dst:extra_source
        | `Fetch ->
          let rule = Fetch_rules.fetch ~target:pkg.paths.source_dir `File fetch in
          loc, rule
      in
      Path.build extra_source, rule)
    |> List.unzip
  in
  let copy_rules = copy_rules @ extra_copy_rules in
  let source_deps = Dep.Set.union source_deps (Dep.Set.of_files extra_source_deps) in
  source_deps, Memo.parallel_iter copy_rules ~f:(fun (loc, copy) -> rule ~loc copy)
;;

let build_rule context_name ~source_deps (pkg : Pkg.t) =
  let+ build_action =
    let+ build_and_install =
      let+ copy_action =
        let+ copy_action =
          Fs_memo.dir_exists
            (In_source_dir (Path.Build.drop_build_context_exn pkg.files_dir))
          >>= function
          | false -> Memo.return []
          | true ->
            let+ deps, source_deps = Source_deps.files (Path.build pkg.files_dir) in
            let open Action_builder.O in
            [ Action_builder.with_no_targets
              @@ (Action_builder.deps deps
                  >>> (Path.Set.to_list_map source_deps ~f:(fun src ->
                         let dst =
                           let local_path =
                             Path.drop_prefix_exn src ~prefix:(Path.build pkg.files_dir)
                           in
                           Path.Build.append_local pkg.paths.source_dir local_path
                         in
                         Action.progn
                           [ Action.mkdir (Path.Build.parent_exn dst)
                           ; Action.copy src dst
                           ])
                       |> Action.concurrent
                       |> Action.Full.make
                       |> Action_builder.return))
            ]
        in
        copy_action
        @ List.map pkg.info.extra_sources ~f:(fun (local, _) ->
          let src = Path.build (Paths.extra_source pkg.paths local) in
          let dst = Path.Build.append_local pkg.paths.source_dir local in
          Action.copy src dst |> Action.Full.make |> Action_builder.With_targets.return)
      and+ build_action =
        match Action_expander.build_command context_name pkg with
        | None -> Memo.return []
        | Some build_command -> build_command >>| List.singleton
      and+ install_action =
        match Action_expander.install_command context_name pkg with
        | None -> Memo.return []
        | Some install_action ->
          let+ install_action = install_action in
          let mkdir_install_dirs =
            let install_paths = Paths.install_paths pkg.paths in
            Install_action.installable_sections
            |> List.rev_map ~f:(fun section ->
              Install.Paths.get install_paths section |> Action.mkdir)
            |> Action.progn
            |> Action.Full.make
            |> Action_builder.With_targets.return
          in
          [ mkdir_install_dirs; install_action ]
      in
      List.concat [ copy_action; build_action; install_action ]
    in
    let install_file_action =
      Install_action.action
        pkg.paths
        (match Action_expander.install_command context_name pkg with
         | None -> `No_install_action
         | Some _ -> `Has_install_action)
      |> Action.Full.make
      |> Action_builder.return
      |> Action_builder.with_no_targets
    in
    Action_builder.progn (build_and_install @ [ install_file_action ])
  in
  let deps = Dep.Set.union source_deps (Pkg.package_deps pkg) in
  let open Action_builder.With_targets.O in
  Action_builder.deps deps
  |> Action_builder.with_no_targets
  (* TODO should we add env deps on these? *)
  >>> add_env (Pkg.exported_env pkg) build_action
  |> Action_builder.With_targets.add_directories
       ~directory_targets:[ pkg.paths.target_dir ]
;;

let gen_rules context_name (pkg : Pkg.t) =
  let* source_deps, copy_rules = source_rules pkg in
  let* () = copy_rules
  and* build_rule = build_rule context_name pkg ~source_deps in
  rule ~loc:Loc.none (* TODO *) build_rule
;;

module Gen_rules = Build_config.Gen_rules

let setup_package_rules context ~dir ~pkg_name : Gen_rules.result Memo.t =
  let name = User_error.ok_exn (Package.Name.of_string_user_error (Loc.none, pkg_name)) in
  let* pkg =
    let* db = DB.get context in
    Resolve.resolve db context (Loc.none, name)
    >>| function
    | `Inside_lock_dir pkg -> pkg
    | `System_provided ->
      User_error.raise
        (* TODO loc *)
        [ Pp.textf
            "There are no rules for %S because it's set as provided by the system"
            (Package.Name.to_string name)
        ]
  in
  let paths = Paths.make name context in
  let+ directory_targets =
    let map =
      let target_dir = paths.target_dir in
      Path.Build.Map.singleton target_dir Loc.none
    in
    match pkg.info.source with
    | None -> Memo.return map
    | Some source ->
      Lock_dir.source_kind source
      >>| (function
       | `Local (`Directory, _) -> map
       | `Local (`File, _) | `Fetch ->
         Path.Build.Map.add_exn map paths.source_dir (fst source.url))
  in
  let build_dir_only_sub_dirs =
    Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.empty
  in
  let rules = Rules.collect_unit (fun () -> gen_rules context pkg) in
  Gen_rules.make ~directory_targets ~build_dir_only_sub_dirs rules
;;

let setup_rules ~components ~dir ctx =
  match components with
  | [ ".pkg" ] ->
    Gen_rules.make
      ~build_dir_only_sub_dirs:
        (Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
      (Memo.return Rules.empty)
    |> Memo.return
  | [ ".pkg"; pkg_name ] -> setup_package_rules ctx ~dir ~pkg_name
  | ".pkg" :: _ :: _ -> Memo.return @@ Gen_rules.redirect_to_parent Gen_rules.Rules.empty
  | [] ->
    let build_dir_only_sub_dirs =
      Gen_rules.Build_only_sub_dirs.singleton ~dir @@ Subdir_set.of_list [ ".pkg" ]
    in
    Memo.return @@ Gen_rules.make ~build_dir_only_sub_dirs (Memo.return Rules.empty)
  | _ -> Memo.return @@ Gen_rules.rules_here Gen_rules.Rules.empty
;;

let ocaml_toolchain context =
  (let* lock_dir = Lock_dir.get context in
   let* db = DB.get context in
   match lock_dir.ocaml with
   | None -> Memo.return `System_provided
   | Some ocaml -> Resolve.resolve db context ocaml)
  >>| function
  | `System_provided -> None
  | `Inside_lock_dir pkg ->
    let toolchain =
      let cookie = (Pkg_installed.of_paths pkg.paths).cookie in
      let open Action_builder.O in
      let* cookie = cookie in
      (* TODO we should use the closure of [pkg] *)
      let binaries =
        Section.Map.find cookie.files Bin |> Option.value ~default:[] |> Path.Set.of_list
      in
      let env = Env.extend_env (Global.env ()) (Pkg.exported_env pkg) in
      let path = Env_path.path (Global.env ()) in
      Action_builder.of_memo @@ Ocaml_toolchain.of_binaries ~path context env binaries
    in
    Some (Action_builder.memoize "ocaml_toolchain" toolchain)
;;

let all_packages context =
  let* db = DB.get context in
  Dune_lang.Package_name.Map.values db.all
  |> Memo.parallel_map ~f:(fun (package : Lock_dir.Pkg.t) ->
    let package = package.info.name in
    Resolve.resolve db context (Loc.none, package)
    >>| function
    | `Inside_lock_dir pkg -> Some pkg
    | `System_provided -> None)
  >>| List.filter_opt
  >>| Pkg.top_closure
;;

let which context =
  let artifacts_and_deps =
    Memo.lazy_ (fun () ->
      let+ { binaries; dep_info = _ } =
        all_packages context >>= Action_expander.Artifacts_and_deps.of_closure
      in
      binaries)
  in
  Staged.stage (fun program ->
    let+ artifacts = Memo.Lazy.force artifacts_and_deps in
    Filename.Map.find artifacts program)
;;

let ocamlpath context =
  let+ all_packages = all_packages context in
  let env = Pkg.build_env_of_deps all_packages in
  Env.Map.find env Dune_findlib.Config.ocamlpath_var
  |> Option.value ~default:[]
  |> List.map ~f:(function
    | Value.Dir p | Path p -> p
    | String s -> Path.of_filename_relative_to_initial_cwd s)
;;

let lock_dir_active = Lock_dir.lock_dir_active
let lock_dir_path = Lock_dir.get_path

let exported_env context =
  let+ all_packages = all_packages context in
  let env = Pkg.build_env_of_deps all_packages in
  let vars = Env.Map.map env ~f:Value_list_env.string_of_env_values in
  Env.extend Env.empty ~vars
;;

let find_package ctx pkg =
  lock_dir_active ctx
  >>= function
  | false -> Memo.return None
  | true ->
    let* db = DB.get ctx in
    Resolve.resolve db ctx (Loc.none, pkg)
    >>| (function
           | `System_provided -> Action_builder.return ()
           | `Inside_lock_dir pkg ->
             let open Action_builder.O in
             let+ _cookie = (Pkg_installed.of_paths pkg.paths).cookie in
             ())
    >>| Option.some
;;
