open Import
open Memo.O
open Dune_pkg

module Sys_vars = struct
  type t =
    { os : string option Memo.Lazy.t
    ; os_version : string option Memo.Lazy.t
    ; os_distribution : string option Memo.Lazy.t
    ; os_family : string option Memo.Lazy.t
    ; arch : string option Memo.Lazy.t
    }

  let poll =
    let path = Env_path.path Stdune.Env.initial in
    let sys_poll_memo key =
      Memo.lazy_ (fun () -> Memo.of_reproducible_fiber @@ key ~path)
    in
    { os = sys_poll_memo Sys_poll.os
    ; os_version = sys_poll_memo Sys_poll.os_version
    ; os_distribution = sys_poll_memo Sys_poll.os_distribution
    ; os_family = sys_poll_memo Sys_poll.os_family
    ; arch = sys_poll_memo Sys_poll.arch
    }
  ;;
end

module Variable = struct
  type value = OpamVariable.variable_contents =
    | B of bool
    | S of string
    | L of string list

  type t = Package_variable.Name.t * value

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

  let to_dyn (name, value) =
    Dyn.(pair Package_variable.Name.to_dyn dyn_of_value (name, value))
  ;;
end

module Pkg_info = struct
  include Dune_pkg.Lock_dir.Pkg_info

  let variables t =
    Package_variable.Name.Map.of_list_map_exn
      [ "name", Variable.S (Package.Name.to_string t.name)
      ; "version", S (Package_version.to_string t.version)
      ; "dev", B t.dev
      ]
      ~f:(fun (name, value) -> Package_variable.Name.of_string name, value)
  ;;
end

module Lock_dir = struct
  include Dune_pkg.Lock_dir

  module Load = Make_load (struct
      include Memo

      let readdir_with_kinds path =
        Fs_memo.dir_contents (In_source_dir path)
        >>| function
        | Error _ ->
          (* TODO *)
          User_error.raise [ Pp.text "" ]
        | Ok content -> Fs_cache.Dir_contents.to_list content
      ;;

      let with_lexbuf_from_file path ~f =
        Fs_memo.with_lexbuf_from_file (In_source_dir path) ~f
      ;;

      let stats_kind p =
        Fs_memo.path_stat (In_source_dir p)
        >>| Stdune.Result.map ~f:(fun { Fs_cache.Reduced_stats.st_kind; _ } -> st_kind)
      ;;
    end)

  let get_path ctx =
    let+ workspace = Workspace.workspace () in
    match
      List.find_map workspace.contexts ~f:(fun ctx' ->
        match Context_name.equal (Workspace.Context.name ctx') ctx with
        | false -> None
        | true -> Some ctx')
    with
    | None -> Some default_path
    | Some (Default { lock; _ }) -> Some (Option.value lock ~default:default_path)
    | Some (Opam _) -> None
  ;;

  let get (ctx : Context_name.t) : t Memo.t =
    get_path ctx >>| Option.value_exn >>= Load.load
  ;;

  let lock_dir_active ctx =
    if !Clflags.ignore_lock_directory
    then Memo.return false
    else
      get_path ctx
      >>= function
      | None -> Memo.return false
      | Some path -> Fs_memo.dir_exists (In_source_dir path)
  ;;
end

module Paths = struct
  type t =
    { source_dir : Path.Build.t
    ; target_dir : Path.Build.t
    ; extra_sources : Path.Build.t
    ; name : Package.Name.t
    ; install_roots : Path.t Install.Roots.t Lazy.t
    ; install_paths : Install.Paths.t Lazy.t
    }

  let install_roots ~target_dir = Path.build target_dir |> Install.Roots.opam_from_prefix
  let install_paths roots package = Install.Paths.make ~package ~roots

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

module Env_update = struct
  include Dune_lang.Action.Env_update

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

  let string_of_env_values values =
    List.map values ~f:(function
      | Value.String s -> s
      | Dir s | Path s -> Path.to_absolute_filename s)
    |> Bin.encode_strings
  ;;
end

module Pkg = struct
  module Id = Id.Make ()

  type t =
    { id : Id.t
    ; build_command : Dune_lang.Action.t option
    ; install_command : Dune_lang.Action.t option
    ; deps : t list
    ; info : Pkg_info.t
    ; paths : Paths.t
    ; files_dir : Path.Build.t
    ; mutable exported_env : string Env_update.t list
    }

  module Top_closure = Top_closure.Make (Id.Set) (Monad.Id)

  let top_closure deps =
    Top_closure.top_closure deps ~key:(fun t -> t.id) ~deps:(fun t -> t.deps)
  ;;

  let deps_closure t =
    match top_closure t.deps with
    | Ok s -> s
    | Error _ -> assert false
  ;;

  let source_files t ~loc =
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
          List.partition_map contents ~f:(fun (name, kind) ->
            (* TODO handle links and cycles correctly *)
            match kind with
            | S_DIR -> Right name
            | _ -> Left name)
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
    match t.info.source with
    | None -> Memo.return Path.Local.Set.empty
    | Some (External_copy (_, root)) -> loop root Path.Local.Set.empty Path.Local.root
    | Some (Fetch _) -> assert false
  ;;

  let dep t = Dep.file (Path.build t.paths.target_dir)

  let package_deps t =
    deps_closure t
    |> List.fold_left ~init:Dep.Set.empty ~f:(fun acc t -> dep t |> Dep.Set.add acc)
  ;;

  let build_env_of_deps =
    let add_to_path env var what =
      Env.Map.update env var ~f:(fun paths ->
        let paths = Option.value paths ~default:[] in
        Some (Value.Dir (Path.build what) :: paths))
    in
    fun xs ->
      List.fold_left xs ~init:Env.Map.empty ~f:(fun env t ->
        let env =
          let roots =
            Paths.install_roots t.paths |> Install.Roots.map ~f:Path.as_in_build_dir_exn
          in
          let init = add_to_path env Env_path.var roots.bin in
          let vars = Install.Roots.to_env_without_path roots in
          List.fold_left vars ~init ~f:(fun acc (var, path) -> add_to_path acc var path)
        in
        List.fold_left t.exported_env ~init:env ~f:Env_update.set)
  ;;

  let build_env t = build_env_of_deps @@ deps_closure t

  let exported_env t =
    let base =
      Env.Map.of_list_exn
        [ Opam_switch.opam_switch_prefix_var_name, Path.Build.to_string t.paths.target_dir
        ; "CDPATH", ""
        ; "MAKELEVEL", ""
        ; "OPAM_PACKAGE_NAME", Package.Name.to_string t.info.name
        ; "OPAM_PACKAGE_VERSION", Package_version.to_string t.info.version
        ; "OPAMCLI", "2.0"
        ]
    in
    let package_env =
      let vars =
        build_env t
        |> Env.Map.map ~f:Env_update.string_of_env_values
        |> Env.Map.superpose base
      in
      Env.extend Env.empty ~vars
    in
    (* TODO: Run actions in a constrained environment. [Env.initial] is the
       environment from which dune was executed, and some of the environment
       variables may affect builds in unintended ways and make builds less
       reproducible. However other environment variables must be set in order
       for build actions to run successfully, such as $PATH on systems where the
       shell's default $PATH variable doesn't include the location of standard
       programs or build tools (e.g. NixOS). *)
    Env_path.extend_env_concat_path Env.initial package_env
  ;;
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

module Substitute = struct
  include Substs.Make (struct
      type 'a t = 'a

      module O = struct
        let ( let+ ) x f = f x
      end

      module List = struct
        let map t ~f = List.map t ~f
      end
    end)

  module Spec = struct
    type ('src, 'dst) t =
      { (* XXX it's not good to serialize the substitution map like this. We're
           essentially implementing the same substitution procedure but in two
           different places: action geeneration, and action execution.

           The two implementations are bound to drift. Better would be to
           reconstruct everything that is needed to call our one and only
           substitution function. *)
        vars : OpamVariable.variable_contents Package_variable.Map.t
      ; package : Package.Name.t
      ; src : 'src
      ; dst : 'dst
      }

    let name = "substitute"
    let version = 1
    let bimap t f g = { t with src = f t.src; dst = g t.dst }
    let is_useful_to ~memoize = memoize

    let encode { vars; package; src; dst } input output : Dune_lang.t =
      let e =
        Package_variable.Map.to_list_map
          vars
          ~f:(fun { Package_variable.scope; name } v ->
            let k =
              let package =
                match scope with
                | Self -> Dune_sexp.List []
                | Package p -> Dune_lang.Package_name.encode p
              in
              Dune_sexp.List [ package; Package_variable.Name.encode name ]
            in
            let v =
              Dune_lang.atom_or_quoted_string (OpamVariable.string_of_variable_contents v)
            in
            Dune_sexp.List [ k; v ])
      in
      let s = Dune_lang.Package_name.encode package in
      List [ Dune_lang.atom_or_quoted_string name; List e; s; input src; output dst ]
    ;;

    let action { vars; package; src; dst } ~ectx:_ ~eenv:_ =
      let open Fiber.O in
      let+ () = Fiber.return () in
      let env (var : Package_variable.t) =
        match Package_variable.Map.find vars var with
        | Some _ as v -> v
        | None ->
          Package_variable.Map.find
            vars
            { var with Package_variable.scope = Package package }
      in
      subst env package ~src ~dst
    ;;
  end

  let action package ~vars ~src ~dst =
    let module M = struct
      type path = Path.t
      type target = Path.Build.t

      module Spec = Spec

      let v = { Spec.vars; package; src; dst }
    end
    in
    Action.Extension (module M)
  ;;
end

module Action_expander = struct
  module Expander = struct
    type t =
      { paths : Paths.t
      ; artifacts : Path.t Filename.Map.t
      ; deps : (Variable.value Package_variable.Name.Map.t * Paths.t) Package.Name.Map.t
      ; context : Context_name.t
      ; version : Package_version.t
      ; env : Value.t list Env.Map.t
      }

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
      | Toplevel -> Path.relative roots.lib_root "toplevel"
      | Stublibs -> Path.relative roots.lib_root "stublibs"
    ;;

    let sys_poll_var accessor =
      accessor Sys_vars.poll
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
        Memo.return [ Value.Dir dir ]
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
        | None -> Package_variable.Name.Map.empty, None
        | Some (var, paths) -> var, Some paths
      in
      match Package_variable.Name.Map.find variables variable_name with
      | Some v -> Memo.return @@ Ok (Variable.dune_value v)
      | None ->
        let present = Option.is_some paths in
        (* TODO we should be looking it up in all packages now *)
        (match Package_variable.Name.to_string variable_name with
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
                   (Package_variable.Name.to_string variable_name)
               with
               | None -> Memo.return (Error (`Undefined_pkg_var variable_name))
               | Some section ->
                 let section = dune_section_of_pform section in
                 let install_paths = Paths.install_paths paths in
                 Memo.return @@ Ok [ Value.Dir (Install.Paths.get install_paths section) ])))
    ;;

    let expand_pform
      { env = _; paths; artifacts = _; context; deps; version = _ }
      ~source
      (pform : Pform.t)
      : (Value.t list, [ `Undefined_pkg_var of Package_variable.Name.t ]) result Memo.t
      =
      let loc = Dune_sexp.Template.Pform.loc source in
      match pform with
      | Var (Pkg var) -> expand_pkg paths var >>| Result.ok
      | Var Context_name ->
        Memo.return (Ok [ Value.String (Context_name.to_string context) ])
      | Var Make ->
        let+ make =
          let path = Env_path.path Env.initial in
          Make_prog.which loc context ~path
        in
        Ok [ Value.Path make ]
      | Macro ({ macro = Pkg | Pkg_self; _ } as macro_invocation) ->
        expand_pkg_macro ~loc paths deps macro_invocation
      | _ -> Expander0.isn't_allowed_in_this_position ~source
    ;;

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
                  (Package_variable.Name.to_string variable_name)
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

    let eval_blang t blang =
      Slang_expand.eval_blang
        blang
        ~dir:(Path.build t.paths.source_dir)
        ~f:(fun sw ~dir ->
          String_expander.Memo.expand_result sw ~mode:Many ~f:(expand_pform t) ~dir)
    ;;

    let eval_slangs_located t slangs =
      Slang_expand.eval_multi_located
        slangs
        ~dir:(Path.build t.paths.source_dir)
        ~f:(fun sw ~dir ->
          String_expander.Memo.expand_result sw ~mode:Many ~f:(expand_pform t) ~dir)
    ;;
  end

  let substitute_env (expander : Expander.t) =
    let setenv package name value env =
      let var = { Package_variable.scope = Package package; name } in
      Package_variable.Map.add_exn env var value
    in
    let env =
      (* values set with withenv *)
      Env.Map.map expander.env ~f:Env_update.string_of_env_values
      |> Env.Map.to_list_map ~f:(fun variable value ->
        ( { Package_variable.scope = Self
          ; name = Package_variable.Name.of_string variable
          }
        , OpamVariable.S value ))
      |> Package_variable.Map.of_list_exn
    in
    Dune_lang.Package_name.Map.foldi
      expander.deps
      ~init:env
      ~f:(fun name (var_conts, paths) env ->
        let env =
          Package_variable.Name.Map.foldi var_conts ~init:env ~f:(fun key value env ->
            setenv name key value env)
        in
        let install_paths = Paths.install_paths paths in
        List.fold_left
          ~init:env
          ~f:(fun env (var_name, section) ->
            let key = Package_variable.Name.of_string var_name in
            let section =
              OpamVariable.S (Path.to_string (Install.Paths.get install_paths section))
            in
            setenv name key section env)
          [ "lib", Section.Lib
          ; "lib_root", Lib_root
          ; "libexec", Libexec
          ; "libexec_root", Libexec_root
          ; "bin", Bin
          ; "sbin", Sbin
          ; "toplevel", Toplevel
          ; "share", Share
          ; "share_root", Share_root
          ; "etc", Etc
          ; "doc", Doc
          ; "stublibs", Stublibs
          ; "man", Man
          ])
  ;;

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
         let+ exe = Expander.expand_exe_value expander prog ~loc:prog_loc in
         let args = Value.L.to_strings (List.map ~f:snd args) ~dir in
         Action.Run (exe, Array.Immutable.of_list args))
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
        >>| Expander0.as_in_build_dir ~what:"subsitute" ~loc:(String_with_vars.loc dst)
      in
      let vars = substitute_env expander in
      Substitute.action ~vars expander.paths.name ~src ~dst
    | Withenv (updates, action) -> expand_withenv expander updates action
    | When (condition, action) ->
      Expander.eval_blang expander condition
      >>= (function
       | true -> expand action ~expander
       | false -> Memo.return (Action.progn []))
    | _ ->
      (* TODO *)
      assert false

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
              | Some v -> Env_update.string_of_env_values v
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
          (OpamVariable.variable_contents Package_variable.Name.Map.t * Paths.t)
            Package.Name.Map.t
      }

    let empty = { binaries = Filename.Map.empty; dep_info = Package.Name.Map.empty }

    let of_closure closure =
      Memo.parallel_map closure ~f:(fun (pkg : Pkg.t) ->
        let cookie = (Pkg_installed.of_paths pkg.paths).cookie in
        Action_builder.run cookie Eager
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
                Package_variable.Name.Map.superpose
                  (Package_variable.Name.Map.of_list_exn cookie.variables)
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
    let env = Pkg.build_env pkg in
    let deps =
      Package.Name.Map.add_exn
        dep_info
        pkg.info.name
        (Pkg_info.variables pkg.info, pkg.paths)
    in
    { Expander.paths = pkg.paths
    ; artifacts = binaries
    ; context
    ; deps
    ; version = pkg.info.version
    ; env
    }
  ;;

  let expand =
    let sandbox = Sandbox_mode.Set.singleton Sandbox_mode.copy in
    fun context (pkg : Pkg.t) action ->
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

  let build_command context (pkg : Pkg.t) =
    Option.map pkg.build_command ~f:(expand context pkg)
  ;;

  let install_command context (pkg : Pkg.t) =
    Option.map pkg.install_command ~f:(expand context pkg)
  ;;

  let exported_env (expander : Expander.t) (env : _ Env_update.t) =
    let+ value =
      let+ value = Expander.expand_pform_gen expander env.value ~mode:Single in
      value |> Value.to_string ~dir:(Path.build expander.paths.source_dir)
    in
    { env with value }
  ;;
end

let ocaml_package_name = Package.Name.of_string "ocaml"

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
      let system_provided =
        let ocaml =
          match Env.mem Env.initial ~var:"DUNE_PKG_OVERRIDE_OCAML" with
          | false -> Package.Name.Set.empty
          | true ->
            Package.Name.Set.singleton
              (match all.ocaml with
               | None -> ocaml_package_name
               | Some (_, name) -> name)
        in
        Package.Name.Set.union dune ocaml
      in
      { all = all.packages; system_provided }
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
    | Some { Lock_dir.Pkg.build_command; install_command; deps; info; exported_env } ->
      assert (Package.Name.equal name info.name);
      let* deps =
        Memo.parallel_map deps ~f:(fun name ->
          resolve db ctx name
          >>| function
          | `Inside_lock_dir pkg -> Some pkg
          | `System_provided -> None)
        >>| List.filter_opt
      and+ files_dir =
        let+ lock_dir = Lock_dir.get_path ctx >>| Option.value_exn in
        Path.Build.append_source
          (Context_name.build_dir ctx)
          (Path.Source.relative
             lock_dir
             (* TODO this should come from [Dune_pkg] *)
             (sprintf "%s.files" (Package.Name.to_string info.name)))
      in
      let id = Pkg.Id.gen () in
      let paths = Paths.make name ctx in
      let t =
        { Pkg.id
        ; build_command
        ; install_command
        ; deps
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
            |> Filename.chop_extension
            |> Package.Name.of_string
          in
          let roots = Path.build target_dir |> Install.Roots.opam_from_prefix in
          Install.Paths.make ~package ~roots
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
          let config_file_str = Path.to_string config_file in
          let file = OpamFilename.of_string config_file_str |> OpamFile.make in
          match OpamFile.Dot_config.read file with
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
                    { Lexing.pos_fname = config_file_str
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
        |> List.map ~f:(fun (name, value) -> Package_variable.Name.of_opam name, value)
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
            let+ () = Async.async (fun () -> Path.unlink install_file) in
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

module Fetch = struct
  module Spec = struct
    type ('path, 'target) t =
      { target_dir : 'target
      ; url : Loc.t * string
      ; checksum : (Loc.t * Checksum.t) option
      }

    let name = "source-fetch"
    let version = 1
    let bimap t _ g = { t with target_dir = g t.target_dir }
    let is_useful_to ~memoize = memoize

    let encode_loc f (loc, x) =
      Dune_lang.List
        (* TODO use something better for locs here *)
        [ Dune_lang.atom_or_quoted_string (Loc.to_file_colon_line loc); f x ]
    ;;

    let encode { target_dir; url; checksum } _ target : Dune_lang.t =
      List
        ([ Dune_lang.atom_or_quoted_string name
         ; target target_dir
         ; encode_loc Dune_lang.atom_or_quoted_string url
         ]
         @
         match checksum with
         | None -> []
         | Some checksum ->
           [ encode_loc
               (fun x -> Checksum.to_string x |> Dune_lang.atom_or_quoted_string)
               checksum
           ])
    ;;

    let action { target_dir; url = loc_url, url; checksum } ~ectx:_ ~eenv:_ =
      let open Fiber.O in
      let* () = Fiber.return () in
      let* res =
        let checksum = Option.map checksum ~f:snd in
        Dune_pkg.Fetch.fetch
          ~unpack:true
          ~checksum
          ~target:(Path.build target_dir)
          (OpamUrl.of_string url)
      in
      match res with
      | Ok () -> Fiber.return ()
      | Error (Checksum_mismatch actual_checksum) ->
        (match checksum with
         | None ->
           User_error.raise
             ~loc:loc_url
             [ Pp.text "No checksum provided. It should be:"
             ; Checksum.pp actual_checksum
             ]
         | Some (loc, _) ->
           User_error.raise
             ~loc
             [ Pp.text "Invalid checksum, got"; Dune_pkg.Checksum.pp actual_checksum ])
      | Error (Unavailable message) ->
        let loc = loc_url in
        (match message with
         | None -> User_error.raise ~loc [ Pp.text "Unknown fetch failure" ]
         | Some msg -> User_error.raise ~loc [ User_message.pp msg ])
    ;;
  end

  let action ~url ~checksum ~target_dir =
    let module M = struct
      type path = Path.t
      type target = Path.Build.t

      module Spec = Spec

      let v = { Spec.target_dir; checksum; url }
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
    | Some (Fetch { url = (loc, _) as url; checksum }) ->
      let fetch =
        Fetch.action ~url ~target_dir:pkg.paths.source_dir ~checksum
        |> Action.Full.make
        |> Action_builder.With_targets.return
        |> Action_builder.With_targets.add_directories
             ~directory_targets:[ pkg.paths.source_dir ]
      in
      Memo.return (Dep.Set.of_files [ Path.build pkg.paths.source_dir ], [ loc, fetch ])
    | Some (External_copy (loc, source_root)) ->
      let source_root = Path.external_ source_root in
      let+ source_files, rules =
        Pkg.source_files pkg ~loc
        >>| Path.Local.Set.fold ~init:([], []) ~f:(fun file (source_files, rules) ->
          let src = Path.append_local source_root file in
          let dst = Path.Build.append_local pkg.paths.source_dir file in
          let copy = loc, Action_builder.copy ~src ~dst in
          Path.build dst :: source_files, copy :: rules)
      in
      Dep.Set.of_files source_files, rules
  in
  let extra_source_deps, extra_copy_rules =
    List.map pkg.info.extra_sources ~f:(fun (local, fetch) ->
      let extra_source = Paths.extra_source pkg.paths local in
      let rule =
        match (fetch : Lock_dir.Source.t) with
        | External_copy (loc, src) ->
          loc, Action_builder.copy ~src:(Path.external_ src) ~dst:extra_source
        | Fetch { url = (loc, _) as url; checksum } ->
          let rule =
            Fetch.action ~url ~target_dir:pkg.paths.source_dir ~checksum
            |> Action.Full.make
            |> Action_builder.With_targets.return
            |> Action_builder.With_targets.add_directories
                 ~directory_targets:[ pkg.paths.source_dir ]
          in
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
              Install.Paths.get install_paths section
              |> Path.as_in_build_dir_exn
              |> Action.mkdir)
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
  let* db = DB.get context in
  let+ pkg =
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
  let directory_targets =
    let target_dir = paths.target_dir in
    let map = Path.Build.Map.singleton target_dir Loc.none in
    match pkg.info.source with
    | Some (Fetch f) -> Path.Build.Map.add_exn map paths.source_dir (fst f.url)
    | _ -> map
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
  let* lock_dir = Lock_dir.get context in
  let+ pkg =
    let* db = DB.get context in
    match lock_dir.ocaml with
    | None -> Resolve.resolve db context (Loc.none, ocaml_package_name)
    | Some ocaml -> Resolve.resolve db context ocaml
  in
  match pkg with
  | `System_provided -> None
  | `Inside_lock_dir pkg ->
    let toolchain =
      let cookie = (Pkg_installed.of_paths pkg.paths).cookie in
      let open Action_builder.O in
      let* cookie = cookie in
      let binaries =
        Section.Map.find cookie.files Bin |> Option.value ~default:[] |> Path.Set.of_list
      in
      let env = Pkg.exported_env pkg in
      Action_builder.of_memo @@ Ocaml_toolchain.of_binaries context env binaries
    in
    Some (Action_builder.memoize "ocaml_toolchain" toolchain)
;;

let all_packages context =
  let* db = DB.get context in
  let+ closure =
    Dune_lang.Package_name.Map.values db.all
    |> Memo.parallel_map ~f:(fun (package : Lock_dir.Pkg.t) ->
      let package = package.info.name in
      Resolve.resolve db context (Loc.none, package)
      >>| function
      | `Inside_lock_dir pkg -> Some pkg
      | `System_provided -> None)
    >>| List.filter_opt
    >>| Pkg.top_closure
  in
  match closure with
  | Error _ -> assert false
  | Ok closure -> closure
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

let lock_dir_active = Lock_dir.lock_dir_active
let lock_dir_path = Lock_dir.get_path

let exported_env context =
  let+ all_packages = all_packages context in
  let env = Pkg.build_env_of_deps all_packages in
  let vars = Env.Map.map env ~f:Env_update.string_of_env_values in
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
