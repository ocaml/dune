open Import
open Memo.O
module Package_variable = Dune_pkg.Package_variable
module Substs = Dune_pkg.Substs
module Depexts = Dune_pkg.Lock_dir.Depexts

module Variable = struct
  type value = OpamVariable.variable_contents =
    | B of bool
    | S of string
    | L of string list

  type t = Package_variable_name.t * value

  let value_repr =
    Repr.variant
      "package-variable-value"
      [ Repr.case "Bool" Repr.bool ~proj:(function
          | B b -> Some b
          | _ -> None)
      ; Repr.case "String" Repr.string ~proj:(function
          | S s -> Some s
          | _ -> None)
      ; Repr.case "Strings" (Repr.list Repr.string) ~proj:(function
          | L xs -> Some xs
          | _ -> None)
      ]
  ;;

  let repr = Repr.pair Package_variable_name.repr value_repr

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
end

module Paths = struct
  (* The [paths] of a package are the information about the artifacts
     that we know {e without} executing any commands. *)
  type 'a t =
    { source_dir : 'a
    ; target_dir : 'a
    ; extra_sources : 'a
    ; name : Package.Name.t
    ; install_roots : 'a Install.Roots.t Lazy.t
    ; install_paths : 'a Install.Paths.t Lazy.t
    ; prefix : 'a
    }

  let map_path t ~f =
    { t with
      source_dir = f t.source_dir
    ; target_dir = f t.target_dir
    ; extra_sources = f t.extra_sources
    ; install_roots = Lazy.map ~f:(Install.Roots.map ~f) t.install_roots
    ; install_paths = Lazy.map ~f:(Install.Paths.map ~f) t.install_paths
    ; prefix = f t.prefix
    }
  ;;

  let install_roots ~target_dir ~relative =
    Install.Roots.opam_from_prefix ~relative target_dir
  ;;

  let install_paths roots package ~relative = Install.Paths.make ~relative ~package ~roots

  let of_root name ~root ~relative =
    let source_dir = relative root "source" in
    let target_dir = relative root "target" in
    let extra_sources = relative root "extra_source" in
    let install_roots = lazy (install_roots ~target_dir ~relative) in
    let install_paths = lazy (install_paths (Lazy.force install_roots) name ~relative) in
    { source_dir
    ; target_dir
    ; extra_sources
    ; name
    ; install_paths
    ; install_roots
    ; prefix = target_dir
    }
  ;;

  let extra_source t extra_source = Path.append_local t.extra_sources extra_source

  let extra_source_build t extra_source =
    Path.Build.append_local t.extra_sources extra_source
  ;;

  let make_install_cookie target_dir ~relative = relative target_dir "cookie"

  let install_cookie' target_dir =
    make_install_cookie target_dir ~relative:Path.Build.relative
  ;;

  let install_cookie t = make_install_cookie t.target_dir ~relative:Path.relative

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
  let target_dir t = t.target_dir
end

module Install_cookie = struct
  (* The install cookie represents a serialized representation of all the
     installed artifacts and variables.

     The install cookie of a package is the source of all data we must refer to
     address a package's artifacts.

     It is constructed after we've built and installed the packages. In this
     sense, it is the "installation trace" that we must refer to so that we
     don't have to know anything about the installation procedure.
  *)

  module Gen = struct
    type 'files t =
      { files : 'files
      ; variables : Variable.t list
      }

    let repr files_repr =
      Repr.record
        "install-cookie"
        [ Repr.field "files" files_repr ~get:(fun t -> t.files)
        ; Repr.field "variables" (Repr.list Variable.repr) ~get:(fun t -> t.variables)
        ]
    ;;
  end

  type t = Path.t list Section.Map.t Gen.t

  module Persistent = Persistent.Make (struct
      type nonrec t = (Section.t * Path.t list) list Gen.t

      let sharing = false
      let name = "INSTALL-COOKIE"
      let version = 4
      let repr = Gen.repr (Repr.list (Repr.pair Section.repr (Repr.list Path.repr)))
    end)

  let load_exn f =
    match Persistent.load f with
    | Some f -> { f with files = Section.Map.of_list_exn f.files }
    | None -> User_error.raise ~loc:(Loc.in_file f) [ Pp.text "unable to load" ]
  ;;

  let dump path (t : t) =
    Persistent.dump path { t with files = Section.Map.to_list t.files }
  ;;
end

module Value_list_env = struct
  (* A representation of an environment where each variable can hold a
     list of [Value.t]. Each variable will be encoded into a delimited
     string (in the style of the PATH variable). *)
  type t = Value.t list Env.Map.t

  let global : t Lazy.t =
    let parse_strings s = Bin.parse s |> List.map ~f:(fun s -> Value.String s) in
    let of_env env : t = Env.to_map env |> Env.Map.map ~f:parse_strings in
    lazy (of_env (Global.env ()))
  ;;

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
      Some (Value.Dir path :: paths))
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
        let old_v =
          match old_v with
          | None | Some [] -> [ Value.String "" ]
          | Some v -> v
        in
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

module Expander0 = struct
  include Expander0

  type t =
    { name : Dune_pkg.Package_name.t
    ; paths : Path.t Paths.t
    ; artifacts : Path.t Filename.Map.t Memo.t
    ; depends :
        (Variable.value Package_variable_name.Map.t * Path.t Paths.t) Package.Name.Map.t
          Memo.t
    ; depexts : Depexts.t list
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
      ; depends :
          (Variable.value Package_variable_name.Map.t * Path.t Paths.t) Package.Name.Map.t
      ; artifacts : Path.t Filename.Map.t
      ; src : 'src
      ; dst : 'dst
      }

    let name = "substitute"
    let version = 4
    let runs_process = false
    let can_run_in_action_runner = false
    let bimap t f g = { t with src = f t.src; dst = g t.dst }
    let is_useful_to_memoize = true

    let variable_value_repr =
      Repr.variant
        "pkg-variable-value"
        [ Repr.case "B" Bool.repr ~proj:(function
            | Variable.B x -> Some x
            | _ -> None)
        ; Repr.case "S" String.repr ~proj:(function
            | Variable.S x -> Some x
            | _ -> None)
        ; Repr.case
            "L"
            Repr.(list String.repr)
            ~proj:(function
              | Variable.L x -> Some x
              | _ -> None)
        ]
    ;;

    let variable_map_repr =
      Repr.view
        Repr.(list (pair Package_variable_name.repr variable_value_repr))
        ~to_:Package_variable_name.Map.to_list
    ;;

    let paths_repr = Repr.T3.repr Path.repr Path.repr Package.Name.repr

    let hash_input_repr =
      Repr.T4.repr
        paths_repr
        Repr.(list (pair Filename.repr Path.repr))
        Repr.(list (pair variable_map_repr paths_repr))
        Package_version.repr
    ;;

    let encode { expander; depends; artifacts; src; dst } input output : Sexp.t =
      let e =
        let paths (p : Path.t Paths.t) = p.source_dir, p.target_dir, p.name in
        Digest.repr
          hash_input_repr
          ( paths expander.paths
          , Filename.Map.to_list artifacts
          , Package.Name.Map.to_list_map depends ~f:(fun _ (m, p) -> m, paths p)
          , expander.version )
        |> Digest.to_string_raw
      in
      List [ Atom e; input src; output dst ]
    ;;

    let action { expander; depends = _; artifacts = _; src; dst } ~ectx:_ ~eenv:_ =
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

  module A = Action_ext.Make (Spec)

  let action (expander : Expander.t) ~src ~dst =
    let+ depends = expander.depends
    and+ artifacts = expander.artifacts in
    A.action { Spec.expander; depends; artifacts; src; dst }
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
      | Toplevel -> Path.relative roots.lib_root "toplevel"
      | Stublibs -> Path.relative roots.lib_root "stublibs"
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

    let expand_pkg (paths : Path.t Paths.t) (pform : Pform.Var.Pkg.t) =
      match pform with
      | Switch -> Memo.return [ Value.String "dune" ]
      | Os Os -> sys_poll_var (fun { os; _ } -> os)
      | Os Os_version -> sys_poll_var (fun { os_version; _ } -> os_version)
      | Os Os_distribution -> sys_poll_var (fun { os_distribution; _ } -> os_distribution)
      | Os Os_family -> sys_poll_var (fun { os_family; _ } -> os_family)
      | Sys_ocaml_version ->
        sys_poll_var (fun { sys_ocaml_version; _ } -> sys_ocaml_version)
      | Build -> Memo.return [ Value.Dir paths.source_dir ]
      | Prefix -> Memo.return [ Value.Dir paths.prefix ]
      | User -> Memo.return [ Value.String (Unix.getlogin ()) ]
      | Jobs -> Memo.return [ Value.String (Int.to_string !Clflags.concurrency) ]
      | Arch -> sys_poll_var (fun { arch; _ } -> arch)
      | Group ->
        let group = Unix.getgid () |> Unix.getgrgid in
        Memo.return [ Value.String group.gr_name ]
      | Section_dir section ->
        let roots = Paths.install_roots paths in
        let dir = section_dir_of_root roots section in
        Memo.return [ Value.Dir dir ]
    ;;

    let expand_pkg_macro ~loc (paths : _ Paths.t) deps macro_invocation =
      let* deps = deps in
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
         | "build-id" ->
           (* build-id is used by some packages (e.g., relocatable-compiler) for
              caching across opam switches. Since dune doesn't do this, we use a
              fixed dummy value that won't collide with real opam build-ids. *)
           Memo.return @@ Ok [ Value.String "d00ed00ed00ed00ed00ed00ed00ed00e" ]
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
                 Memo.return @@ Ok [ Value.Dir (Install.Paths.get install_paths section) ])))
    ;;

    let expand_pform
          { name = _
          ; env = _
          ; paths
          ; artifacts = _
          ; context
          ; depends
          ; version = _
          ; depexts = _
          }
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
      String_expander.Memo.expand ~dir:t.paths.source_dir ~f:(fun ~source pform ->
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

    let slang_expander t sw =
      String_expander.Memo.expand_result_deferred_concat sw ~mode:Many ~f:(expand_pform t)
    ;;

    let eval_blang t blang =
      Slang_expand.eval_blang blang ~dir:t.paths.source_dir ~f:(slang_expander t)
    ;;

    let eval_slangs_located t slangs =
      let slangs =
        List.map slangs ~f:(fun slang ->
          Slang.map_loc slang ~f:Dune_pkg.Lock_dir.loc_in_source_tree)
      in
      Slang_expand.eval_multi_located slangs ~dir:t.paths.source_dir ~f:(slang_expander t)
    ;;

    let filtered_depexts t =
      Memo.List.filter_map t.depexts ~f:(fun (depexts : Depexts.t) ->
        let+ enabled =
          match depexts.enabled_if with
          | `Always -> Memo.return true
          | `Conditional condition -> eval_blang t condition
        in
        if enabled then Some depexts.external_package_names else None)
      >>| List.concat
      >>| List.sort_uniq ~compare:String.compare
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
             let dir = t.paths.source_dir in
             Memo.return @@ Ok (Path.relative dir program)
           | In_path ->
             (match program with
              | "dune" ->
                let dune = Path.of_string Sys.executable_name in
                Memo.return @@ Ok dune
              | program ->
                let program = Filename.of_string_exn program in
                let* artifacts = t.artifacts in
                (match Filename.Map.find artifacts program with
                 | Some s -> Memo.return @@ Ok s
                 | None ->
                   (let path = Global.env () |> Env_path.path in
                    Which.which ~path program)
                   >>= (function
                    | Some p -> Memo.return (Ok p)
                    | None ->
                      let+ depexts = filtered_depexts t in
                      let hint =
                        Run_with_path.depexts_hint depexts
                        |> Option.map ~f:(fun pp -> Format.asprintf "%a" Pp.to_fmt pp)
                      in
                      Error
                        (Action.Prog.Not_found.create
                           ?hint
                           ~program
                           ~context:t.context
                           ~loc:(Some loc)
                           ())))))
      in
      Result.map prog ~f:(map_exe t)
    ;;
  end

  let rec expand (action : Dune_lang.Action.t) ~(expander : Expander.t) =
    let dir = expander.paths.source_dir in
    match action with
    | Run args ->
      Expander.eval_slangs_located expander args
      >>= (function
       | [] ->
         let loc =
           let loc = function
             | Slang.Nil -> None
             | Slang.Undefined -> None
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
         and+ depexts = Expander.filtered_depexts expander in
         let args =
           Array.Immutable.of_list_map args ~f:(fun (_loc, arg) ->
             Value.Deferred_concat.parts arg
             |> Array.Immutable.of_list_map ~f:(fun (arg : Value.t) ->
               match arg with
               | String s -> Run_with_path.Spec.String s
               | Path p | Dir p -> Path p))
         in
         let ocamlfind_destdir = (Lazy.force expander.paths.install_roots).lib_root in
         Run_with_path.action
           ~depexts
           ~pkg:(expander.name, prog_loc)
           exe
           args
           ~ocamlfind_destdir)
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
      let* src =
        Expander.expand_pform_gen ~mode:Single expander src >>| Value.to_path ~dir
      and* dst =
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
      let dir = expander.paths.source_dir in
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
      Action.Setenv (Env.Var.to_string k, v, action))
  ;;

  let exported_env (expander : Expander.t) (env : _ Env_update.t) =
    let+ value =
      let+ value = Expander.expand_pform_gen expander env.value ~mode:Single in
      value |> Value.to_string ~dir:expander.paths.source_dir
    in
    { env with value }
  ;;
end

module Install_action = struct
  (* The install action does the following:

     1. Runs the install action in the lock file (if it exists)
     2. Reads the .install file produced by the build command
     3. Discovers all the files produced by 1.
     4. Combines the set of files in 2. and 3. to produce a "cookie" file
  *)

  let installable_sections =
    Section.(Set.diff all (Set.of_list [ Misc; Libexec; Libexec_root ]))
    |> Section.Set.to_list
  ;;

  module Spec = struct
    type ('path, 'target) t =
      { (* location of the install file we must read (if produced) *)
        install_file : 'path
      ; (* location of the variables we must read (if produced) *)
        config_file : 'path
      ; (* where we are supposed to put the installed artifacts *)
        target_dir : 'target
      ; (* if the package's installation prefix is outside the build
           dir, it's stored here and will be used instead of [target_dir]
           as the location of insntalled artifacts *)
        prefix_outside_build_dir : Path.Outside_build_dir.t option
      ; (* does the package have its own install command? *)
        install_action : [ `Has_install_action | `No_install_action ]
      ; package : Package.Name.t
      }

    let name = "install-file-run"
    let version = 1
    let runs_process = false
    let can_run_in_action_runner = false

    let bimap
          ({ install_file
           ; config_file
           ; target_dir
           ; prefix_outside_build_dir = _
           ; install_action = _
           ; package = _
           } as t)
          f
          g
      =
      { t with
        install_file = f install_file
      ; config_file = f config_file
      ; target_dir = g target_dir
      }
    ;;

    let is_useful_to_memoize = true

    let encode
          { install_file
          ; config_file
          ; target_dir
          ; prefix_outside_build_dir
          ; install_action
          ; package
          }
          path
          target
      : Sexp.t
      =
      List
        [ path install_file
        ; path config_file
        ; target target_dir
        ; (match
             Option.map
               prefix_outside_build_dir
               ~f:Path.Outside_build_dir.to_string_maybe_quoted
           with
           | None -> List []
           | Some s -> List [ Atom s ])
        ; Atom (Package.Name.to_string package)
        ; Atom
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
            |> Filename.to_string
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

    let collect_files ~root ~skip_dirs =
      List.iter skip_dirs ~f:(fun s -> assert (Path.equal root (Path.parent_exn s)));
      let path_of ~dir fname = Path.relative_fname (Path.relative root dir) fname in
      Fpath.traverse
        ~dir:(Path.to_string root)
        ~init:[]
        ~on_file:(fun ~dir fname acc -> path_of ~dir fname :: acc)
        ~enter_dir:(fun ~dir fname ->
          let path = path_of ~dir fname in
          not (List.mem skip_dirs path ~equal:Path.equal))
        ~on_error:
          (`Call
              (fun ~dir error acc ->
                if dir = "" then acc else Unix_error.Detailed.raise error))
        ()
    ;;

    let maybe_drop_sandbox_dir path =
      match Path.extract_build_context_dir_maybe_sandboxed path with
      | None -> path
      | Some (sandbox, source) ->
        let ctx =
          let name = Path.basename sandbox in
          Path.relative_fname (Path.build Path.Build.root) name
        in
        Path.append_source ctx source
    ;;

    let section_map_of_dir install_paths =
      (* reverse engineer the installed artifacts from running the install
         action by looking at the file system post running the action and
         taking educated guesses about which section each file belongs to *)
      let get = Install.Paths.get install_paths in
      List.concat_map installable_sections ~f:(fun section ->
        let path = get section in
        let files =
          match section with
          | Lib_root ->
            collect_files ~root:path ~skip_dirs:[ get Toplevel; get Stublibs; get Lib ]
          | Share_root -> collect_files ~root:path ~skip_dirs:[ get Share ]
          | _ -> collect_files ~root:path ~skip_dirs:[]
        in
        files
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
              let perm =
                (Unix.stat (Path.to_string file)).st_perm |> Permissions.Mode.of_int
              in
              if Permissions.(test execute perm) then section' else section
          in
          section, maybe_drop_sandbox_dir file))
      |> Section.Map.of_list_multi
    ;;

    let maybe_set_executable section dst =
      match Section.should_set_executable_bit section with
      | false -> ()
      | true ->
        let dst = Path.to_string dst in
        let permission =
          let perm = (Unix.stat dst).st_perm |> Permissions.Mode.of_int in
          Permissions.(add execute) perm |> Permissions.Mode.to_int
        in
        Unix.chmod dst permission
    ;;

    let read_variables config_file =
      match Fpath.exists (Path.to_string config_file) with
      | false -> []
      | true ->
        let config =
          let filename = Path.to_string config_file in
          match
            Io.read_file_exn config_file
            |> OpamFile.Dot_config.read_from_string
                 ~filename:(OpamFile.make (OpamFilename.of_string filename))
          with
          | s -> s
          | exception OpamPp.Bad_format (pos, message) ->
            let loc =
              Option.map
                pos
                ~f:(fun { OpamParserTypes.FullPos.filename = _; start; stop } ->
                  let file_contents = Io.read_file_exn config_file in
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
              let error =
                Pp.textf
                  "Error parsing %s"
                  (Path.basename config_file |> Filename.to_string)
              in
              match loc with
              | None -> error
              | Some loc ->
                (Loc.pp loc |> Pp.map_tags ~f:(fun Loc.Loc -> User_message.Style.Loc))
                ++ error
            in
            User_error.raise [ message_with_loc; User_error.reason (Pp.verbatim message) ]
        in
        OpamFile.Dot_config.bindings config
        |> List.map ~f:(fun (name, value) -> Package_variable_name.of_opam name, value)
    ;;

    (* On Windows, .install files may omit .exe for bin/sbin entries.
       Try the .exe variant if the original doesn't exist, mirroring opam. *)
    let maybe_add_exe src (entry : Path.t Install.Entry.Expanded.t) =
      match
        match Sys.win32, entry.section with
        | true, (Bin | Sbin) when not (Fpath.exists (Path.to_string src)) ->
          let src_exe_str = Bin.add_exe (Path.to_string src) in
          if Fpath.exists src_exe_str then Some src_exe_str else None
        | _ -> None
      with
      | None -> src, entry
      | Some src_exe_str ->
        ( Path.of_string src_exe_str
        , Install.Entry.map_dst entry ~f:Install.Entry.Dst.maybe_add_exe )
    ;;

    let install_entry
          ~src
          ~install_file
          ~target_dir
          (entry : Path.t Install.Entry.Expanded.t)
      =
      let src, entry = maybe_add_exe src entry in
      match Fpath.exists (Path.to_string src), entry.optional with
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
         Io.portable_hardlink ~overwrite:true ~src ~dst);
        maybe_set_executable entry.section dst;
        Some (entry.section, dst)
    ;;

    let resolve_symlinks_in root =
      let on_symlink ~dir fname () =
        let path = Filename.concat root (Filename.append dir fname) in
        match Fpath.follow_symlink path with
        | Error (Unix_error e) -> Unix_error.Detailed.raise e
        | Error Not_a_symlink ->
          Code_error.raise
            "resolve_symlinks_in: not a symlink"
            [ "path", Dyn.string path ]
        | Error Max_depth_exceeded ->
          User_error.raise
            [ Pp.textf
                "Unable to resolve symlink %s: too many levels of symbolic links"
                path
            ]
        | Ok resolved ->
          (match Unix.lstat resolved with
           | { Unix.st_kind = S_REG; _ } ->
             (* CR-someday rgrinberg: pass chmod:true here? *)
             Fpath.unlink_exn path;
             Io.portable_hardlink
               ~overwrite:true
               ~src:(Path.of_string resolved)
               ~dst:(Path.of_string path)
           | _ -> ());
          (), None
      in
      Fpath.traverse
        ~dir:root
        ~init:()
        ~on_other:`Ignore
        ~on_symlink:(`Call on_symlink)
        ()
    ;;

    let action
          { package
          ; install_file
          ; config_file
          ; target_dir
          ; prefix_outside_build_dir
          ; install_action
          }
          ~ectx:_
          ~eenv:_
      =
      let open Fiber.O in
      let* () = Fiber.return () in
      let* files =
        let from_install_action =
          let target_dir =
            (* If the package used a prefix that was outside the build
               directory (as is the case with toolchains), parse the
               installed sections from that location. Otherwise parse the
               installed sections from the package's location within the
               build directory. *)
            match prefix_outside_build_dir with
            | Some prefix_outside_build_dir ->
              Path.outside_build_dir prefix_outside_build_dir
            | None -> Path.build target_dir
          in
          match install_action with
          | `No_install_action -> Section.Map.empty
          | `Has_install_action ->
            let install_paths =
              Paths.of_root
                package
                ~root:(Path.parent_exn target_dir)
                ~relative:Path.relative
              |> Paths.install_paths
            in
            section_map_of_dir install_paths
        in
        let+ from_install_file =
          (* Read all the artifacts from the .install file produced by
             the build command. This is the happy path where we don't guess
             anything. *)
          Async.async (fun () -> Fpath.exists (Path.to_string install_file))
          >>= function
          | false -> Fiber.return Section.Map.empty
          | true ->
            let* map =
              let install_entries =
                let dir = Path.parent_exn install_file in
                Install.Entry.Expanded.load_install_file install_file (fun local ->
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
            let+ () =
              Async.async (fun () ->
                (* CR-someday rgrinberg: pass chmod:true here? *)
                Fpath.unlink_exn (Path.to_string install_file))
            in
            map
        in
        (* Combine the artifacts declared in the .install, and the ones we discovered
           by runing the install action *)
        (* TODO we should make sure that overwrites aren't allowed *)
        Section.Map.union from_install_action from_install_file ~f:(fun _ x y ->
          Some (x @ y))
        |> Section.Map.map ~f:(List.sort ~compare:Path.compare)
      in
      let* cookies =
        let+ variables = Async.async (fun () -> read_variables config_file) in
        { Install_cookie.Gen.files; variables }
      in
      Async.async (fun () ->
        (* Resolve symlinks in target_dir so that the cache can store them. The
         dune cache doesn't support symlinks, so we replace them with hardlinks
         to their targets. *)
        if Fpath.exists (Path.Build.to_string target_dir)
        then resolve_symlinks_in (Path.Build.to_string target_dir);
        (* Produce the cookie file in the standard path *)
        let cookie_file = Path.build @@ Paths.install_cookie' target_dir in
        cookie_file |> Path.parent_exn |> Path.mkdir_p;
        Install_cookie.dump cookie_file cookies)
    ;;
  end

  module A = Action_ext.Make (Spec)

  let action (p : Path.Build.t Paths.t) install_action ~prefix_outside_build_dir =
    A.action
      { Spec.install_file = Path.build @@ Paths.install_file p
      ; config_file = Path.build @@ Paths.config_file p
      ; target_dir = p.target_dir
      ; prefix_outside_build_dir
      ; install_action
      ; package = p.name
      }
  ;;
end
