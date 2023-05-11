open Import
open Memo.O
open Dune_pkg

[@@@ocaml.warning "-32-69"]

(* TODO
   - substitutions
   - extra-files
   - patches
   - build env
   - post dependencies
   - build dependencies
   - cross compilation
   - filters
   - stage forms: with-test, with-doc, with-dev-setup
   - full support for dune actions
   - initialize context using packages in lock file
   - sandboxing
*)

module Source = struct
  type t =
    | External_copy of Loc.t * Path.External.t
    | Fetch of
        { url : Loc.t * string
        ; checksum : (Loc.t * Checksum.t) option
        }

  let decode =
    let open Dune_lang.Decoder in
    sum
      [ ( "copy"
        , located string >>| fun (loc, source) path ->
          External_copy
            ( loc
            , if Filename.is_relative source then
                Path.External.relative path source
              else Path.External.of_string source ) )
      ; ( "fetch"
        , enter @@ fields
          @@ let+ url = field "url" (located string)
             and+ checksum = field_o "checksum" (located string) in
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
end

module Variable = struct
  type value = OpamVariable.variable_contents =
    | B of bool
    | S of string
    | L of string list

  type t = string * value

  let dyn_of_value : value -> Dyn.t =
    let open Dyn in
    function
    | B b -> variant "Bool" [ bool b ]
    | S s -> variant "String" [ string s ]
    | L xs -> variant "Strings" [ list string xs ]

  let dune_value : value -> Value.t list = function
    | B b -> [ String (Bool.to_string b) ]
    | S s -> [ String s ]
    | L s -> List.map s ~f:(fun x -> Value.String x)

  let to_dyn (name, value) = Dyn.(pair string dyn_of_value (name, value))
end

module Pkg_info = struct
  type t =
    { name : Package.Name.t
    ; version : string
    ; dev : bool
    ; source : Source.t option
    }

  let variables t =
    String.Map.of_list_exn
      [ ("name", Variable.S (Package.Name.to_string t.name))
      ; ("version", S t.version)
      ; ("dev", B t.dev)
      ]
end

module Env_update = struct
  type 'a t =
    { op : OpamParserTypes.env_update_op
    ; var : Env.Var.t
    ; value : 'a
    }

  let decode =
    let open Dune_lang.Decoder in
    let env_update_op =
      enum
        [ ("=", OpamParserTypes.Eq)
        ; ("+=", PlusEq)
        ; ("=+", EqPlus)
        ; (":=", ColonEq)
        ; ("=:", EqColon)
        ; ("=+=", EqPlusEq)
        ]
    in
    let+ op, var, value = triple env_update_op string String_with_vars.decode in
    { op; var; value }
end

module Lock_file = struct
  let syntax =
    Dune_sexp.Syntax.create ~experimental:true ~name:"package"
      ~desc:"the package management language"
      [ ((0, 1), `Since (0, 0)) ]

  module Pkg = struct
    type t =
      { build_command : Action_unexpanded.t option
      ; install_command : Action_unexpanded.t option
      ; deps : Package.Name.t list
      ; info : Pkg_info.t
      ; lock_dir : Path.Source.t
      ; exported_env : String_with_vars.t Env_update.t list
      }

    let decode =
      let open Dune_lang.Decoder in
      enter @@ fields
      @@ let+ version = field ~default:"dev" "version" string
         and+ install_command = field_o "install" Action_unexpanded.decode
         and+ build_command = field_o "build" Action_unexpanded.decode
         and+ deps = field ~default:[] "deps" (repeat Package.Name.decode)
         and+ source = field_o "source" Source.decode
         and+ dev = field_b "dev"
         and+ exported_env =
           field "exported_env" ~default:[] (repeat Env_update.decode)
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
           { build_command
           ; deps
           ; install_command
           ; info
           ; exported_env
           ; lock_dir
           }
  end

  type t =
    { version : Syntax.Version.t
    ; packages : Pkg.t Package.Name.Map.t
    }

  let path = Path.Source.(relative root "dune.lock")

  let metadata = "lock.dune"

  module Metadata = Dune_sexp.Versioned_file.Make (Unit)

  let () = Metadata.Lang.register syntax ()

  let get () : t Memo.t =
    Fs_memo.dir_exists (In_source_dir path) >>= function
    | false ->
      (* TODO *)
      User_error.raise [ Pp.text "" ]
    | true -> (
      Fs_memo.dir_contents (In_source_dir path) >>= function
      | Error _ ->
        (* TODO *)
        User_error.raise [ Pp.text "" ]
      | Ok content ->
        let* version =
          Fs_memo.with_lexbuf_from_file
            (In_source_dir (Path.Source.relative path metadata))
            ~f:(fun lexbuf ->
              Metadata.parse_contents lexbuf ~f:(fun instance ->
                  Dune_sexp.Decoder.return instance.version))
        in
        let+ packages =
          Fs_cache.Dir_contents.to_list content
          |> List.filter_map ~f:(fun (name, (kind : Unix.file_kind)) ->
                 match kind with
                 | S_REG when name <> metadata ->
                   let name = Package.Name.of_string name in
                   Some name
                 | _ ->
                   (* TODO *)
                   None)
          |> Memo.parallel_map ~f:(fun name ->
                 let+ package =
                   let+ sexp =
                     let path =
                       Package.Name.to_string name |> Path.Source.relative path
                     in
                     Fs_memo.with_lexbuf_from_file (In_source_dir path)
                       ~f:(Dune_sexp.Parser.parse ~mode:Many)
                   in
                   let parser =
                     let env = Pform.Env.pkg version in
                     String_with_vars.set_decoding_env env Pkg.decode
                   in
                   (Dune_lang.Decoder.parse parser Univ_map.empty
                      (List (Loc.none, sexp)))
                     ~lock_dir:path name
                 in
                 (name, package))
          >>| Package.Name.Map.of_list_exn
        in
        { packages; version })
end

module Paths = struct
  type t =
    { source_dir : Path.Build.t
    ; target_dir : Path.Build.t
    ; name : Package.Name.t
    ; install_roots : Path.t Install.Section.Paths.Roots.t Lazy.t
    ; install_paths : Install.Section.Paths.t Lazy.t
    }

  let install_roots ~target_dir =
    Path.build target_dir |> Install.Section.Paths.Roots.opam_from_prefix

  let install_paths roots package = Install.Section.Paths.make ~package ~roots

  let of_root name ~root =
    let source_dir = Path.Build.relative root "source" in
    let target_dir = Path.Build.relative root "target" in
    let install_roots = lazy (install_roots ~target_dir) in
    let install_paths = lazy (install_paths (Lazy.force install_roots) name) in
    { source_dir; target_dir; name; install_paths; install_roots }

  let make name (ctx : Context_name.t) =
    let build_dir = Context_name.build_dir ctx in
    let root =
      Path.Build.L.relative build_dir [ ".pkg"; Package.Name.to_string name ]
    in
    of_root name ~root

  let install_cookie' target_dir = Path.Build.relative target_dir "cookie"

  let install_cookie t = install_cookie' t.target_dir

  let install_file t =
    Path.Build.relative t.source_dir
      (sprintf "%s.install" (Package.Name.to_string t.name))

  let config_file t =
    Path.Build.relative t.source_dir
      (sprintf "%s.config" (Package.Name.to_string t.name))

  let entry_dst t entry =
    let paths = Lazy.force t.install_paths in
    Install.Entry.relative_installed_path entry ~paths

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
      [ ("files", Section.Map.to_dyn (list Path.to_dyn) files)
      ; ("variables", list Variable.to_dyn variables)
      ]

  include Dune_util.Persistent.Make (struct
    type nonrec t = t

    let name = "INSTALL-COOKIE"

    let version = 1

    let to_dyn = to_dyn
  end)

  let load_exn f =
    match load f with
    | None -> User_error.raise [ Pp.text "unable to load" ]
    | Some f -> f
end

module Pkg = struct
  module Id = Id.Make ()

  type t =
    { id : Id.t
    ; build_command : Action_unexpanded.t option
    ; install_command : Action_unexpanded.t option
    ; deps : t list
    ; info : Pkg_info.t
    ; paths : Paths.t
    ; files_dir : Path.Source.t
    ; mutable exported_env : string Env_update.t list
    }

  module Top_closure = Top_closure.Make (Id.Set) (Monad.Id)

  let top_closure deps =
    Top_closure.top_closure deps ~key:(fun t -> t.id) ~deps:(fun t -> t.deps)

  let deps_closure t =
    match top_closure t.deps with
    | Ok s -> s
    | Error _ -> assert false

  let source_files t =
    let rec loop root acc path =
      let* contents =
        let path =
          (* TODO add [External.append_local] *)
          Path.append_local (Path.external_ root) path
          |> Path.as_external |> Option.value_exn
        in
        Fs_memo.dir_contents (External path)
      in
      match contents with
      | Error _ ->
        (* TODO *)
        assert false
      | Ok contents ->
        let contents = Fs_cache.Dir_contents.to_list contents in
        let files, dirs =
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
    | Some (External_copy (_, root)) ->
      loop root Path.Local.Set.empty Path.Local.root
    | Some (Fetch _) -> assert false

  let package_deps t =
    deps_closure t
    |> List.fold_left ~init:Dep.Set.empty ~f:(fun acc t ->
           Path.build t.paths.target_dir |> Dep.file |> Dep.Set.add acc)

  let update kind ~new_v ~old_v ~f =
    if new_v = "" then old_v
    else
      match kind with
      | `Colon ->
        let old_v = Option.value ~default:[] old_v in
        Some (f ~old_v ~new_v)
      | `Plus -> (
        match old_v with
        | None | Some [] -> Some [ Value.String new_v ]
        | Some old_v -> Some (f ~old_v ~new_v))

  let append = update ~f:(fun ~old_v ~new_v -> old_v @ [ Value.String new_v ])

  let prepend = update ~f:(fun ~old_v ~new_v -> Value.String new_v :: old_v)

  let exported_env t =
    let base =
      Env.Map.of_list_exn
        [ ("OPAM_SWITCH_PREFIX", Path.Build.to_string t.paths.target_dir)
        ; ("CDPATH", "")
        ; ("MAKELEVEL", "")
        ; ("OPAM_PACKAGE_NAME", Package.Name.to_string t.info.name)
        ; ("OPAM_PACKAGE_VERSION", t.info.version)
        ; ("OPAMCLI", "2.0")
        ]
    in
    let env =
      deps_closure t
      |> List.fold_left ~init:Env.Map.empty ~f:(fun env t ->
             let env =
               let paths = Paths.install_paths t.paths in
               let env =
                 Value.Dir (Install.Section.Paths.get paths Bin)
                 |> Env.Map.add_multi env "PATH"
               in
               let env =
                 Value.Dir (Install.Section.Paths.get paths Toplevel)
                 |> Env.Map.add_multi env "OCAMLTOP_INCLUDE_PATH"
               in
               let env =
                 Value.Dir (Install.Section.Paths.get paths Toplevel)
                 |> Env.Map.add_multi env "OCAMLTOP_INCLUDE_PATH"
               in
               let env =
                 Value.Dir (Install.Section.Paths.get paths Lib)
                 |> Env.Map.add_multi env "OCAMLPATH"
               in
               let env =
                 Value.Dir (Install.Section.Paths.get paths Stublibs)
                 |> Env.Map.add_multi env "CAML_LD_LIBRARY_PATH"
               in
               Value.Dir (Install.Section.Paths.get paths Man)
               |> Env.Map.add_multi env "MANPATH"
             in
             List.fold_left t.exported_env ~init:env
               ~f:(fun env { Env_update.op; var = k; value = new_v } ->
                 Env.Map.update env k ~f:(fun old_v ->
                     let append = append ~new_v ~old_v in
                     let prepend = prepend ~new_v ~old_v in
                     match op with
                     | Eq ->
                       if new_v = "" then
                         if Sys.win32 then None else Some [ String "" ]
                       else Some [ Value.String new_v ]
                     | PlusEq -> prepend `Plus
                     | ColonEq -> prepend `Colon
                     | EqPlus -> append `Plus
                     | EqColon -> append `Colon
                     | EqPlusEq ->
                       (* TODO nobody uses this AFAIK *)
                       assert false)))
      |> Env.Map.map ~f:(fun values ->
             List.map values ~f:(function
               | Value.String s -> s
               | Dir s | Path s -> Path.to_absolute_filename s)
             |> Bin.encode_strings)
    in
    Env.extend Env.empty ~vars:(Env.Map.superpose env base)
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
end

module Action_expander = struct
  module Expander = struct
    module String_expander = String_with_vars.Make_expander (Applicative.Id)

    type t =
      { paths : Paths.t
      ; artifacts : Path.t Filename.Map.t
      ; deps : (Variable.value String.Map.t * Paths.t) Package.Name.Map.t
      ; context : Context_name.t
      ; version : string
      }

    let map_exe _ x =
      (* TODO *)
      x

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
      | Misc -> Misc

    let section_dir_of_root (roots : _ Install.Section.Paths.Roots.t)
        (section : Pform.Var.Pkg.Section.t) =
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
      | Misc -> assert false

    let expand_pform { paths; artifacts = _; context = _; deps; version = _ }
        ~source (pform : Pform.t) : Value.t list =
      let loc = Dune_sexp.Template.Pform.loc source in
      match pform with
      | Var (Pkg Switch) -> [ String "dune" ]
      | Var (Pkg Os_version) -> [ String (assert false) ]
      | Var (Pkg Os_distribution) -> [ String (assert false) ]
      | Var (Pkg Os_family) -> [ String (assert false) ]
      | Var (Pkg Build) -> [ Dir (Path.build paths.source_dir) ]
      | Var (Pkg Prefix) -> [ Dir (Path.build paths.target_dir) ]
      | Var (Pkg User) -> [ Value.String (Unix.getlogin ()) ]
      | Var (Pkg Jobs) -> [ String "1" ]
      | Var (Pkg Arch) -> [ String (assert false) ]
      | Var Make ->
        (* TODO *)
        assert false
      | Var (Pkg Group) ->
        let group = Unix.getgid () |> Unix.getgrgid in
        [ String group.gr_name ]
      | Var (Pkg (Section_dir section)) ->
        let roots = Paths.install_roots paths in
        let dir = section_dir_of_root roots section in
        [ Value.Dir dir ]
      | Macro (Pkg, arg) -> (
        match String.split arg ~on:':' with
        | [ "var"; name; var ] -> (
          let variables, paths =
            let name =
              if name = "_" then paths.name else Package.Name.of_string name
            in
            match Package.Name.Map.find deps name with
            | None -> (String.Map.empty, None)
            | Some (var, paths) -> (var, Some paths)
          in
          let present = Option.is_some paths in
          match String.Map.find variables var with
          | Some v -> Variable.dune_value v
          | None -> (
            match var with
            | "pinned" -> [ String "false" ]
            | "enable" -> [ String (if present then "enable" else "disable") ]
            | "installed" -> [ String (Bool.to_string present) ]
            | _ -> (
              match paths with
              | None -> assert false
              | Some paths -> (
                match Pform.Var.Pkg.Section.of_string var with
                | None ->
                  User_error.raise ~loc [ Pp.textf "invalid section %S" var ]
                | Some section ->
                  let section = dune_section_of_pform section in
                  let install_paths = Paths.install_paths paths in
                  [ Dir (Install.Section.Paths.get install_paths section) ]))))
        | _ -> assert false)
      | _ -> Expander.isn't_allowed_in_this_position ~source

    let expand_pform_gen t =
      String_expander.expand ~f:(expand_pform t)
        ~dir:(Path.build t.paths.source_dir)

    let expand_pform = expand_pform_gen ~mode:Many

    let expand_exe t sw =
      let dir = Path.build t.paths.source_dir in
      let prog, args = expand_pform_gen t sw ~mode:At_least_one in
      let+ prog =
        let loc = String_with_vars.loc sw in
        match prog with
        | Value.Dir p ->
          User_error.raise ~loc
            [ Pp.textf "%s is a directory and cannot be used as an executable"
                (Path.to_string_maybe_quoted p)
            ]
        | Path p -> Memo.return @@ Ok p
        | String program -> (
          match Filename.analyze_program_name program with
          | Relative_to_current_dir | Absolute ->
            Memo.return @@ Ok (Path.relative dir program)
          | In_path -> (
            match Filename.Map.find t.artifacts program with
            | Some s -> Memo.return @@ Ok s
            | None -> (
              let+ path =
                let path = Global.env () |> Env_path.path in
                Which.which ~path program
              in
              match path with
              | Some p -> Ok p
              | None ->
                Error
                  (Action.Prog.Not_found.create ~program ~context:t.context
                     ~loc:(Some loc) ()))))
      in
      let prog = Result.map prog ~f:(map_exe t) in
      (prog, args)
  end

  let rec expand (action : Action_unexpanded.t) ~(expander : Expander.t) =
    let dir = Path.build expander.paths.source_dir in
    match action with
    | Run (exe, args) ->
      let+ exe, more_args = Expander.expand_exe expander exe in
      let args =
        more_args @ List.concat_map args ~f:(Expander.expand_pform expander)
        |> Value.L.to_strings ~dir
      in
      Action.Run (exe, args)
    | Progn t ->
      let+ args = Memo.parallel_map t ~f:(expand ~expander) in
      Action.Progn args
    | System arg ->
      let arg =
        Expander.expand_pform_gen ~mode:Single expander arg
        |> Value.to_string ~dir
      in
      Memo.return @@ Action.System arg
    | _ ->
      (* TODO *)
      assert false

  let expander context (pkg : Pkg.t) =
    let+ artifacts, deps =
      Pkg.deps_closure pkg
      |> Memo.parallel_map ~f:(fun (pkg : Pkg.t) ->
             let cookie = (Pkg_installed.of_paths pkg.paths).cookie in
             Action_builder.run cookie Eager
             |> Memo.map ~f:(fun ((cookie : Install_cookie.t), _) ->
                    (pkg, cookie)))
      |> Memo.map ~f:(fun cookies ->
             List.fold_left cookies
               ~init:(Filename.Map.empty, Package.Name.Map.empty)
               ~f:(fun
                    (bins, dep_info)
                    ((pkg : Pkg.t), (cookie : Install_cookie.t))
                  ->
                 let bins =
                   Section.Map.Multi.find cookie.files Bin
                   |> List.fold_left ~init:bins ~f:(fun acc bin ->
                          Filename.Map.set acc (Path.basename bin) bin)
                 in
                 let dep_info =
                   let variables =
                     String.Map.superpose
                       (Pkg_info.variables pkg.info)
                       (String.Map.of_list_exn cookie.variables)
                   in
                   Package.Name.Map.add_exn dep_info pkg.info.name
                     (variables, pkg.paths)
                 in
                 (bins, dep_info)))
    in
    let deps =
      Package.Name.Map.add_exn deps pkg.info.name
        (Pkg_info.variables pkg.info, pkg.paths)
    in
    { Expander.paths = pkg.paths
    ; artifacts
    ; context
    ; deps
    ; version = pkg.info.version
    }

  let expand context (pkg : Pkg.t) action =
    let* expander = expander context pkg in
    let+ action =
      expand action ~expander >>| Action.chdir (Path.build pkg.paths.source_dir)
    in
    Action.Full.make ~sandbox:Sandbox_config.needs_sandboxing action
    |> Action_builder.return |> Action_builder.with_no_targets

  let build_command context (pkg : Pkg.t) =
    Option.map pkg.build_command ~f:(expand context pkg)

  let install_command context (pkg : Pkg.t) =
    Option.map pkg.install_command ~f:(expand context pkg)

  let exported_env (expander : Expander.t) (env : _ Env_update.t) =
    let value =
      Expander.expand_pform_gen expander env.value ~mode:Single
      |> Value.to_string ~dir:(Path.build expander.paths.source_dir)
    in
    { env with value }
end

type db = Lock_file.Pkg.t Package.Name.Map.t

module rec Resolve : sig
  val resolve : db -> Context_name.t -> Package.Name.t -> Pkg.t option Memo.t
end = struct
  open Resolve

  let resolve_impl ((db : db), ctx, (name : Package.Name.t)) =
    match Package.Name.Map.find db name with
    | None -> Memo.return None
    | Some
        { Lock_file.Pkg.build_command
        ; install_command
        ; deps
        ; info
        ; exported_env
        ; lock_dir
        } ->
      assert (Package.Name.equal name info.name);
      let* deps =
        Memo.parallel_map deps ~f:(fun name ->
            resolve db ctx name >>| function
            | Some pkg -> pkg
            | None -> User_error.raise [ Pp.text "invalid dependencies" ])
      in
      let id = Pkg.Id.gen () in
      let paths = Paths.make name ctx in
      let files_dir =
        Path.Source.relative lock_dir
          (sprintf "%s.files" (Package.Name.to_string info.name))
      in
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
      let+ expander = Action_expander.expander ctx t in
      t.exported_env <-
        List.map exported_env ~f:(Action_expander.exported_env expander);
      Some t

  let resolve =
    let module Input = struct
      type t = db * Context_name.t * Package.Name.t

      let equal = ( == )

      let hash = Tuple.T3.hash Poly.hash Context_name.hash Package.Name.hash

      let to_dyn = Dyn.opaque
    end in
    let memo =
      Memo.create "pkg-resolve"
        ~input:(module Input)
        ~human_readable_description:(fun (_db, _ctx, pkg) ->
          Pp.textf "- package %s" (Package.Name.to_string pkg))
        resolve_impl
    in
    fun db ctx name -> Memo.exec memo (db, ctx, name)
end

open Resolve

module Install_action = struct
  let installable_sections =
    Section.(Set.diff all (Set.of_list [ Misc; Libexec; Libexec_root ]))
    |> Section.Set.to_list

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
        ({ install_file
         ; config_file
         ; target_dir
         ; install_action = _
         ; package = _
         } as t) f g =
      { t with
        install_file = f install_file
      ; config_file = f config_file
      ; target_dir = g target_dir
      }

    let is_useful_to ~distribute:_ ~memoize = memoize

    let encode
        { install_file; config_file; target_dir; install_action; package } path
        target : Dune_lang.t =
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

    let prepare_copy_or_move ~install_file ~target_dir entry =
      let dst =
        let paths =
          let package =
            Path.basename install_file |> Filename.chop_extension
            |> Package.Name.of_string
          in
          let roots =
            Path.build target_dir
            |> Install.Section.Paths.Roots.opam_from_prefix
          in
          Install.Section.Paths.make ~package ~roots
        in
        Install.Entry.relative_installed_path entry ~paths
      in
      Path.mkdir_p (Path.parent_exn dst);
      dst

    let readdir path =
      match Path.Untracked.readdir_unsorted_with_kinds path with
      | Error _ -> ([], [])
      | Ok listing ->
        List.partition_map listing ~f:(fun (basename, kind) ->
            let path = Path.relative path basename in
            match kind with
            | S_DIR -> Right path
            | _ -> Left path)

    let rec collect paths acc =
      match paths with
      | [] -> acc
      | path :: paths ->
        let files, dirs = readdir path in
        let acc = List.rev_append files acc in
        collect (List.rev_append dirs paths) acc

    let skip path skip =
      List.iter skip ~f:(fun s -> assert (Path.equal path (Path.parent_exn s)));
      let files, dirs = readdir path in
      let dirs =
        List.filter_map dirs ~f:(fun path ->
            if List.mem skip path ~equal:Path.equal then None else Some path)
      in
      (files, dirs)

    let maybe_drop_sandbox_dir path =
      match Path.extract_build_context_dir_maybe_sandboxed path with
      | None -> path
      | Some (sandbox, source) ->
        let ctx =
          let name = Path.basename sandbox in
          Path.relative (Path.build Path.Build.root) name
        in
        Path.append_source ctx source

    let section_map_of_dir install_paths =
      let get = Install.Section.Paths.get install_paths in
      List.concat_map installable_sections ~f:(fun section ->
          let path = get section in
          let acc, dirs =
            match section with
            | Lib_root -> skip path [ get Toplevel; get Stublibs; get Lib ]
            | Share_root -> skip path [ get Share ]
            | _ -> ([], [ path ])
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
                     if Path.Permissions.(test execute perm) then section'
                     else section
                 in
                 (section, maybe_drop_sandbox_dir file)))
      |> Section.Map.of_list_multi

    let action
        { package; install_file; config_file; target_dir; install_action }
        ~ectx:_ ~eenv:_ =
      let open Fiber.O in
      let+ () = Fiber.return () in
      let files =
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
        let from_install_file =
          match Path.Untracked.exists install_file with
          | false -> Section.Map.empty
          | true ->
            let map =
              let install_dir = Path.parent_exn install_file in
              let install_entries = Install.load_install_file install_file in
              let by_src =
                List.rev_map install_entries
                  ~f:(fun (entry : _ Install.Entry.t) ->
                    ( Path.as_in_source_tree_exn entry.src
                      |> Path.append_source install_dir
                    , entry ))
                |> Path.Map.of_list_multi
              in
              let install_entries =
                Path.Map.to_list_map by_src ~f:(fun src entries ->
                    (* TODO set permissions *)
                    let maybe_set_executable section dst =
                      match Section.should_set_executable_bit section with
                      | false -> ()
                      | true ->
                        let permission =
                          let perm = (Path.Untracked.stat_exn dst).st_perm in
                          Path.Permissions.(add execute) perm
                        in
                        Path.chmod dst ~mode:permission
                    in
                    let exists = lazy (Path.Untracked.exists src) in
                    match entries with
                    | [] -> assert false
                    | [ entry ] ->
                      if entry.optional && (not @@ Lazy.force exists) then []
                      else
                        let dst =
                          prepare_copy_or_move ~install_file ~target_dir entry
                        in
                        Path.rename src dst;
                        maybe_set_executable entry.section dst;
                        [ (entry.section, dst) ]
                    | entry :: entries ->
                      let install_entries =
                        List.filter_map entries ~f:(fun entry ->
                            if entry.optional && (not @@ Lazy.force exists) then
                              None
                            else
                              let dst =
                                prepare_copy_or_move ~install_file ~target_dir
                                  entry
                              in
                              (* TODO hard link if possible *)
                              Io.copy_file ~src ~dst ();
                              maybe_set_executable entry.section dst;
                              Some (entry.section, dst))
                      in
                      if entry.optional && (not @@ Lazy.force exists) then
                        install_entries
                      else
                        let dst =
                          prepare_copy_or_move ~install_file ~target_dir entry
                        in
                        Path.rename src dst;
                        maybe_set_executable entry.section dst;
                        (entry.section, dst) :: install_entries)
              in
              List.concat install_entries
              |> List.rev_map ~f:(fun (section, file) ->
                     let file = maybe_drop_sandbox_dir file in
                     (section, file))
              |> Section.Map.of_list_multi
            in
            Path.unlink install_file;
            map
        in
        (* TODO we should make sure that overwrites aren't allowed *)
        Section.Map.union from_install_action from_install_file ~f:(fun _ x y ->
            Some (x @ y))
      in
      let variables =
        match Path.Untracked.exists config_file with
        | false -> []
        | true ->
          let config =
            Path.to_string config_file |> OpamFilename.of_string
            |> OpamFile.make |> OpamFile.Dot_config.read
          in
          OpamFile.Dot_config.bindings config
          |> List.map ~f:(fun (name, value) ->
                 (OpamVariable.to_string name, value))
      in
      let cookies = { Install_cookie.files; variables } in
      let cookie_file = Path.build @@ Paths.install_cookie' target_dir in
      cookie_file |> Path.parent_exn |> Path.mkdir_p;
      Install_cookie.dump cookie_file cookies
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
    end in
    Action.Extension (module M)
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

    let is_useful_to ~distribute:_ ~memoize = memoize

    let encode_loc f (loc, x) =
      Dune_lang.List
        (* TODO use something better for locs here *)
        [ Dune_lang.atom_or_quoted_string (Loc.to_file_colon_line loc); f x ]

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

    let action { target_dir; url = loc_url, url; checksum } ~ectx:_ ~eenv:_ =
      let open Fiber.O in
      let* () = Fiber.return () in
      let* res =
        let checksum = Option.map checksum ~f:snd in
        Dune_pkg.Fetch.fetch ~checksum ~target:(Path.build target_dir)
          (OpamUrl.of_string url)
      in
      match res with
      | Ok () -> Fiber.return ()
      | Error (Checksum_mismatch actual_checksum) -> (
        match checksum with
        | None ->
          User_error.raise ~loc:loc_url
            [ Pp.text "No checksum provided. It should be:"
            ; Checksum.pp actual_checksum
            ]
        | Some (loc, _) ->
          User_error.raise ~loc
            [ Pp.text "Invalid checksum, got"
            ; Dune_pkg.Checksum.pp actual_checksum
            ])
      | Error (Unavailable message) -> (
        let loc = loc_url in
        match message with
        | None -> User_error.raise ~loc [ Pp.text "Unknown fetch failure" ]
        | Some msg -> User_error.raise ~loc [ User_message.pp msg ])
  end

  let action ~url ~checksum ~target_dir =
    let module M = struct
      type path = Path.t

      type target = Path.Build.t

      module Spec = Spec

      let v = { Spec.target_dir; checksum; url }
    end in
    Action.Extension (module M)
end

module Copy_tree = struct
  module Spec = struct
    type ('path, 'target) t = 'path * 'target

    let name = "copy-tree"

    let version = 1

    let bimap (src, dst) f g = (f src, g dst)

    let is_useful_to ~distribute:_ ~memoize = memoize

    let encode (src, dst) dep target : Dune_lang.t =
      List [ Dune_lang.atom_or_quoted_string name; dep src; target dst ]

    let action (root, dst) ~ectx:_ ~eenv:_ =
      let open Fiber.O in
      let+ () = Fiber.return () in
      Install_action.Spec.collect [ root ] []
      |> List.iter ~f:(fun src ->
             let dst =
               Path.append_local (Path.build dst)
                 (Path.drop_prefix_exn src ~prefix:root)
             in
             let old_permissions =
               match Path.Untracked.stat dst with
               | Error _ -> None
               | Ok s ->
                 Path.unlink dst;
                 Some s.st_perm
             in
             Io.copy_file
               ~chmod:(fun default -> Option.value old_permissions ~default)
               ~src ~dst ())
  end

  let action ~src ~dst =
    let module M = struct
      type path = Path.t

      type target = Path.Build.t

      module Spec = Spec

      let v = (src, dst)
    end in
    Action.Extension (module M)
end

let add_env env action =
  Action_builder.With_targets.map action ~f:(Action.Full.add_env env)

let rule ?loc { Action_builder.With_targets.build; targets } =
  (* TODO this ignores the workspace file *)
  let build = Action_builder.map build ~f:(Action.Full.add_env Env.initial) in
  Rule.make ~info:(Rule.Info.of_loc_opt loc) ~targets build ~context:None
  |> Rules.Produce.rule

let gen_rules context_name (pkg : Pkg.t) =
  let* source_deps, copy_rules =
    match pkg.info.source with
    | None -> Memo.return (Dep.Set.empty, [])
    | Some (Fetch { url = (loc, _) as url; checksum }) ->
      let fetch =
        Fetch.action ~url ~target_dir:pkg.paths.target_dir ~checksum
        |> Action.Full.make |> Action_builder.With_targets.return
      in
      Memo.return
        (Dep.Set.of_files [ Path.build pkg.paths.source_dir ], [ (loc, fetch) ])
    | Some (External_copy (loc, source_root)) ->
      let source_root = Path.external_ source_root in
      let+ source_files, rules =
        Pkg.source_files pkg
        >>| Path.Local.Set.fold ~init:([], [])
              ~f:(fun file (source_files, rules) ->
                let src = Path.append_local source_root file in
                let dst = Path.Build.append_local pkg.paths.source_dir file in
                let copy = (loc, Action_builder.copy ~src ~dst) in
                (Path.build dst :: source_files, copy :: rules))
      in
      (Dep.Set.of_files source_files, rules)
  in
  let* () =
    Memo.parallel_iter copy_rules ~f:(fun (loc, copy) -> rule ~loc copy)
  in
  let* build_rule =
    let+ build_action =
      let install_action = Action_expander.install_command context_name pkg in
      let+ build_and_install =
        let* copy_action =
          Fs_memo.dir_exists (In_source_dir pkg.files_dir) >>| function
          | false -> []
          | true ->
            [ Copy_tree.action
                ~src:(Path.source pkg.files_dir)
                ~dst:pkg.paths.source_dir
              |> Action.Full.make |> Action_builder.With_targets.return
            ]
        in
        let* build_action =
          match Action_expander.build_command context_name pkg with
          | None -> Memo.return copy_action
          | Some build_command ->
            let+ build_command = build_command in
            copy_action @ [ build_command ]
        in
        match Action_expander.install_command context_name pkg with
        | None -> Memo.return build_action
        | Some install_action ->
          let+ install_action = install_action in
          let mkdir_install_dirs =
            let install_paths = Paths.install_paths pkg.paths in
            Install_action.installable_sections
            |> List.rev_map ~f:(fun section ->
                   Install.Section.Paths.get install_paths section
                   |> Path.as_in_build_dir_exn |> Action.mkdir)
            |> Action.progn |> Action.Full.make
            |> Action_builder.With_targets.return
          in
          build_action @ [ mkdir_install_dirs; install_action ]
      in
      let install_file_action =
        Install_action.action pkg.paths
          (match install_action with
          | None -> `No_install_action
          | Some _ -> `Has_install_action)
        |> Action.Full.make |> Action_builder.return
        |> Action_builder.with_no_targets
      in
      Action_builder.progn (build_and_install @ [ install_file_action ])
    in
    let deps = Dep.Set.union source_deps (Pkg.package_deps pkg) in
    let open Action_builder.With_targets.O in
    Action_builder.deps deps |> Action_builder.with_no_targets
    >>> add_env (Pkg.exported_env pkg) build_action
    |> Action_builder.With_targets.add_directories
         ~directory_targets:[ pkg.paths.target_dir ]
  in
  rule build_rule

let setup_package_rules context ~dir ~pkg_name :
    Build_config.gen_rules_result Memo.t =
  match Package.Name.of_string_user_error (Loc.none, pkg_name) with
  | Error m -> raise (User_error.E m)
  | Ok name -> (
    let* db = Lock_file.get () in
    resolve db.packages context name >>| function
    | None ->
      User_error.raise
        [ Pp.textf "unknown package %S" (Package.Name.to_string name) ]
    | Some pkg ->
      let paths = Paths.make name context in
      let rules =
        { Build_config.Rules.directory_targets =
            (let target_dir = paths.target_dir in
             Path.Build.Map.singleton target_dir Loc.none)
        ; build_dir_only_sub_dirs =
            Build_config.Rules.Build_only_sub_dirs.singleton ~dir
              Subdir_set.empty
        ; rules = Rules.collect_unit (fun () -> gen_rules context pkg)
        }
      in
      Build_config.Rules rules)
