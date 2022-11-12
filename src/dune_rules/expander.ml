open Import
open Action_builder.O

module Expanding_what = struct
  type t =
    | Nothing_special
    | Deps_like_field
    | User_action of Path.Build.t Targets_spec.t
    | User_action_without_targets of { what : string }
end

module Deps = struct
  module T = struct
    type 'a t =
      | Without of 'a Memo.t
      | With of 'a Action_builder.t

    let return x = Without (Memo.return x)

    let map t ~f =
      match t with
      | Without t -> Without (Memo.map t ~f)
      | With t -> With (Action_builder.map t ~f)

    let both a b =
      match (a, b) with
      | Without a, Without b -> Without (Memo.both a b)
      | With a, With b -> With (Action_builder.both a b)
      | Without a, With b ->
        With (Action_builder.both (Action_builder.of_memo a) b)
      | With a, Without b ->
        With (Action_builder.both a (Action_builder.of_memo b))
  end

  include T
  include Applicative.Make (T)

  let action_builder = function
    | Without x -> Action_builder.of_memo x
    | With x -> x
end

type value = Value.t list Deps.t

type t =
  { dir : Path.Build.t
  ; env : Env.t
  ; local_env : string Action_builder.t Env.Var.Map.t
  ; lib_artifacts : Artifacts.Public_libs.t
  ; lib_artifacts_host : Artifacts.Public_libs.t
  ; bin_artifacts_host : Artifacts.Bin.t
  ; bindings : value Pform.Map.t
  ; scope : Scope.t
  ; scope_host : Scope.t
  ; c_compiler : string
  ; context : Context.t
  ; lookup_artifacts :
      (dir:Path.Build.t -> Ml_sources.Artifacts.t Memo.t) option
  ; foreign_flags :
         dir:Path.Build.t
      -> string list Action_builder.t Foreign_language.Dict.t Memo.t
  ; sites : Sites.t
  ; expanding_what : Expanding_what.t
  }

let scope t = t.scope

let artifacts t = t.bin_artifacts_host

let dir t = t.dir

let context t = t.context

let set_foreign_flags t ~f:foreign_flags = { t with foreign_flags }

let set_local_env_var t ~var ~value =
  { t with local_env = Env.Var.Map.set t.local_env var value }

let set_dir t ~dir = { t with dir }

let set_scope t ~scope = { t with scope }

let set_bin_artifacts t ~bin_artifacts_host = { t with bin_artifacts_host }

let set_lookup_ml_sources t ~f = { t with lookup_artifacts = Some f }

let set_expanding_what t x = { t with expanding_what = x }

let map_exe t p =
  match t.expanding_what with
  | Deps_like_field -> p
  | Nothing_special | User_action _ | User_action_without_targets _ ->
    Context.map_exe t.context p

let extend_env t ~env =
  (* [t.local_env] has precedence over [t.env], so we cannot extend [env] if
     there are already local bidings.. *)
  assert (Env.Var.Map.is_empty t.local_env);
  { t with env = Env.extend_env t.env env }

let add_bindings_full t ~bindings =
  { t with bindings = Pform.Map.superpose t.bindings bindings }

let add_bindings t ~bindings =
  add_bindings_full t
    ~bindings:
      (Pform.Map.map bindings ~f:(fun v -> Deps.Without (Memo.return v)))

let path p = [ Value.Path p ]

let string s = [ Value.String s ]

let strings l = Value.L.strings l

let dep p =
  let+ () = Action_builder.path p in
  [ Value.Path p ]

let expand_version { scope; _ } ~source s =
  let value_from_version = function
    | None -> [ Value.String "" ]
    | Some s -> [ String s ]
  in
  let project = Scope.project scope in
  match
    Package.Name.Map.find
      (Dune_project.packages project)
      (Package.Name.of_string s)
  with
  | Some p -> Memo.return (value_from_version p.version)
  | None when Dune_project.dune_version project < (2, 9) ->
    User_error.raise ~loc:source.Dune_lang.Template.Pform.loc
      [ Pp.textf "Package %S doesn't exist in the current project." s ]
      ~hints:
        [ Pp.text
            "If you want to refer to an installed package, or more generally \
             to a package from another project, you need at least (lang dune \
             2.9)."
        ]
  | None -> (
    let libname = Lib_name.of_string s in
    let pkgname = Lib_name.package_name libname in
    if not (String.equal (Package.Name.to_string pkgname) s) then
      User_error.raise ~loc:source.Dune_lang.Template.Pform.loc
        [ Pp.textf
            "Library names are not allowed in this position. Only package \
             names are allowed"
        ];
    let open Memo.O in
    Lib.DB.find (Scope.libs scope) libname >>| function
    | Some lib -> value_from_version (Lib_info.version (Lib.info lib))
    | None ->
      User_error.raise ~loc:source.Dune_lang.Template.Pform.loc
        [ Pp.textf
            "Package %S doesn't exist in the current project and isn't \
             installed either."
            s
        ])

let isn't_allowed_in_this_position ~source =
  let exn =
    User_error.make ~loc:source.Dune_lang.Template.Pform.loc
      [ Pp.textf "%s isn't allowed in this position."
          (Dune_lang.Template.Pform.describe source)
      ]
  in
  raise (User_error.E exn)

let expand_artifact ~source t a s =
  match t.lookup_artifacts with
  | None -> isn't_allowed_in_this_position ~source
  | Some lookup -> (
    let path = Path.Build.relative t.dir s in
    let name = Path.Build.basename path in
    let dir = Path.Build.parent_exn path in
    let does_not_exist ~loc ~what name =
      User_error.raise ~loc [ Pp.textf "%s %s does not exist." what name ]
    in
    let* artifacts = Action_builder.of_memo (lookup ~dir) in
    match a with
    | Pform.Artifact.Mod kind -> (
      let name =
        Module_name.of_string_allow_invalid
          (Dune_lang.Template.Pform.loc source, name)
      in
      match Ml_sources.Artifacts.lookup_module artifacts name with
      | None ->
        does_not_exist
          ~loc:(Dune_lang.Template.Pform.loc source)
          ~what:"Module"
          (Module_name.to_string name)
      | Some (t, m) -> (
        match Obj_dir.Module.cm_file t m ~kind:(Ocaml kind) with
        | None -> Action_builder.return [ Value.String "" ]
        | Some path -> dep (Path.build path)))
    | Lib mode -> (
      let name =
        Lib_name.parse_string_exn (Dune_lang.Template.Pform.loc source, name)
      in
      match Ml_sources.Artifacts.lookup_library artifacts name with
      | None ->
        does_not_exist
          ~loc:(Dune_lang.Template.Pform.loc source)
          ~what:"Library" (Lib_name.to_string name)
      | Some lib ->
        let archives = Mode.Dict.get (Lib_info.archives lib) mode in
        Action_builder.all
          (List.map archives ~f:(fun fn ->
               let fn = Path.build fn in
               let+ () = Action_builder.path fn in
               Value.Path fn))))

let cc t =
  Memo.map (t.foreign_flags ~dir:t.dir) ~f:(fun cc ->
      Foreign_language.Dict.map cc ~f:(fun cc ->
          let+ flags = cc in
          strings (t.c_compiler :: flags)))

let get_prog = function
  | Ok p -> path p
  | Error err -> Action.Prog.Not_found.raise err

let c_compiler_and_flags (context : Context.t) =
  Ocaml_config.c_compiler context.ocaml_config
  :: Ocaml_config.ocamlc_cflags context.ocaml_config

let relative ~source d s =
  Path.build
    (Path.Build.relative ~error_loc:(Dune_lang.Template.Pform.loc source) d s)

type nonrec expansion_result =
  | Direct of value
  | Need_full_expander of (t -> value)

let static v = Direct (Without (Memo.return v))

let[@inline never] invalid_use_of_target_variable t
    ~(source : Dune_lang.Template.Pform.t) ~var_multiplicity =
  match t.expanding_what with
  | Nothing_special | Deps_like_field -> isn't_allowed_in_this_position ~source
  | User_action_without_targets { what } ->
    User_error.raise ~loc:source.loc
      [ Pp.textf "You cannot use %s in %s."
          (Dune_lang.Template.Pform.describe source)
          what
      ]
  | User_action targets -> (
    match targets with
    | Infer ->
      User_error.raise ~loc:source.loc
        [ Pp.textf "You cannot use %s with inferred rules."
            (Dune_lang.Template.Pform.describe source)
        ]
    | Static { targets = _; multiplicity } ->
      assert (multiplicity <> var_multiplicity);
      Targets_spec.Multiplicity.check_variable_matches_field ~loc:source.loc
        ~field:multiplicity ~variable:var_multiplicity;
      assert false)

let expand_read_macro ~dir ~source s ~read ~pack =
  let path = relative ~source dir s in
  let read =
    let open Memo.O in
    let+ x = Build_system.read_file path ~f:read in
    pack x
  in
  Need_full_expander
    (fun t ->
      if Dune_project.dune_version (Scope.project t.scope) >= (3, 0) then
        Without read
      else
        (* To prevent it from working in certain position before Dune 3.0. It'd
           be nice if we could invite the user to upgrade to (lang dune 3.0),
           but this is a bigger refactoring. *)
        With (Action_builder.of_memo read))

let expand_pform_gen ~(context : Context.t) ~bindings ~dir ~source
    (pform : Pform.t) : expansion_result =
  match Pform.Map.find bindings pform with
  | Some x -> Direct x
  | None -> (
    match pform with
    | Var var -> (
      match var with
      | Nothing -> static []
      | User_var _
      | Deps
      | Input_file
      | Library_name
      | Impl_files
      | Intf_files
      | Inline_tests
      | Test
      | Corrected_suffix ->
        (* These would be part of [bindings] *)
        isn't_allowed_in_this_position ~source
      | First_dep ->
        (* This case is for %{<} which was only allowed inside jbuild files *)
        assert false
      | Target ->
        Need_full_expander
          (fun t ->
            invalid_use_of_target_variable t ~source ~var_multiplicity:One)
      | Targets ->
        Need_full_expander
          (fun t ->
            invalid_use_of_target_variable t ~source ~var_multiplicity:Multiple)
      | Ocaml -> static (get_prog context.ocaml)
      | Ocamlc -> static (path context.ocamlc)
      | Ocamlopt -> static (get_prog context.ocamlopt)
      | Make ->
        let open Memo.O in
        Direct
          (Without
             (Context.make context >>| function
              | Some p -> path p
              | None ->
                Utils.program_not_found ~context:context.name
                  ~loc:(Some (Dune_lang.Template.Pform.loc source))
                  "make"))
      | Cpp -> static (strings (c_compiler_and_flags context @ [ "-E" ]))
      | Pa_cpp ->
        static
          (strings
             (c_compiler_and_flags context
             @ [ "-undef"; "-traditional"; "-x"; "c"; "-E" ]))
      | Arch_sixtyfour ->
        static (string (string_of_bool context.arch_sixtyfour))
      | Ocaml_bin_dir -> static [ Dir context.ocaml_bin ]
      | Ocaml_version ->
        static (string (Ocaml_config.version_string context.ocaml_config))
      | Ocaml_stdlib_dir -> static (string (Path.to_string context.stdlib_dir))
      | Dev_null -> static (string (Path.to_string Config.dev_null))
      | Ext_obj -> static (string context.lib_config.ext_obj)
      | Ext_asm -> static (string (Ocaml_config.ext_asm context.ocaml_config))
      | Ext_lib -> static (string context.lib_config.ext_lib)
      | Ext_dll -> static (string context.lib_config.ext_dll)
      | Ext_exe -> static (string (Ocaml_config.ext_exe context.ocaml_config))
      | Ext_plugin ->
        static
          (string
             (Mode.plugin_ext
                (if Ocaml_config.natdynlink_supported context.ocaml_config then
                 Mode.Native
                else Mode.Byte)))
      | Profile -> static (string (Profile.to_string context.profile))
      | Workspace_root -> static [ Value.Dir (Path.build context.build_dir) ]
      | Context_name -> static (string (Context_name.to_string context.name))
      | Os_type ->
        static
          (string
             (Ocaml_config.Os_type.to_string
                (Ocaml_config.os_type context.ocaml_config)))
      | Architecture ->
        static (string (Ocaml_config.architecture context.ocaml_config))
      | System -> static (string (Ocaml_config.system context.ocaml_config))
      | Model -> static (string (Ocaml_config.model context.ocaml_config))
      | Ignoring_promoted_rules ->
        static (string (string_of_bool !Clflags.ignore_promoted_rules))
      | Project_root ->
        Need_full_expander
          (fun t ->
            Without
              (Memo.return [ Value.Dir (Path.build (Scope.root t.scope)) ]))
      | Cc ->
        Need_full_expander
          (fun t ->
            With
              (let* cc = Action_builder.of_memo (cc t) in
               cc.c))
      | Cxx ->
        Need_full_expander
          (fun t ->
            With
              (let* cc = Action_builder.of_memo (cc t) in
               cc.cxx))
      | Ccomp_type ->
        static
          (string
             (Ocaml_config.Ccomp_type.to_string context.lib_config.ccomp_type))
      | Toolchain ->
        static
          (string
             (match context.findlib_toolchain with
             | Some toolchain -> Context_name.to_string toolchain
             | None ->
               let loc = Dune_lang.Template.Pform.loc source in
               User_error.raise ~loc
                 [ Pp.text "No toolchain defined for this context" ])))
    | Macro (macro, s) -> (
      match macro with
      | Ocaml_config ->
        static
          (match Ocaml_config.by_name context.ocaml_config s with
          | None ->
            User_error.raise
              ~loc:(Dune_lang.Template.Pform.loc source)
              [ Pp.textf "Unknown ocaml configuration variable %S" s ]
          | Some v -> (
            match v with
            | Bool x -> string (string_of_bool x)
            | Int x -> string (string_of_int x)
            | String x -> string x
            | Words x -> strings x
            | Prog_and_args x -> strings (x.prog :: x.args)))
      | Env ->
        Need_full_expander
          (fun t ->
            match String.rsplit2 s ~on:'=' with
            | None ->
              User_error.raise ~loc:source.Dune_lang.Template.Pform.loc
                [ Pp.textf "%s must always come with a default value."
                    (Dune_lang.Template.Pform.describe source)
                ]
                ~hints:[ Pp.text "the syntax is %{env:VAR=DEFAULT-VALUE}" ]
            | Some (var, default) -> (
              match Env.Var.Map.find t.local_env var with
              | Some v ->
                With
                  (let+ v = v in
                   string v)
              | None ->
                Without
                  (Memo.return
                     (string (Option.value ~default (Env.get t.env var))))))
      | Version ->
        Need_full_expander (fun t -> Without (expand_version t ~source s))
      | Artifact a ->
        Need_full_expander (fun t -> With (expand_artifact ~source t a s))
      | Path_no_dep ->
        (* This case is for %{path-no-dep:...} which was only allowed inside
           jbuild files *)
        assert false
      | Exe ->
        Need_full_expander
          (fun t -> With (dep (map_exe t (relative ~source t.dir s))))
      | Dep ->
        Need_full_expander (fun t -> With (dep (relative ~source t.dir s)))
      | Bin ->
        Need_full_expander
          (fun t ->
            With
              (let* prog =
                 Action_builder.of_memo
                   (Artifacts.Bin.binary
                      ~loc:(Some (Dune_lang.Template.Pform.loc source))
                      t.bin_artifacts_host s)
               in
               dep (Action.Prog.ok_exn prog)))
      | Lib { lib_exec; lib_private } ->
        Need_full_expander
          (fun t ->
            With
              (let lib, file =
                 let loc = Dune_lang.Template.Pform.loc source in
                 match String.lsplit2 s ~on:':' with
                 | None ->
                   User_error.raise ~loc
                     [ Pp.textf "invalid %%{lib:...} form: %s" s ]
                 | Some (lib, f) -> (Lib_name.parse_string_exn (loc, lib), f)
               in
               let scope = if lib_exec then t.scope_host else t.scope in
               let p =
                 let open Resolve.Memo.O in
                 if lib_private then
                   let* lib =
                     Lib.DB.resolve (Scope.libs scope)
                       (Dune_lang.Template.Pform.loc source, lib)
                   in
                   let current_project = Scope.project t.scope
                   and referenced_project =
                     Lib.info lib |> Lib_info.status |> Lib_info.Status.project
                   in
                   if
                     Option.equal Dune_project.equal (Some current_project)
                       referenced_project
                   then
                     Resolve.Memo.return
                       (Path.relative (Lib_info.src_dir (Lib.info lib)) file)
                   else
                     Resolve.Memo.fail
                       (User_error.make
                          ~loc:(Dune_lang.Template.Pform.loc source)
                          [ Pp.textf
                              "The variable \"lib%s-private\" can only refer \
                               to libraries within the same project. The \
                               current project's name is %S, but the reference \
                               is to %s."
                              (if lib_exec then "exec" else "")
                              (Dune_project.Name.to_string_hum
                                 (Dune_project.name current_project))
                              (match referenced_project with
                              | None -> "an external library"
                              | Some project ->
                                Dune_project.name project
                                |> Dune_project.Name.to_string_hum
                                |> String.quoted)
                          ])
                 else
                   let artifacts =
                     if lib_exec then t.lib_artifacts_host else t.lib_artifacts
                   in
                   Artifacts.Public_libs.file_of_lib artifacts
                     ~loc:(Dune_lang.Template.Pform.loc source)
                     ~lib ~file
               in
               let p =
                 let open Memo.O in
                 Resolve.Memo.peek p >>| function
                 | Ok p -> (
                   match file with
                   | "" | "." ->
                     let lang_version =
                       Dune_project.dune_version (Scope.project t.scope)
                     in
                     if lang_version < (3, 0) then
                       Action_builder.return [ Value.Path p ]
                     else
                       User_error.raise
                         ~loc:(Dune_lang.Template.Pform.loc source)
                         [ Pp.textf
                             "The form %%{%s:<libname>:%s} is no longer \
                              supported since version 3.0 of the Dune \
                              language."
                             (if lib_private then "lib-private" else "lib")
                             file
                         ]
                         ~hints:
                           [ (match Lib_name.to_string lib with
                             | "ctypes" ->
                               Pp.text
                                 "Did you know that Dune 3.0 supports ctypes \
                                  natively? See the manual for more details."
                             | _ ->
                               Pp.textf
                                 "If you are trying to use this form to \
                                  include a directory, you should instead use \
                                  (foreign_stubs (include_dirs (lib %s))). See \
                                  the manual for more details."
                                 (Lib_name.to_string lib))
                           ]
                   | _ ->
                     if
                       (not lib_exec) || (not Sys.win32)
                       || Filename.extension s = ".exe"
                     then dep p
                     else
                       let p_exe = Path.extend_basename p ~suffix:".exe" in
                       Action_builder.if_file_exists p_exe ~then_:(dep p_exe)
                         ~else_:(dep p))
                 | Error () ->
                   let p =
                     if lib_private then
                       Resolve.Memo.map p ~f:(fun _ -> assert false)
                     else
                       let open Resolve.Memo.O in
                       let* available =
                         Resolve.Memo.lift_memo
                           (Lib.DB.available (Scope.libs scope) lib)
                       in
                       match available with
                       | false -> p >>| fun _ -> assert false
                       | true ->
                         Resolve.Memo.fail
                           (User_error.make
                              ~loc:(Dune_lang.Template.Pform.loc source)
                              [ Pp.textf
                                  "The library %S is not public. The variable \
                                   \"lib%s\" expands to the file's \
                                   installation path which is not defined for \
                                   private libraries."
                                  (Lib_name.to_string lib)
                                  (if lib_exec then "exec" else "")
                              ])
                   in
                   Resolve.Memo.read p
               in
               Action_builder.of_memo_join p))
      | Lib_available ->
        Need_full_expander
          (fun t ->
            Without
              (let lib =
                 Lib_name.parse_string_exn
                   (Dune_lang.Template.Pform.loc source, s)
               in
               let open Memo.O in
               let+ available = Lib.DB.available (Scope.libs t.scope) lib in
               available |> string_of_bool |> string))
      | Bin_available ->
        Need_full_expander
          (fun t ->
            Without
              (let open Memo.O in
              let+ b = Artifacts.Bin.binary_available t.bin_artifacts_host s in
              b |> string_of_bool |> string))
      | Read -> expand_read_macro ~dir ~source s ~read:Io.read_file ~pack:string
      | Read_lines ->
        expand_read_macro ~dir ~source s ~read:Io.lines_of_file ~pack:strings
      | Read_strings ->
        expand_read_macro ~dir ~source s ~read:Io.lines_of_file
          ~pack:(fun lines ->
            strings
              (List.map lines ~f:(fun line ->
                   match Scanf.unescaped line with
                   | Error () ->
                     User_error.raise
                       ~loc:(Loc.in_file (relative ~source dir s))
                       [ Pp.textf
                           "This file must be a list of lines escaped using \
                            OCaml's conventions"
                       ]
                   | Ok s -> s)))
      | Coq_config ->
        Need_full_expander
          (fun t ->
            Without
              (let open Memo.O in
              let* bin =
                Artifacts.Bin.binary t.bin_artifacts_host ~loc:None "coqc"
              in
              match bin with
              | Ok bin -> (
                let+ t = Coq_config.make ~bin in
                match Coq_config.by_name t s with
                | None ->
                  User_error.raise
                    ~loc:(Dune_lang.Template.Pform.loc source)
                    [ Pp.textf "Unknown Coq configuration variable %S" s ]
                | Some v -> (
                  match v with
                  | `Int x -> string (string_of_int x)
                  | `String x -> string x
                  | `Path x -> Value.L.paths [ x ]))
              | Error _ -> User_error.raise Pp.[ textf "coqc not found." ]))))

(* Make sure to delay exceptions *)
let expand_pform_gen ~context ~bindings ~dir ~source pform =
  match expand_pform_gen ~context ~bindings ~source ~dir pform with
  | exception (User_error.E _ as exn) ->
    Direct
      (Without
         (let open Memo.O in
         let+ () = Memo.return () in
         reraise exn))
  | Direct _ as x -> x
  | Need_full_expander f ->
    Need_full_expander
      (fun t ->
        try f t
        with User_error.E _ as exn ->
          Without
            (let open Memo.O in
            let+ () = Memo.return () in
            reraise exn))

let describe_source ~source =
  Pp.textf "%s at %s"
    (Dune_lang.Template.Pform.to_string source)
    (Loc.to_file_colon_line source.loc)

let expand_pform t ~source pform =
  Action_builder.push_stack_frame
    (fun () ->
      match
        match
          expand_pform_gen ~context:t.context ~bindings:t.bindings ~dir:t.dir
            ~source pform
        with
        | Direct v -> v
        | Need_full_expander f -> f t
      with
      | With x -> x
      | Without x -> Action_builder.of_memo x)
    ~human_readable_description:(fun () -> describe_source ~source)

let expand_pform_no_deps t ~source pform =
  Memo.push_stack_frame
    (fun () ->
      match
        match
          expand_pform_gen ~context:t.context ~bindings:t.bindings ~dir:t.dir
            ~source pform
        with
        | Direct v -> v
        | Need_full_expander f -> f t
      with
      | With _ -> isn't_allowed_in_this_position ~source
      | Without x -> x)
    ~human_readable_description:(fun () -> describe_source ~source)

let expand t ~mode template =
  Action_builder.Expander.expand ~dir:(Path.build t.dir) ~mode template
    ~f:(expand_pform t)

let make ~scope ~scope_host ~(context : Context.t) ~lib_artifacts
    ~lib_artifacts_host ~bin_artifacts_host =
  let open Memo.O in
  let+ sites = Sites.create context in
  { dir = context.build_dir
  ; env = context.env
  ; local_env = Env.Var.Map.empty
  ; bindings = Pform.Map.empty
  ; scope
  ; scope_host
  ; lib_artifacts
  ; lib_artifacts_host
  ; bin_artifacts_host
  ; c_compiler = Ocaml_config.c_compiler context.ocaml_config
  ; context
  ; lookup_artifacts = None
  ; foreign_flags =
      (fun ~dir ->
        Code_error.raise "foreign flags expander is not set"
          [ ("dir", Path.Build.to_dyn dir) ])
  ; sites
  ; expanding_what = Nothing_special
  }

let expand_path t sw =
  let+ v = expand t ~mode:Single sw in
  Value.to_path v ~error_loc:(String_with_vars.loc sw) ~dir:(Path.build t.dir)

let expand_str t sw =
  let+ v = expand t ~mode:Single sw in
  Value.to_string v ~dir:(Path.build t.dir)

module No_deps = struct
  open Memo.O

  let expand_pform = expand_pform_no_deps

  let expand t ~mode sw =
    String_with_vars.expand ~dir:(Path.build t.dir) ~mode sw ~f:(expand_pform t)

  let expand_path t sw =
    let+ v = expand t ~mode:Single sw in
    Value.to_path v ~error_loc:(String_with_vars.loc sw) ~dir:(Path.build t.dir)

  let expand_str t sw =
    let+ v = expand t ~mode:Single sw in
    Value.to_string v ~dir:(Path.build t.dir)
end

module With_deps_if_necessary = struct
  open Deps.O
  module E = String_with_vars.Make_expander (Deps)

  let expand_pform t ~source pform : _ Deps.t =
    match
      match
        expand_pform_gen ~context:t.context ~bindings:t.bindings ~dir:t.dir
          ~source pform
      with
      | Direct v -> v
      | Need_full_expander f -> f t
    with
    | Without t ->
      Without
        (Memo.push_stack_frame
           (fun () -> t)
           ~human_readable_description:(fun () -> describe_source ~source))
    | With t ->
      With
        (Action_builder.push_stack_frame
           (fun () -> t)
           ~human_readable_description:(fun () -> describe_source ~source))

  let expand t ~mode sw =
    E.expand ~dir:(Path.build t.dir) ~mode sw ~f:(expand_pform t)

  let expand_path t sw =
    let+ vs = expand t ~mode:Many sw in
    List.map vs ~f:(fun v ->
        Value.to_path_in_build_or_external v
          ~error_loc:(String_with_vars.loc sw) ~dir:t.dir)

  let expand_str t sw =
    let+ v = expand t ~mode:Single sw in
    Value.to_string v ~dir:(Path.build t.dir)
end

module With_reduced_var_set = struct
  open Memo.O

  let expand_pform_opt ~context ~bindings ~dir ~source pform =
    let open Memo.O in
    Memo.push_stack_frame
      (fun () ->
        match expand_pform_gen ~context ~bindings ~dir ~source pform with
        | Need_full_expander _ | Direct (With _) -> Memo.return None
        | Direct (Without x) -> x >>| Option.some)
      ~human_readable_description:(fun () -> describe_source ~source)

  let expand_pform ~context ~bindings ~dir ~source pform =
    expand_pform_opt ~context ~bindings ~dir ~source pform >>| function
    | Some v -> v
    | None -> isn't_allowed_in_this_position ~source

  let expand_str ~context ~dir sw =
    let+ v =
      String_with_vars.expand ~dir:(Path.build dir) ~mode:Single sw
        ~f:(expand_pform ~context ~bindings:Pform.Map.empty ~dir)
    in
    Value.to_string v ~dir:(Path.build dir)

  let expand_str_partial ~context ~dir sw =
    String_with_vars.expand_as_much_as_possible ~dir:(Path.build dir) sw
      ~f:(expand_pform_opt ~context ~bindings:Pform.Map.empty ~dir)

  let eval_blang ~context ~dir blang =
    Blang.eval
      ~f:(expand_pform ~context ~bindings:Pform.Map.empty ~dir)
      ~dir:(Path.build dir) blang
end

let expand_and_eval_set t set ~standard =
  let dir = Path.build (dir t) in
  let+ standard =
    if Ordered_set_lang.Unexpanded.has_special_forms set then standard
    else Action_builder.return []
  and+ set = Ordered_set_lang.Unexpanded.expand set ~dir ~f:(expand_pform t) in
  Ordered_set_lang.eval set ~standard ~eq:String.equal ~parse:(fun ~loc:_ s ->
      s)

let eval_blang t blang =
  Blang.eval ~f:(No_deps.expand_pform t) ~dir:(Path.build t.dir) blang

let expand_lock ~base expander (Locks.Lock sw) =
  let open Memo.O in
  match base with
  | `Of_expander -> No_deps.expand_path expander sw
  | `This base ->
    let+ str = No_deps.expand_str expander sw in
    Path.relative base str

let expand_locks ~base expander locks =
  Memo.List.map locks ~f:(expand_lock ~base expander)

let sites t = t.sites
