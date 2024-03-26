open Import
open Action_builder.O
open Expander0

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
    ;;

    let both a b =
      match a, b with
      | Without a, Without b -> Without (Memo.both a b)
      | With a, With b -> With (Action_builder.both a b)
      | Without a, With b -> With (Action_builder.both (Action_builder.of_memo a) b)
      | With a, Without b -> With (Action_builder.both a (Action_builder.of_memo b))
    ;;
  end

  include T
  include Applicative.Make (T)

  let action_builder = function
    | Without x -> Action_builder.of_memo x
    | With x -> x
  ;;
end

type value = Value.t list Deps.t

let lookup_artifacts = Fdecl.create Dyn.opaque

type t =
  { dir : Path.Build.t
  ; env : Env.t Memo.t
  ; local_env : string Action_builder.t Env.Var.Map.t
  ; public_libs : Lib.DB.t Memo.t
  ; public_libs_host : Lib.DB.t Memo.t
  ; artifacts_host : Artifacts.t Memo.t
  ; bindings : value Pform.Map.t
  ; scope : Scope.t Memo.t
  ; scope_host : Scope.t Memo.t
  ; context : Context.t
  ; expanding_what : Expanding_what.t
  ; project : Dune_project.t
  }

let artifacts t = t.artifacts_host
let dir t = t.dir
let project t = t.project
let context t = Context.name t.context

let set_local_env_var t ~var ~value =
  { t with local_env = Env.Var.Map.set t.local_env var value }
;;

let set_scope t ~dir ~project ~scope ~scope_host =
  { t with dir; project; scope; scope_host }
;;

let set_artifacts t ~artifacts_host = { t with artifacts_host }
let set_expanding_what t x = { t with expanding_what = x }

let map_exe t p =
  match t.expanding_what with
  | Deps_like_field -> p
  | Nothing_special | User_action _ | User_action_without_targets _ ->
    Context.map_exe t.context p
;;

let extend_env t ~env =
  (* [t.local_env] has precedence over [t.env], so we cannot extend [env] if
     there are already local bindings.. *)
  let env =
    Memo.Lazy.create (fun () ->
      let open Memo.O in
      let+ env = env
      and+ base = t.env in
      Env.extend_env base env)
    |> Memo.Lazy.force
  in
  assert (Env.Var.Map.is_empty t.local_env);
  { t with env }
;;

let add_bindings_full t ~bindings =
  { t with bindings = Pform.Map.superpose bindings t.bindings }
;;

let add_bindings t ~bindings =
  add_bindings_full
    t
    ~bindings:(Pform.Map.map bindings ~f:(fun v -> Deps.Without (Memo.return v)))
;;

let path p = [ Value.Path p ]
let string s = [ Value.String s ]
let strings l = Value.L.strings l

let dep p =
  let+ () = Action_builder.path p in
  [ Value.Path p ]
;;

let expand_version { scope; _ } ~(source : Dune_lang.Template.Pform.t) s =
  let value_from_version = function
    | None -> [ Value.String "" ]
    | Some s -> [ String (Package_version.to_string s) ]
  in
  let open Memo.O in
  let* scope = scope in
  let project = Scope.project scope in
  match
    let name = Package.Name.of_string s in
    let packages =
      (* CR-rgrinberg: craziness to preserve buggy behavior people are relying
         on at the moment *)
      Dune_project.including_hidden_packages project
    in
    Package.Name.Map.find packages name
  with
  | Some p -> Memo.return (value_from_version (Package.version p))
  | None when Dune_project.dune_version project < (2, 9) ->
    User_error.raise
      ~loc:source.loc
      [ Pp.textf "Package %S doesn't exist in the current project." s ]
      ~hints:
        [ Pp.text
            "If you want to refer to an installed package, or more generally to a \
             package from another project, you need at least (lang dune 2.9)."
        ]
  | None ->
    let libname = Lib_name.of_string s in
    let pkgname = Lib_name.package_name libname in
    if not (String.equal (Package.Name.to_string pkgname) s)
    then
      User_error.raise
        ~loc:source.loc
        [ Pp.textf
            "Library names are not allowed in this position. Only package names are \
             allowed"
        ];
    let open Memo.O in
    Lib.DB.find (Scope.libs scope) libname
    >>| (function
     | Some lib -> value_from_version (Lib_info.version (Lib.info lib))
     | None ->
       User_error.raise
         ~loc:source.loc
         [ Pp.textf
             "Package %S doesn't exist in the current project and isn't installed either."
             s
         ])
;;

let expand_artifact ~source t artifact arg =
  let path = Path.Build.relative t.dir arg in
  let loc = Dune_lang.Template.Pform.loc source in
  let name = Path.Build.basename path in
  let dir = Path.Build.parent_exn path in
  if Path.Build.is_root dir
  then User_error.raise ~loc [ Pp.text "cannot escape the workspace root directory" ];
  let does_not_exist ~what name =
    User_error.raise ~loc [ Pp.textf "%s %s does not exist." what name ]
  in
  let* artifacts =
    let lookup = Fdecl.get lookup_artifacts in
    Action_builder.of_memo (lookup ~dir)
  in
  match artifact with
  | Pform.Artifact.Mod kind ->
    let name =
      Module_name.of_string_allow_invalid (Dune_lang.Template.Pform.loc source, name)
    in
    (match Artifacts_obj.lookup_module artifacts name with
     | None -> does_not_exist ~what:"Module" (Module_name.to_string name)
     | Some (t, m) ->
       (match Obj_dir.Module.cm_file t m ~kind:(Ocaml kind) with
        | None -> Action_builder.return [ Value.String "" ]
        | Some path -> dep (Path.build path)))
  | Lib mode ->
    let name = Lib_name.parse_string_exn (Dune_lang.Template.Pform.loc source, name) in
    (match Artifacts_obj.lookup_library artifacts name with
     | None -> does_not_exist ~what:"Library" (Lib_name.to_string name)
     | Some lib ->
       Mode.Dict.get (Lib_info.archives lib) mode
       |> Action_builder.List.map ~f:(fun fn ->
         let fn = Path.build fn in
         let+ () = Action_builder.path fn in
         Value.Path fn))
;;

let foreign_flags = Fdecl.create Dyn.opaque

let cc t =
  let make (language : Foreign_language.t) =
    let+ cc =
      let* cc = Action_builder.of_memo @@ Fdecl.get foreign_flags ~dir:t.dir in
      Foreign_language.Dict.get cc language
    and+ c_compiler =
      let+ ocaml = Action_builder.of_memo @@ Context.ocaml t.context in
      Ocaml_config.c_compiler ocaml.ocaml_config
    in
    strings (c_compiler :: cc)
  in
  { Foreign_language.Dict.c = make C; cxx = make Cxx }
;;

let get_prog = function
  | Ok p -> path p
  | Error err -> Action.Prog.Not_found.raise err
;;

let c_compiler_and_flags ocaml_config =
  Ocaml_config.c_compiler ocaml_config :: Ocaml_config.ocamlc_cflags ocaml_config
;;

let relative ~source d s =
  Path.build (Path.Build.relative ~error_loc:(Dune_lang.Template.Pform.loc source) d s)
;;

type nonrec expansion_result =
  | Direct of value
  | Need_full_expander of (t -> value)

let static v = Direct (Without v)

let[@inline never] invalid_use_of_target_variable
  t
  ~(source : Dune_lang.Template.Pform.t)
  ~var_multiplicity
  =
  match t.expanding_what with
  | Nothing_special | Deps_like_field -> isn't_allowed_in_this_position ~source
  | User_action_without_targets { what } ->
    User_error.raise
      ~loc:source.loc
      [ Pp.textf
          "You cannot use %s in %s."
          (Dune_lang.Template.Pform.describe source)
          what
      ]
  | User_action targets ->
    (match targets with
     | Infer ->
       User_error.raise
         ~loc:source.loc
         [ Pp.textf
             "You cannot use %s with inferred rules."
             (Dune_lang.Template.Pform.describe source)
         ]
     | Static { targets = _; multiplicity } ->
       assert (multiplicity <> var_multiplicity);
       Targets_spec.Multiplicity.check_variable_matches_field
         ~loc:source.loc
         ~field:multiplicity
         ~variable:var_multiplicity;
       assert false)
;;

let expand_read_macro ~dir ~source s ~read =
  let path = relative ~source dir s in
  let read =
    let open Memo.O in
    Build_system.read_file path >>| read
  in
  Need_full_expander
    (fun t ->
      if Dune_project.dune_version t.project >= (3, 0)
      then Deps.Without read
      else
        (* To prevent it from working in certain position before Dune 3.0. It'd
           be nice if we could invite the user to upgrade to (lang dune 3.0),
           but this is a bigger refactoring. *)
        With (Action_builder.of_memo read))
;;

let file_of_lib db context ~loc ~lib ~file =
  let open Resolve.Memo.O in
  let+ dir =
    let* lib = Lib.DB.resolve db (loc, lib) in
    let info = Lib.info lib in
    match Lib.is_local lib with
    | false -> Resolve.Memo.return @@ Lib_info.src_dir info
    | true ->
      let name = Lib.name lib in
      let subdir =
        Lib_info.Status.relative_to_package (Lib_info.status info) name
        |> Option.value_exn
      in
      let+ pkg_root =
        let package = Lib_name.package_name name in
        (* Why do we return the install path? *)
        let+ context = Resolve.Memo.lift_memo @@ context >>| Context.name in
        Install.Context.lib_dir ~context ~package
      in
      Path.build (Path.Build.append_local pkg_root subdir)
  in
  Path.relative dir file
;;

let expand_lib_variable t source ~lib ~file ~lib_exec ~lib_private =
  let loc = Dune_lang.Template.Pform.loc source in
  let lib = Lib_name.parse_string_exn (loc, lib) in
  let open Memo.O in
  let scope = if lib_exec then t.scope_host else t.scope in
  let project = t.scope >>| Scope.project in
  let p =
    let open Resolve.Memo.O in
    if lib_private
    then
      let* scope = Resolve.Memo.lift_memo scope in
      let* lib = Lib.DB.resolve (Scope.libs scope) (loc, lib) in
      let* current_project = Resolve.Memo.lift_memo project in
      let referenced_project =
        Lib.info lib |> Lib_info.status |> Lib_info.Status.project
      in
      if Option.equal Dune_project.equal (Some current_project) referenced_project
      then Resolve.Memo.return (Path.relative (Lib_info.src_dir (Lib.info lib)) file)
      else
        Resolve.Memo.fail
          (User_error.make
             ~loc
             [ Pp.textf
                 "The variable \"lib%s-private\" can only refer to libraries within the \
                  same project. The current project's name is %S, but the reference is \
                  to %s."
                 (if lib_exec then "exec" else "")
                 (Dune_project_name.to_string_hum (Dune_project.name current_project))
                 (match referenced_project with
                  | None -> "an external library"
                  | Some project ->
                    Dune_project.name project
                    |> Dune_project_name.to_string_hum
                    |> String.quoted)
             ])
    else (
      let artifacts, context =
        if lib_exec
        then t.public_libs_host, Context.host t.context
        else t.public_libs, Memo.return t.context
      in
      let* artifacts = Resolve.Memo.lift_memo artifacts in
      file_of_lib artifacts context ~loc ~lib ~file)
  in
  (let open Memo.O in
   let* project = project in
   let* scope = scope in
   Resolve.Memo.peek p
   >>| function
   | Ok p ->
     (match file with
      | "" | "." ->
        if Dune_project.dune_version project < (3, 0)
        then Action_builder.return [ Value.Path p ]
        else
          User_error.raise
            ~loc
            [ Pp.textf
                "The form %%{%s:<libname>:%s} is no longer supported since version 3.0 \
                 of the Dune language."
                (if lib_private then "lib-private" else "lib")
                file
            ]
            ~hints:
              [ (match Lib_name.to_string lib with
                 | "ctypes" ->
                   Pp.text
                     "Did you know that Dune 3.0 supports ctypes natively? See the \
                      manual for more details."
                 | _ ->
                   Pp.textf
                     "If you are trying to use this form to include a directory, you \
                      should instead use (foreign_stubs (include_dirs (lib %s))). See \
                      the manual for more details."
                     (Lib_name.to_string lib))
              ]
      | _ ->
        if (not lib_exec) || (not Sys.win32) || Filename.extension file = ".exe"
        then dep p
        else (
          let p_exe = Path.extend_basename p ~suffix:".exe" in
          Action_builder.if_file_exists p_exe ~then_:(dep p_exe) ~else_:(dep p)))
   | Error () ->
     (if lib_private
      then Resolve.Memo.map p ~f:(fun _ -> assert false)
      else
        let open Resolve.Memo.O in
        Lib.DB.available (Scope.libs scope) lib
        |> Resolve.Memo.lift_memo
        >>= function
        | false ->
          let+ (_ : Path.t) = p in
          assert false
        | true ->
          Resolve.Memo.fail
            (User_error.make
               ~loc
               [ Pp.textf
                   "The library %S is not public. The variable \"lib%s\" expands to the \
                    file's installation path which is not defined for private libraries."
                   (Lib_name.to_string lib)
                   (if lib_exec then "exec" else "")
               ]))
     |> Resolve.Memo.read)
  |> Action_builder.of_memo_join
;;

let make loc context =
  let path = Context.path context in
  let open Memo.O in
  let+ make = Make_prog.which loc (Context.name context) ~path in
  [ Value.Path make ]
;;

let lib_config_var (var : Pform.Var.t) (lib_config : Lib_config.t) =
  [ (match var with
     | Ocaml_stdlib_dir -> Value.Dir lib_config.stdlib_dir
     | Ext_obj -> String lib_config.ext_obj
     | Ext_lib -> String lib_config.ext_lib
     | Ext_dll -> String lib_config.ext_dll
     | Ccomp_type -> String (Ocaml_config.Ccomp_type.to_string lib_config.ccomp_type)
     | _ -> Code_error.raise "not a Lib_config.t variable" [ "var", Pform.Var.to_dyn var ])
  ]
;;

let ocaml_config_var (var : Pform.Var.t) (ocaml_config : Ocaml_config.t) =
  match var with
  | Cpp -> c_compiler_and_flags ocaml_config @ [ "-E" ] |> strings
  | Pa_cpp ->
    c_compiler_and_flags ocaml_config @ [ "-undef"; "-traditional"; "-x"; "c"; "-E" ]
    |> strings
  | Arch_sixtyfour -> 64 = Ocaml_config.word_size ocaml_config |> string_of_bool |> string
  | Ocaml_version -> Ocaml_config.version_string ocaml_config |> string
  | Ext_asm -> Ocaml_config.ext_asm ocaml_config |> string
  | Ext_plugin ->
    (if Ocaml_config.natdynlink_supported ocaml_config then Mode.Native else Byte)
    |> Mode.plugin_ext
    |> string
  | Os_type ->
    Ocaml_config.os_type ocaml_config |> Ocaml_config.Os_type.to_string |> string
  | Architecture -> Ocaml_config.architecture ocaml_config |> string
  | System -> Ocaml_config.system ocaml_config |> string
  | Model -> Ocaml_config.model ocaml_config |> string
  | Ext_exe -> Ocaml_config.ext_exe ocaml_config |> string
  | _ -> Code_error.raise "not a ocaml_config variables" [ "var", Pform.Var.to_dyn var ]
;;

let expand_pform_var (context : Context.t) ~dir ~source (var : Pform.Var.t) =
  let open Memo.O in
  let ocaml = Context.ocaml context in
  match var with
  | Pkg _ -> assert false
  | Nothing -> static (Memo.return [])
  | User_var _
  | Deps
  | Input_file
  | Library_name
  | Partition
  | Impl_files
  | Intf_files
  | Test
  | Corrected_suffix ->
    (* These would be part of [bindings] *)
    isn't_allowed_in_this_position ~source
  | First_dep ->
    (* This case is for %{<} which was only allowed inside jbuild files *)
    assert false
  | Target ->
    Need_full_expander
      (fun t -> invalid_use_of_target_variable t ~source ~var_multiplicity:One)
  | Targets ->
    Need_full_expander
      (fun t -> invalid_use_of_target_variable t ~source ~var_multiplicity:Multiple)
  | Profile ->
    Context.profile context |> Profile.to_string |> string |> Memo.return |> static
  | Workspace_root ->
    [ Value.Dir (Path.build (Context.build_dir context)) ] |> Memo.return |> static
  | Context_name ->
    Context.name context |> Context_name.to_string |> string |> Memo.return |> static
  | Ocaml ->
    static
    @@ let+ ocaml = ocaml in
       get_prog ocaml.ocaml
  | Ocaml_bin_dir ->
    static
    @@ let+ ocaml = ocaml in
       [ Value.Dir ocaml.bin_dir ]
  | Ocamlc ->
    static
    @@ let+ ocaml = ocaml in
       path ocaml.ocamlc
  | Ocamlopt ->
    static
    @@ let+ ocaml = ocaml in
       get_prog ocaml.ocamlopt
  | Make -> Direct (Without (make (Dune_lang.Template.Pform.loc source) context))
  | Dev_null -> path Dev_null.path |> Memo.return |> static
  | Ocaml_stdlib_dir | Ext_obj | Ext_lib | Ext_dll | Ccomp_type ->
    (let+ ocaml = ocaml in
     lib_config_var var ocaml.lib_config)
    |> static
  | Ext_exe
  | Cpp
  | Pa_cpp
  | Arch_sixtyfour
  | Ocaml_version
  | Ext_asm
  | Ext_plugin
  | Os_type
  | Architecture
  | System
  | Model ->
    (let+ ocaml = ocaml in
     ocaml_config_var var ocaml.ocaml_config)
    |> static
  | Ignoring_promoted_rules ->
    string_of_bool !Clflags.ignore_promoted_rules |> string |> Memo.return |> static
  | Project_root ->
    Need_full_expander
      (fun t ->
        Without
          ([ Value.Dir
               (Path.Build.append_source
                  (Context.build_dir t.context)
                  (Dune_project.root t.project)
                |> Path.build)
           ]
           |> Memo.return))
  | Cc -> Need_full_expander (fun t -> With (cc t).c)
  | Cxx -> Need_full_expander (fun t -> With (cc t).cxx)
  | Toolchain ->
    (match Context.findlib_toolchain context with
     | Some toolchain -> Context_name.to_string toolchain
     | None ->
       let loc = Dune_lang.Template.Pform.loc source in
       User_error.raise ~loc [ Pp.text "No toolchain defined for this context" ])
    |> string
    |> Memo.return
    |> static
  | Inline_tests ->
    (let+ inline_tests = Env_stanza_db.inline_tests ~dir in
     Dune_env.Inline_tests.to_string inline_tests |> string)
    |> static
;;

let ocaml_config_macro source macro_invocation context =
  let s = Pform.Macro_invocation.Args.whole macro_invocation in
  static
  @@
  let open Memo.O in
  let+ ocaml_config =
    let+ ocaml = Context.ocaml context in
    ocaml.ocaml_config
  in
  match Ocaml_config.by_name ocaml_config s with
  | None ->
    User_error.raise
      ~loc:(Dune_lang.Template.Pform.loc source)
      [ Pp.textf "Unknown ocaml configuration variable %S" s ]
  | Some v ->
    (match v with
     | Bool x -> string (string_of_bool x)
     | Int x -> string (string_of_int x)
     | String x -> string x
     | Words x -> strings x
     | Prog_and_args x -> strings (x.prog :: x.args))
;;

let env_macro t source macro_invocation =
  match Pform.Macro_invocation.Args.whole macro_invocation |> String.rsplit2 ~on:'=' with
  | None ->
    User_error.raise
      ~loc:source.Dune_lang.Template.Pform.loc
      [ Pp.textf
          "%s must always come with a default value."
          (Dune_lang.Template.Pform.describe source)
      ]
      ~hints:[ Pp.text "the syntax is %{env:VAR=DEFAULT-VALUE}" ]
  | Some (var, default) ->
    (match Env.Var.Map.find t.local_env var with
     | Some v -> Deps.With (v >>| string)
     | None ->
       Deps.Without
         (let open Memo.O in
          let+ env = t.env in
          Env.get env var |> Option.value ~default |> string))
;;

let expand_pform_macro
  (context : Context.t)
  ~dir
  ~source
  (macro_invocation : Pform.Macro_invocation.t)
  =
  let s = Pform.Macro_invocation.Args.whole macro_invocation in
  match macro_invocation.macro with
  | Pkg -> Code_error.raise "pkg forms aren't possible here" []
  | Pkg_self -> Code_error.raise "pkg-self forms aren't possible here" []
  | Ocaml_config -> ocaml_config_macro source macro_invocation context
  | Env -> Need_full_expander (fun t -> env_macro t source macro_invocation)
  | Version -> Need_full_expander (fun t -> Deps.Without (expand_version t ~source s))
  | Artifact a -> Need_full_expander (fun t -> Deps.With (expand_artifact ~source t a s))
  | Path_no_dep ->
    (* This case is for %{path-no-dep:...} which was only allowed inside
           jbuild files *)
    assert false
  | Exe -> Need_full_expander (fun t -> With (dep (map_exe t (relative ~source t.dir s))))
  | Dep -> Need_full_expander (fun t -> With (dep (relative ~source t.dir s)))
  | Bin ->
    Need_full_expander
      (fun t ->
        With
          (let* prog =
             Action_builder.of_memo
               (let open Memo.O in
                let* artifacts_host = t.artifacts_host in
                Artifacts.binary
                  ~loc:(Some (Dune_lang.Template.Pform.loc source))
                  artifacts_host
                  s)
           in
           dep (Action.Prog.ok_exn prog)))
  | Lib { lib_exec; lib_private } ->
    Need_full_expander
      (fun t ->
        let lib, file =
          Pform.Macro_invocation.Args.lsplit2_exn
            macro_invocation
            (Dune_lang.Template.Pform.payload_loc source)
        in
        With (expand_lib_variable t source ~lib ~file ~lib_exec ~lib_private))
  | Lib_available ->
    Need_full_expander
      (fun t ->
        Without
          (let lib = Lib_name.parse_string_exn (Dune_lang.Template.Pform.loc source, s) in
           let open Memo.O in
           let* scope = t.scope in
           let+ available = Lib.DB.available (Scope.libs scope) lib in
           available |> string_of_bool |> string))
  | Bin_available ->
    Need_full_expander
      (fun t ->
        Without
          (let open Memo.O in
           let* artifacts_host = t.artifacts_host in
           let+ b = Artifacts.binary_available artifacts_host s in
           b |> string_of_bool |> string))
  | Read -> expand_read_macro ~dir ~source s ~read:string
  | Read_lines ->
    expand_read_macro ~dir ~source s ~read:(fun x -> String.split_lines x |> strings)
  | Read_strings ->
    expand_read_macro ~dir ~source s ~read:(fun contents ->
      let lines = String.split_lines contents in
      List.map lines ~f:(fun line ->
        match Scanf.unescaped line with
        | Error () ->
          User_error.raise
            ~loc:(Loc.in_file (relative ~source dir s))
            [ Pp.textf
                "This file must be a list of lines escaped using OCaml's conventions"
            ]
        | Ok s -> s)
      |> strings)
  | Coq_config ->
    Need_full_expander
      (fun t ->
        Without
          (let open Memo.O in
           let* artifacts_host = t.artifacts_host in
           Coq_config.expand source macro_invocation artifacts_host))
;;

let expand_pform_gen ~(context : Context.t) ~bindings ~dir ~source (pform : Pform.t)
  : expansion_result
  =
  match Pform.Map.find bindings pform with
  | Some x -> Direct x
  | None ->
    (match pform with
     | Var var -> expand_pform_var context ~dir ~source var
     | Macro macro_invocation -> expand_pform_macro context ~dir ~source macro_invocation)
;;

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
        try f t with
        | User_error.E _ as exn ->
          Without
            (let open Memo.O in
             let+ () = Memo.return () in
             reraise exn))
;;

let describe_source ~source =
  Pp.textf
    "%s at %s"
    (Dune_lang.Template.Pform.to_string source)
    (Loc.to_file_colon_line source.loc)
;;

let expand_pform t ~source pform =
  Action_builder.push_stack_frame
    (fun () ->
      match
        match
          expand_pform_gen
            ~context:t.context
            ~bindings:t.bindings
            ~dir:t.dir
            ~source
            pform
        with
        | Direct v -> v
        | Need_full_expander f -> f t
      with
      | With x -> x
      | Without x -> Action_builder.of_memo x)
    ~human_readable_description:(fun () -> describe_source ~source)
;;

let expand t ~mode template =
  String_expander.Action_builder.expand
    ~dir:(Path.build t.dir)
    ~mode
    template
    ~f:(expand_pform t)
;;

let expand_str_partial t template =
  String_expander.Action_builder.expand_as_much_as_possible
    ~dir:(Path.build t.dir)
    template
    ~f:(fun ~source pform -> expand_pform t ~source pform >>| Option.some)
;;

let make_root
  ~project
  ~scope
  ~scope_host
  ~(context : Context.t)
  ~env
  ~public_libs
  ~public_libs_host
  ~artifacts_host
  =
  { dir = Context.build_dir context
  ; env
  ; local_env = Env.Var.Map.empty
  ; bindings = Pform.Map.empty
  ; scope
  ; scope_host
  ; public_libs
  ; public_libs_host
  ; artifacts_host
  ; context
  ; expanding_what = Nothing_special
  ; project
  }
;;

let expand_path t sw =
  let loc = String_with_vars.loc sw in
  let+ path =
    expand t ~mode:Single sw >>| Value.to_path ~error_loc:loc ~dir:(Path.build t.dir)
  in
  let context_root = (Context.build_context t.context).build_dir in
  (match Path.as_in_build_dir path with
   | Some p when not (Path.Build.is_descendant p ~of_:context_root) ->
     (* TODO consider turning these into external paths, since we already allow
        them to be specified as absolute paths. *)
     User_error.raise ~loc [ Pp.text "path cannot escape the context root" ]
   | _ -> ());
  path
;;

let expand_str t sw = expand t ~mode:Single sw >>| Value.to_string ~dir:(Path.build t.dir)

module No_deps = struct
  open Memo.O

  let expand_pform_no_deps t ~source pform =
    Memo.push_stack_frame
      (fun () ->
        match
          match
            expand_pform_gen
              ~context:t.context
              ~bindings:t.bindings
              ~dir:t.dir
              ~source
              pform
          with
          | Direct v -> v
          | Need_full_expander f -> f t
        with
        | With _ -> isn't_allowed_in_this_position ~source
        | Without x -> x)
      ~human_readable_description:(fun () -> describe_source ~source)
  ;;

  let expand_pform = expand_pform_no_deps

  let expand t ~mode sw =
    String_expander.Memo.expand ~dir:(Path.build t.dir) ~mode sw ~f:(expand_pform t)
  ;;

  let expand_path t sw =
    expand t ~mode:Single sw
    >>| Value.to_path ~error_loc:(String_with_vars.loc sw) ~dir:(Path.build t.dir)
  ;;

  let expand_str t sw =
    expand t ~mode:Single sw >>| Value.to_string ~dir:(Path.build t.dir)
  ;;
end

module With_deps_if_necessary = struct
  open Deps.O
  module E = String_with_vars.Make_expander (Deps)

  let expand_pform t ~source pform : _ Deps.t =
    match
      match
        expand_pform_gen ~context:t.context ~bindings:t.bindings ~dir:t.dir ~source pform
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
  ;;

  let expand t ~mode sw = E.expand ~dir:(Path.build t.dir) ~mode sw ~f:(expand_pform t)

  let expand_path t sw =
    let+ vs = expand t ~mode:Many sw in
    List.map
      vs
      ~f:
        (Value.to_path_in_build_or_external
           ~error_loc:(String_with_vars.loc sw)
           ~dir:t.dir)
  ;;
end

let expand_ordered_set_lang =
  let module Expander =
    Ordered_set_lang.Unexpanded.Expand (struct
      include Action_builder
      include String_expander.Action_builder
    end)
  in
  fun t osl ->
    let dir = Path.build (dir t) in
    Expander.expand osl ~dir ~f:(expand_pform t)
;;

let expand_and_eval_set t set ~standard =
  let+ standard =
    if Ordered_set_lang.Unexpanded.has_standard set
    then standard
    else Action_builder.return []
  and+ set = expand_ordered_set_lang t set in
  Ordered_set_lang.eval set ~standard ~eq:String.equal ~parse:(fun ~loc:_ s -> s)
;;

let eval_blang t blang =
  Blang_expand.eval ~f:(No_deps.expand_pform t) ~dir:(Path.build t.dir) blang
;;

let expand_locks t (locks : Locks.t) =
  Memo.List.map locks ~f:(fun (Lock x) -> No_deps.expand_path t x)
  |> Action_builder.of_memo
;;

module M = struct
  type nonrec t = t

  let expand = expand
  let project = project
  let eval_blang = eval_blang
  let expand_str = expand_str
  let expand_str_partial = expand_str_partial
end

let to_expander0 t = Expander0.create (Memo.return t) (module M)
