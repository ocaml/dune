open Import
open Dune_lang.Decoder
module Repository = Dune_pkg.Pkg_workspace.Repository

let default_repositories = [ Repository.overlay; Repository.upstream ]

module Lock_dir = struct
  type t =
    { path : Path.Source.t
    ; version_preference : Dune_pkg.Version_preference.t option
    ; solver_env : Dune_pkg.Solver_env.t option
    ; unset_solver_vars : Dune_pkg.Variable_name.Set.t option
    ; repositories : (Loc.t * Dune_pkg.Pkg_workspace.Repository.Name.t) list
    ; constraints : Dune_lang.Package_dependency.t list
    }

  let to_dyn
    { path; version_preference; solver_env; unset_solver_vars; repositories; constraints }
    =
    Dyn.record
      [ "path", Path.Source.to_dyn path
      ; ( "version_preference"
        , Dyn.option Dune_pkg.Version_preference.to_dyn version_preference )
      ; "solver_env", Dyn.option Dune_pkg.Solver_env.to_dyn solver_env
      ; ( "unset_solver_vars"
        , Dyn.option Dune_pkg.Variable_name.Set.to_dyn unset_solver_vars )
      ; ( "repositories"
        , Dyn.list
            Dune_pkg.Pkg_workspace.Repository.Name.to_dyn
            (List.map repositories ~f:snd) )
      ; "constraints", Dyn.list Dune_lang.Package_dependency.to_dyn constraints
      ]
  ;;

  let hash
    { path; version_preference; solver_env; unset_solver_vars; repositories; constraints }
    =
    Poly.hash
      (path, version_preference, solver_env, unset_solver_vars, repositories, constraints)
  ;;

  let equal
    { path; version_preference; solver_env; unset_solver_vars; repositories; constraints }
    t
    =
    Path.Source.equal path t.path
    && Option.equal
         Dune_pkg.Version_preference.equal
         version_preference
         t.version_preference
    && Option.equal Dune_pkg.Solver_env.equal solver_env t.solver_env
    && Option.equal Dune_pkg.Variable_name.Set.equal unset_solver_vars t.unset_solver_vars
    && List.equal
         (Tuple.T2.equal Loc.equal Dune_pkg.Pkg_workspace.Repository.Name.equal)
         repositories
         t.repositories
    && List.equal Dune_lang.Package_dependency.equal constraints t.constraints
  ;;

  let decode ~dir =
    let repositories_of_ordered_set ordered_set =
      Dune_lang.Ordered_set_lang.eval
        ordered_set
        ~parse:(fun ~loc string ->
          loc, Dune_pkg.Pkg_workspace.Repository.Name.parse_string_exn (loc, string))
        ~eq:(fun (_, x) (_, y) -> Dune_pkg.Pkg_workspace.Repository.Name.equal x y)
        ~standard:
          (List.map default_repositories ~f:(fun d -> Loc.none, Repository.name d))
    in
    let decode =
      let+ path =
        let+ path = field ~default:"dune.lock" "path" string in
        Path.Source.relative dir path
      and+ solver_env = field_o "solver_env" Dune_pkg.Solver_env.decode
      and+ unset_solver_vars =
        field_o "unset_solver_vars" (repeat (located Dune_pkg.Variable_name.decode))
      and+ version_preference =
        field_o "version_preference" Dune_pkg.Version_preference.decode
      and+ repositories = Dune_lang.Ordered_set_lang.field "repositories"
      and+ constraints =
        field ~default:[] "constraints" (repeat Dune_lang.Package_dependency.decode)
      in
      Option.iter solver_env ~f:(fun solver_env ->
        Option.iter
          unset_solver_vars
          ~f:
            (List.iter ~f:(fun (loc, variable) ->
               if Option.is_some (Dune_pkg.Solver_env.get solver_env variable)
               then
                 User_error.raise
                   ~loc
                   [ Pp.textf
                       "Variable %S appears in both 'solver_env' and 'unset_solver_vars' \
                        which is not allowed."
                       (Dune_pkg.Variable_name.to_string variable)
                   ])));
      let unset_solver_vars =
        Option.map unset_solver_vars ~f:(fun x ->
          List.map x ~f:snd |> Dune_pkg.Variable_name.Set.of_list)
      in
      { path
      ; solver_env
      ; unset_solver_vars
      ; version_preference
      ; repositories = repositories_of_ordered_set repositories
      ; constraints
      }
    in
    fields decode
  ;;
end

(* workspace files use the same version numbers as dune-project files for
   simplicity *)
let syntax = Stanza.syntax

let env_field, env_field_lazy =
  let make f g =
    field "env" ~default:(f None)
    @@ g
    @@ let+ () = Dune_lang.Syntax.since syntax (1, 1)
       and+ version = Dune_lang.Syntax.get_exn syntax
       and+ loc = loc
       and+ s = Dune_env.decode in
       let s =
         let minimum_version = 3, 2 in
         if version >= minimum_version
         then s
         else (
           match List.find_map s.rules ~f:(fun (_, config) -> config.binaries) with
           | None -> s
           | Some _ (* CR-rgrinberg: the location should come from this field *) ->
             let message =
               User_message.make
                 ~loc
                 [ Pp.text
                     (Dune_lang.Syntax.Error_msg.since
                        syntax
                        minimum_version
                        ~what:"\"binaries\" in an \"env\" stanza in a dune-workspace file")
                 ]
             in
             Dune_env.add_warning ~message s |> Dune_env.add_error ~message)
       in
       match
         List.find_map s.rules ~f:(fun (_, config) ->
           Option.bind config.binaries ~f:File_binding.Unexpanded.L.find_pform)
       with
       | None -> Some s
       | Some loc ->
         (* CR-rgrinberg: why do we forbid variables here?. *)
         User_error.raise
           ~loc
           [ Pp.text
               "Variables are not supported in \"binaries\" in an \"env\" stanza in a \
                dune-workspace file."
           ]
  in
  make Fun.id Fun.id, make Lazy.from_val lazy_
;;

module Lock_dir_selection = struct
  type t =
    | Name of string
    | Cond of Dune_lang.Cond.t

  let to_dyn = function
    | Name name -> Dyn.variant "Name" [ Dyn.string name ]
    | Cond cond -> Dyn.variant "Cond" [ Dune_lang.Cond.to_dyn cond ]
  ;;

  let decode =
    enter
      (let+ () = keyword "cond"
       and+ cond = Dune_lang.Cond.decode in
       Cond cond)
    <|> let+ name = string in
        Name name
  ;;

  let equal a b =
    match a, b with
    | Name a, Name b -> String.equal a b
    | Cond a, Cond b -> Dune_lang.Cond.equal a b
    | _ -> false
  ;;

  let eval t ~dir ~f =
    let open Memo.O in
    match t with
    | Name name -> Memo.return (Path.Source.relative dir name)
    | Cond cond ->
      let+ value = Cond_expand.eval cond ~dir:(Path.source dir) ~f in
      (match (value : Value.t option) with
       | None ->
         User_error.raise
           ~loc:cond.loc
           [ Pp.text "None of the conditions matched so no lockdir could be chosen." ]
       | Some (String s) -> Path.Source.relative dir s
       | Some (Dir p | Path p) ->
         Path.reach ~from:(Path.source dir) p |> Path.Source.of_string)
  ;;
end

module Context = struct
  module Target = struct
    type t =
      | Native
      | Named of Context_name.t

    let equal x y =
      match x, y with
      | Native, Native -> true
      | Native, _ | _, Native -> false
      | Named x, Named y -> Context_name.equal x y
    ;;

    let t =
      let+ context_name = Context_name.decode in
      match Context_name.to_string context_name with
      | "native" -> Native
      | _ -> Named context_name
    ;;

    let to_dyn =
      let open Dyn in
      function
      | Native -> variant "Native" []
      | Named name -> variant "Named" [ Context_name.to_dyn name ]
    ;;

    let add ts x =
      match x with
      | None -> ts
      | Some t -> if List.mem ts t ~equal then ts else ts @ [ t ]
    ;;
  end

  module Common = struct
    type t =
      { loc : Loc.t
      ; profile : Profile.t
      ; targets : Target.t list
      ; env : Dune_env.t option
      ; toolchain : Context_name.t option
      ; name : Context_name.t
      ; host_context : Context_name.t option
      ; paths : (string * Ordered_set_lang.t) list
      ; fdo_target_exe : Path.t option
      ; dynamically_linked_foreign_archives : bool
      ; instrument_with : Lib_name.t list
      ; merlin : bool
      }

    let to_dyn { name; targets; host_context; _ } =
      Dyn.record
        [ "name", Context_name.to_dyn name
        ; "targets", Dyn.list Target.to_dyn targets
        ; "host_context", Dyn.option Context_name.to_dyn host_context
        ]
    ;;

    let equal
      { loc = _
      ; profile
      ; targets
      ; env
      ; toolchain
      ; name
      ; host_context
      ; paths
      ; fdo_target_exe
      ; dynamically_linked_foreign_archives
      ; instrument_with
      ; merlin
      }
      t
      =
      Profile.equal profile t.profile
      && List.equal Target.equal targets t.targets
      && Option.equal Dune_env.equal env t.env
      && Option.equal Context_name.equal toolchain t.toolchain
      && Context_name.equal name t.name
      && Option.equal Context_name.equal host_context t.host_context
      && List.equal (Tuple.T2.equal String.equal Ordered_set_lang.equal) paths t.paths
      && Option.equal Path.equal fdo_target_exe t.fdo_target_exe
      && Bool.equal
           dynamically_linked_foreign_archives
           t.dynamically_linked_foreign_archives
      && List.equal Lib_name.equal instrument_with t.instrument_with
      && Bool.equal merlin t.merlin
    ;;

    let fdo_suffix t =
      match t.fdo_target_exe with
      | None -> ""
      | Some file ->
        let name, _ = Path.split_extension file in
        "-fdo-" ^ Path.basename name
    ;;

    let decode =
      let+ env = env_field
      and+ targets = field "targets" (repeat Target.t) ~default:[ Target.Native ]
      and+ profile = field_o "profile" Profile.decode
      and+ host_context =
        field_o "host" (Dune_lang.Syntax.since syntax (1, 10) >>> Context_name.decode)
      and+ toolchain =
        field_o "toolchain" (Dune_lang.Syntax.since syntax (1, 5) >>> Context_name.decode)
      and+ dynamically_linked_foreign_archives =
        let+ disable =
          field
            ~default:false
            "disable_dynamically_linked_foreign_archives"
            (Dune_lang.Syntax.since syntax (2, 0) >>> bool)
        in
        not disable
      and+ fdo_target_exe =
        let f file =
          let ext = Filename.extension file in
          if ext = ".exe"
          then Path.(relative root file)
          else
            User_error.raise
              [ Pp.concat
                  ~sep:Pp.space
                  [ User_message.command (sprintf "fdo %s" file)
                  ; Pp.textf
                      "expects executable filename ending with .exe extension, not %s. \n\
                       Please specify the name of the executable to optimize, including \
                       path from <root>."
                      ext
                  ]
                |> Pp.hovbox
              ]
        in
        field_o "fdo" (Dune_lang.Syntax.since syntax (2, 0) >>> map string ~f)
      and+ paths =
        let f l =
          match Env.Map.of_list (List.map ~f:(fun ((loc, s), _) -> s, loc) l) with
          | Ok _ -> List.map ~f:(fun ((_, s), x) -> s, x) l
          | Error (var, _, loc) ->
            User_error.raise
              ~loc
              [ Pp.textf "the variable %S can appear at most once in this stanza." var ]
        in
        field
          "paths"
          ~default:[]
          (Dune_lang.Syntax.since Stanza.syntax (1, 12)
           >>> map ~f (repeat (pair (located string) Ordered_set_lang.decode)))
      and+ instrument_with =
        field_o
          "instrument_with"
          (Dune_lang.Syntax.since syntax (2, 7) >>> repeat Lib_name.decode)
      and+ loc = loc
      and+ merlin = field_b "merlin" in
      fun ~profile_default ~instrument_with_default ->
        let profile = Option.value profile ~default:profile_default in
        let instrument_with =
          Option.value instrument_with ~default:instrument_with_default
        in
        Option.iter host_context ~f:(fun _ ->
          match targets with
          | [ Target.Native ] -> ()
          | _ ->
            User_error.raise
              ~loc
              [ Pp.text "`targets` and `host` options cannot be used in the same context."
              ]);
        { targets
        ; profile
        ; loc
        ; env
        ; name = Context_name.default
        ; host_context
        ; toolchain
        ; paths
        ; fdo_target_exe
        ; dynamically_linked_foreign_archives
        ; instrument_with
        ; merlin
        }
    ;;
  end

  module Opam = struct
    type t =
      { base : Common.t
      ; switch : Opam_switch.t
      }

    let to_dyn { base; switch } =
      let open Dyn in
      record [ "base", Common.to_dyn base; "switch", Opam_switch.to_dyn switch ]
    ;;

    let equal { base; switch } t =
      Common.equal base t.base && Opam_switch.equal switch t.switch
    ;;

    let decode =
      let+ loc_switch, switch = field "switch" (located string)
      and+ name = field_o "name" Context_name.decode
      and+ root = field_o "root" string
      and+ base = Common.decode in
      fun ~profile_default ~instrument_with_default ~x ->
        let base = base ~profile_default ~instrument_with_default in
        let name =
          match name with
          | Some s -> s
          | None ->
            let name = switch ^ Common.fdo_suffix base in
            (match Context_name.of_string_opt name with
             | Some s -> s
             | None ->
               User_error.raise
                 ~loc:loc_switch
                 [ Pp.textf "Generated context name %S is invalid" name
                 ; Pp.text
                     "Please specify a context name manually with the (name ..) field"
                 ])
        in
        let base = { base with targets = Target.add base.targets x; name } in
        let switch = { Opam_switch.switch; root } in
        { base; switch }
    ;;
  end

  module Default = struct
    type t =
      { base : Common.t
      ; lock_dir : Lock_dir_selection.t option
      }

    let to_dyn { base; lock_dir } =
      Dyn.record
        [ "base", Common.to_dyn base
        ; "lock_dir", Dyn.(option Lock_dir_selection.to_dyn) lock_dir
        ]
    ;;

    let decode =
      let+ common = Common.decode
      and+ name =
        field_o "name" (Dune_lang.Syntax.since syntax (1, 10) >>> Context_name.decode)
      and+ lock_dir =
        (* TODO
           1. guard before version check before releasing
           2. allow external paths
        *)
        field_o "lock_dir" Lock_dir_selection.decode
      in
      fun ~profile_default ~instrument_with_default ~x ->
        let common = common ~profile_default ~instrument_with_default in
        let default =
          (* TODO proper error handling with locs *)
          let name = Context_name.to_string common.name ^ Common.fdo_suffix common in
          Context_name.parse_string_exn (Loc.none, name)
        in
        let name = Option.value ~default name in
        let base = { common with targets = Target.add common.targets x; name } in
        { base; lock_dir }
    ;;

    let equal { base; lock_dir } t =
      Common.equal base t.base
      && Option.equal Lock_dir_selection.equal lock_dir t.lock_dir
    ;;
  end

  type t =
    | Default of Default.t
    | Opam of Opam.t

  let hash = Poly.hash

  let to_dyn =
    let open Dyn in
    function
    | Default d -> variant "Default" [ Default.to_dyn d ]
    | Opam o -> variant "Opam" [ Opam.to_dyn o ]
  ;;

  let equal x y =
    match x, y with
    | Default x, Default y -> Default.equal x y
    | Opam x, Opam y -> Opam.equal x y
    | _, _ -> false
  ;;

  let base = function
    | Default x -> x.base
    | Opam x -> x.base
  ;;

  let loc t = (base t).loc

  let host_context = function
    | Default { base = { host_context; _ }; _ } | Opam { base = { host_context; _ }; _ }
      -> host_context
  ;;

  let decode =
    sum
      [ ( "default"
        , let+ f = fields Default.decode in
          fun ~profile_default ~instrument_with_default ~x ->
            Default (f ~profile_default ~instrument_with_default ~x) )
      ; ( "opam"
        , let+ f = fields Opam.decode in
          fun ~profile_default ~instrument_with_default ~x ->
            Opam (f ~profile_default ~instrument_with_default ~x) )
      ]
  ;;

  let env t = (base t).env
  let name t = (base t).name
  let targets t = (base t).targets

  let all_names t =
    let n = name t in
    n
    :: List.filter_map (targets t) ~f:(function
      | Native -> None
      | Named s -> Some (Context_name.target n ~toolchain:s))
  ;;

  let default ~x ~profile ~instrument_with =
    Default
      { lock_dir = None
      ; base =
          { loc = Loc.of_pos __POS__
          ; targets = [ Option.value x ~default:Target.Native ]
          ; profile = Option.value profile ~default:Profile.default
          ; name = Context_name.default
          ; host_context = None
          ; env = None
          ; toolchain = None
          ; paths = []
          ; fdo_target_exe = None
          ; dynamically_linked_foreign_archives = true
          ; instrument_with = Option.value instrument_with ~default:[]
          ; merlin = false
          }
      }
  ;;

  let build_contexts t =
    let name = name t in
    let native = Build_context.create ~name in
    native
    :: List.filter_map (targets t) ~f:(function
      | Native -> None
      | Named toolchain ->
        let name = Context_name.target name ~toolchain in
        Some (Build_context.create ~name))
  ;;
end

type t =
  { merlin_context : Context_name.t option
  ; contexts : Context.t list
  ; env : Dune_env.t option
  ; config : Dune_config.t
  ; repos : Dune_pkg.Pkg_workspace.Repository.t list
  ; lock_dirs : Lock_dir.t list
  ; dir : Path.Source.t
  }

let to_dyn { merlin_context; contexts; env; config; repos; lock_dirs; dir } =
  let open Dyn in
  record
    [ "merlin_context", option Context_name.to_dyn merlin_context
    ; "contexts", list Context.to_dyn contexts
    ; "env", option Dune_env.to_dyn env
    ; "config", Dune_config.to_dyn config
    ; "repos", list Repository.to_dyn repos
    ; "solver", (list Lock_dir.to_dyn) lock_dirs
    ; "dir", Path.Source.to_dyn dir
    ]
;;

let equal { merlin_context; contexts; env; config; repos; lock_dirs; dir } w =
  Option.equal Context_name.equal merlin_context w.merlin_context
  && List.equal Context.equal contexts w.contexts
  && Option.equal Dune_env.equal env w.env
  && Dune_config.equal config w.config
  && List.equal Repository.equal repos w.repos
  && List.equal Lock_dir.equal lock_dirs w.lock_dirs
  && Path.Source.equal dir w.dir
;;

let hash { merlin_context; contexts; env; config; repos; lock_dirs; dir } =
  Poly.hash
    ( Option.hash Context_name.hash merlin_context
    , List.hash Context.hash contexts
    , Option.hash Dune_env.hash env
    , Dune_config.hash config
    , List.hash Repository.hash repos
    , List.hash Lock_dir.hash lock_dirs
    , Path.Source.hash dir )
;;

let find_lock_dir t path =
  List.find t.lock_dirs ~f:(fun lock_dir -> Path.Source.equal lock_dir.path path)
;;

include Dune_lang.Versioned_file.Make (struct
    type t = unit
  end)

let () = Lang.register syntax ()

module Clflags = struct
  type t =
    { x : Context_name.t option
    ; profile : Profile.t option
    ; instrument_with : Lib_name.t list option
    ; workspace_file : Path.Outside_build_dir.t option
    ; config_from_command_line : Dune_config.Partial.t
    ; config_from_config_file : Dune_config.Partial.t
    }

  let to_dyn
    { x
    ; profile
    ; instrument_with
    ; workspace_file
    ; config_from_command_line
    ; config_from_config_file
    }
    =
    let open Dyn in
    record
      [ "x", option Context_name.to_dyn x
      ; "profile", option Profile.to_dyn profile
      ; "instrument_with", option (list Lib_name.to_dyn) instrument_with
      ; "workspace_file", option Path.Outside_build_dir.to_dyn workspace_file
      ; "config_from_command_line", Dune_config.Partial.to_dyn config_from_command_line
      ; "config_from_config_file", Dune_config.Partial.to_dyn config_from_config_file
      ]
  ;;

  let t = Fdecl.create to_dyn
  let set v = Fdecl.set t v
  let t () = Fdecl.get t
end

let bad_configuration_check map =
  let find_exn loc name host =
    match Context_name.Map.find map host with
    | Some host_ctx -> host_ctx
    | None ->
      User_error.raise
        ~loc
        [ Pp.textf
            "Undefined host context '%s' for '%s'."
            (Context_name.to_string host)
            (Context_name.to_string name)
        ]
  in
  let check elt =
    Context.host_context elt
    |> Option.iter ~f:(fun host ->
      let name = Context.name elt in
      let loc = Context.loc elt in
      let host_elt = find_exn loc name host in
      Context.host_context host_elt
      |> Option.iter ~f:(fun host_of_host ->
        User_error.raise
          ~loc:(Context.loc host_elt)
          [ Pp.textf
              "Context '%s' is both a host (for '%s') and a target (for '%s')."
              (Context_name.to_string host)
              (Context_name.to_string name)
              (Context_name.to_string host_of_host)
          ]))
  in
  Context_name.Map.iter map ~f:check
;;

let top_sort contexts =
  let key = Context.name in
  let map = Context_name.Map.of_list_map_exn contexts ~f:(fun x -> key x, x) in
  let deps def =
    match Context.host_context def with
    | None -> []
    | Some ctx -> [ Context_name.Map.find_exn map ctx ]
  in
  bad_configuration_check map;
  match Context_name.Top_closure.top_closure ~key ~deps contexts with
  | Ok topo_contexts -> topo_contexts
  | Error _ -> assert false
;;

let create_final_config
  ~config_from_config_file
  ~config_from_command_line
  ~config_from_workspace_file
  =
  let ( ++ ) = Dune_config.superpose in
  Dune_config.default
  ++ config_from_config_file
  ++ config_from_workspace_file
  ++ config_from_command_line
;;

(* We load the configuration in two steps:

   - step1: we eagerly interpret all the bits that are common to the workspace
     file and the user configuration file. The other fields are left under a lazy

   - step2: we force the interpretation of the rest of the fields

   We do that so that we can load only the general configuration part at Dune's
   initialisation time, and report errors that are more specific to OCaml later
   on *)
module Step1 = struct
  type nonrec t =
    { t : t Lazy.t
    ; config : Dune_config.t
    }
end

let step1 clflags =
  let { Clflags.x
      ; profile = cl_profile
      ; instrument_with = cl_instrument_with
      ; workspace_file
      ; config_from_command_line
      ; config_from_config_file
      }
    =
    clflags
  in
  let dir =
    match workspace_file with
    | None -> Path.Source.root
    | Some file ->
      (match Path.Outside_build_dir.parent file with
       | None -> assert false
       | Some (External _) ->
         (* CR-rgrinberg: not really correct, but we don't support lock
            directories outside the workspace (for now) *)
         Path.Source.root
       | Some (In_source_dir s) -> s)
  in
  let x = Option.map x ~f:(fun s -> Context.Target.Named s) in
  let superpose_with_command_line cl field =
    let+ x = field in
    lazy (Option.value cl ~default:(Lazy.force x))
  in
  let* () = Dune_lang.Versioned_file.no_more_lang
  and+ env = env_field_lazy
  and+ profile =
    superpose_with_command_line
      cl_profile
      (field "profile" (lazy_ Profile.decode) ~default:(lazy Profile.default))
  and+ repos = multi_field "repository" (lazy_ Repository.decode)
  and+ instrument_with =
    superpose_with_command_line
      cl_instrument_with
      (field
         "instrument_with"
         (lazy_ (Dune_lang.Syntax.since Stanza.syntax (2, 7) >>> repeat Lib_name.decode))
         ~default:(lazy []))
  and+ config_from_workspace_file = Dune_config.decode_fields_of_workspace_file
  and+ lock_dirs = multi_field "lock_dir" (Lock_dir.decode ~dir) in
  let+ contexts = multi_field "context" (lazy_ Context.decode) in
  let config =
    create_final_config
      ~config_from_workspace_file
      ~config_from_config_file
      ~config_from_command_line
  in
  let t =
    lazy
      (let profile = Lazy.force profile in
       let instrument_with = Lazy.force instrument_with in
       let contexts =
         List.map contexts ~f:(fun f ->
           Lazy.force
             f
             ~profile_default:profile
             ~instrument_with_default:instrument_with
             ~x)
       in
       let defined_names = ref Context_name.Set.empty in
       let env = Lazy.force env in
       let repos = default_repositories @ List.map ~f:Lazy.force repos in
       let merlin_context =
         List.fold_left contexts ~init:None ~f:(fun acc ctx ->
           let name = Context.name ctx in
           if Context_name.Set.mem !defined_names name
           then
             User_error.raise
               ~loc:(Context.loc ctx)
               [ Pp.textf
                   "second definition of build context %S"
                   (Context_name.to_string name)
               ];
           defined_names
           := Context_name.Set.union
                !defined_names
                (Context_name.Set.of_list (Context.all_names ctx));
           match Context.base ctx, acc with
           | { merlin = true; _ }, Some _ ->
             User_error.raise
               ~loc:(Context.loc ctx)
               [ Pp.text "you can only have one context for merlin" ]
           | { merlin = true; _ }, None -> Some name
           | _ -> acc)
       in
       let contexts =
         match contexts with
         | [] ->
           [ Context.default
               ~x
               ~profile:(Some profile)
               ~instrument_with:(Some instrument_with)
           ]
         | _ -> contexts
       in
       let merlin_context =
         match merlin_context with
         | Some _ -> merlin_context
         | None ->
           if List.exists contexts ~f:(function
                | Context.Default _ -> true
                | _ -> false)
           then Some Context_name.default
           else None
       in
       { merlin_context
       ; contexts = top_sort (List.rev contexts)
       ; env
       ; config
       ; repos
       ; lock_dirs
       ; dir
       })
  in
  { Step1.t; config }
;;

let step1 clflags = fields (step1 clflags)

let default clflags =
  let { Clflags.x
      ; profile
      ; instrument_with
      ; workspace_file = _
      ; config_from_command_line
      ; config_from_config_file
      }
    =
    clflags
  in
  let x = Option.map x ~f:(fun s -> Context.Target.Named s) in
  let config =
    create_final_config
      ~config_from_config_file
      ~config_from_command_line
      ~config_from_workspace_file:Dune_config.Partial.empty
  in
  { merlin_context = Some Context_name.default
  ; contexts = [ Context.default ~x ~profile ~instrument_with ]
  ; env = None
  ; config
  ; repos = default_repositories
  ; lock_dirs = []
  ; dir = Path.Source.root
  }
;;

let default_step1 clflags =
  let t = default clflags in
  { Step1.t = lazy t; config = t.config }
;;

let load_step1 clflags p =
  Fs_memo.with_lexbuf_from_file p ~f:(fun lb ->
    if Dune_lang.Dune_file_script.eof_reached lb
    then default_step1 clflags
    else
      parse_contents lb ~f:(fun lang ->
        String_with_vars.set_decoding_env (Pform.Env.initial lang.version) (step1 clflags)))
;;

let filename = "dune-workspace"

let workspace_step1 =
  let open Memo.O in
  let f () =
    let clflags = Clflags.t () in
    let* workspace_file =
      match clflags.workspace_file with
      | None ->
        let p = Path.Outside_build_dir.of_string filename in
        let+ exists = Fs_memo.file_exists p in
        Option.some_if exists p
      | Some p ->
        Fs_memo.file_exists p
        >>| (function
         | true -> Some p
         | false ->
           User_error.raise
             [ Pp.textf
                 "Workspace file %s does not exist"
                 (Path.Outside_build_dir.to_string_maybe_quoted p)
             ])
    in
    let clflags = { clflags with workspace_file } in
    match workspace_file with
    | None -> Memo.return (default_step1 clflags)
    | Some p -> load_step1 clflags p
  in
  let memo = Memo.lazy_ ~name:"workspaces-internal" f in
  fun () -> Memo.Lazy.force memo
;;

let workspace_config () =
  let open Memo.O in
  let+ step1 = workspace_step1 () in
  step1.config
;;

let workspace =
  let open Memo.O in
  let f () =
    let+ step1 = workspace_step1 () in
    Lazy.force step1.t
  in
  let memo = Memo.lazy_ ~cutoff:equal ~name:"workspace" f in
  fun () -> Memo.Lazy.force memo
;;

let update_execution_parameters t ep =
  ep
  |> Execution_parameters.set_action_stdout_on_success t.config.action_stdout_on_success
  |> Execution_parameters.set_action_stderr_on_success t.config.action_stderr_on_success
;;

let build_contexts t = List.concat_map t.contexts ~f:Context.build_contexts
