open Import
open Memo.O

let compute_env ~mode =
  let f =
    Env_stanza_db_flags.flags
      ~name:"jsoo-env"
      ~root:(fun ctx _ ->
        let+ profile = Per_context.profile ctx in
        Js_of_ocaml.Env.(map ~f:Action_builder.return (default ~profile)))
      ~f:(fun ~parent expander (local : Dune_env.config) ->
        let local =
          Js_of_ocaml.Mode.select ~mode ~js:local.js_of_ocaml ~wasm:local.wasm_of_ocaml
        in
        let* parent = parent in
        let+ enabled_if =
          match Option.first_some local.enabled_if parent.enabled_if with
          | None -> Memo.return None
          | Some enabled_if ->
            let+ v = Expander.eval_blang expander enabled_if in
            Some (Blang.Const v)
        in
        { Js_of_ocaml.Env.compilation_mode =
            Option.first_some local.compilation_mode parent.compilation_mode
        ; sourcemap = Option.first_some local.sourcemap parent.sourcemap
        ; enabled_if
        ; runtest_alias = Option.first_some local.runtest_alias parent.runtest_alias
        ; flags =
            Js_of_ocaml.Flags.make
              ~spec:local.flags
              ~default:parent.flags
              ~eval:(Expander.expand_and_eval_set expander)
        })
  in
  fun ~dir ->
    let* () = Memo.return () in
    (Staged.unstage f) dir
;;

let js_env = compute_env ~mode:JS
let wasm_env = compute_env ~mode:Wasm
let jsoo_env ~dir ~mode = (Js_of_ocaml.Mode.select ~mode ~js:js_env ~wasm:wasm_env) ~dir

module Config : sig
  type t

  val all : t list
  val path : t -> string
  val of_string : string -> t
  val of_flags : string list -> t
  val to_flags : t -> string list
end = struct
  type t =
    { js_string : bool option
    ; effects : bool option
    ; toplevel : bool option
    }

  let default = { js_string = None; effects = None; toplevel = None }
  let bool_opt = [ None; Some true; Some false ]

  let all =
    List.concat_map bool_opt ~f:(fun js_string ->
      List.concat_map bool_opt ~f:(fun effects ->
        List.concat_map bool_opt ~f:(fun toplevel -> [ { js_string; effects; toplevel } ])))
  ;;

  let get t =
    List.filter_map
      [ "use-js-string", t.js_string; "effects", t.effects; "toplevel", t.toplevel ]
      ~f:(fun (n, v) ->
        match v with
        | None -> None
        | Some v -> Some (n, v))
  ;;

  let set acc name v =
    match name with
    | "use-js-string" -> { acc with js_string = Some v }
    | "effects" -> { acc with effects = Some v }
    | "toplevel" -> { acc with toplevel = Some v }
    | _ -> acc
  ;;

  let path t =
    if t = default
    then "default"
    else
      List.map (get t) ~f:(function
        | x, true -> x
        | x, false -> "!" ^ x)
      |> String.concat ~sep:"+"
  ;;

  let of_string x =
    match x with
    | "default" -> default
    | _ ->
      List.fold_left (String.split ~on:'+' x) ~init:default ~f:(fun acc name ->
        match String.drop_prefix ~prefix:"!" name with
        | Some name -> set acc name false
        | None -> set acc name true)
  ;;

  let of_flags l =
    let rec loop acc = function
      | [] -> acc
      | "--enable" :: name :: rest -> loop (set acc name true) rest
      | maybe_enable :: rest when String.is_prefix maybe_enable ~prefix:"--enable=" ->
        (match String.drop_prefix maybe_enable ~prefix:"--enable=" with
         | Some name -> loop (set acc name true) rest
         | _ -> assert false)
      | "--disable" :: name :: rest -> loop (set acc name false) rest
      | maybe_disable :: rest when String.is_prefix maybe_disable ~prefix:"--disable=" ->
        (match String.drop_prefix maybe_disable ~prefix:"--disable=" with
         | Some name -> loop (set acc name false) rest
         | _ -> assert false)
      | "--toplevel" :: rest -> loop (set acc "toplevel" true) rest
      | _ :: rest -> loop acc rest
    in
    loop default l
  ;;

  let to_flags t =
    List.concat_map (get t) ~f:(function
      | "toplevel", true -> [ "--toplevel" ]
      | "toplevel", false -> []
      | name, true -> [ "--enable"; name ]
      | name, false -> [ "--disable"; name ])
  ;;
end

module Version = struct
  type t = int * int

  let of_string s : t option =
    let s =
      match
        String.findi s ~f:(function
          | '+' | '-' | '~' -> true
          | _ -> false)
      with
      | None -> s
      | Some i -> String.take s i
    in
    try
      match String.split s ~on:'.' with
      | [] -> None
      | [ major ] -> Some (int_of_string major, 0)
      | major :: minor :: _ -> Some (int_of_string major, int_of_string minor)
    with
    | _ -> None
  ;;

  let compare (ma1, mi1) (ma2, mi2) =
    match Int.compare ma1 ma2 with
    | Eq -> Int.compare mi1 mi2
    | n -> n
  ;;

  let impl_version bin =
    let* _ = Build_system.build_file bin in
    Memo.of_reproducible_fiber
    @@ Process.run_capture_line ~display:Quiet Strict bin [ "--version" ]
    |> Memo.map ~f:of_string
  ;;

  let version_memo = Memo.create "jsoo-version" ~input:(module Path) impl_version

  let jsoo_version jsoo =
    match jsoo with
    | Ok jsoo_path -> Memo.exec version_memo jsoo_path
    | Error e -> Action.Prog.Not_found.raise e
  ;;
end

let install_jsoo_hint = "opam install js_of_ocaml-compiler"

let in_build_dir (ctx : Build_context.t) ~config args =
  Path.Build.L.relative ctx.build_dir (".js" :: Config.path config :: args)
;;

let in_obj_dir ~obj_dir ~config args =
  let dir =
    match config with
    | None -> Obj_dir.jsoo_dir obj_dir
    | Some config -> Path.Build.relative (Obj_dir.jsoo_dir obj_dir) (Config.path config)
  in
  Path.Build.L.relative dir args
;;

let in_obj_dir' ~obj_dir ~config args =
  let dir =
    match config with
    | None -> Obj_dir.jsoo_dir obj_dir
    | Some config -> Path.relative (Obj_dir.jsoo_dir obj_dir) (Config.path config)
  in
  Path.L.relative dir args
;;

let jsoo ~dir sctx =
  Super_context.resolve_program
    sctx
    ~dir
    ~loc:None
    ~where:Original_path
    ~hint:install_jsoo_hint
    "js_of_ocaml"
;;

let wasmoo ~dir sctx =
  (* TODO add a hint when wasm_of_ocaml released on opam *)
  Super_context.resolve_program sctx ~dir ~loc:None "wasm_of_ocaml"
;;

type sub_command =
  | Compile
  | Link
  | Build_runtime

let js_of_ocaml_flags t ~dir ~mode (spec : Js_of_ocaml.Flags.Spec.t) =
  Action_builder.of_memo
  @@
  let+ expander = Super_context.expander t ~dir
  and+ js_of_ocaml = jsoo_env ~dir ~mode in
  Js_of_ocaml.Flags.make
    ~spec
    ~default:js_of_ocaml.flags
    ~eval:(Expander.expand_and_eval_set expander)
;;

let js_of_ocaml_rule
  sctx
  ~(mode : Js_of_ocaml.Mode.t)
  ~sub_command
  ~dir
  ~(flags : _ Js_of_ocaml.Flags.t)
  ~config
  ~spec
  ~target
  ~sourcemap
  ~directory_targets
  =
  let open Action_builder.O in
  let jsoo =
    match mode with
    | JS -> jsoo ~dir sctx
    | Wasm -> wasmoo ~dir sctx
  in
  let flags =
    let* flags = js_of_ocaml_flags sctx ~dir ~mode flags in
    match sub_command with
    | Compile -> flags.compile
    | Link -> flags.link
    | Build_runtime -> flags.build_runtime
  in
  Command.run_dyn_prog
    ~dir:(Path.build dir)
    jsoo
    [ (match sub_command with
       | Compile -> S []
       | Link -> A "link"
       | Build_runtime -> A "build-runtime")
    ; (match (sourcemap : Js_of_ocaml.Sourcemap.t) with
       | No -> A "--no-source-map"
       | Inline -> A "--source-map-inline"
       | File ->
         assert (Js_of_ocaml.Mode.select ~mode ~js:true ~wasm:false);
         S
           [ A "--source-map"
           ; Hidden_targets [ Path.Build.set_extension target ~ext:".map" ]
           ])
    ; Command.Args.dyn flags
    ; (match config with
       | None -> S []
       | Some config ->
         Dyn
           (Action_builder.map config ~f:(fun config ->
              Command.Args.S
                (List.map (Config.to_flags config) ~f:(fun x -> Command.Args.A x)))))
    ; A "-o"
    ; Target target
    ; spec
    ]
  |> Action_builder.With_targets.add_directories ~directory_targets
;;

let jsoo_runtime_files ~(mode : Js_of_ocaml.Mode.t) libs =
  List.concat_map libs ~f:(fun t ->
    (match mode with
     | JS -> Lib_info.jsoo_runtime
     | Wasm -> Lib_info.wasmoo_runtime)
      (Lib.info t))
;;

let standalone_runtime_rule ~mode cc ~runtime_files ~target ~flags =
  let dir = Compilation_context.dir cc in
  let sctx = Compilation_context.super_context cc in
  let config =
    js_of_ocaml_flags sctx ~dir ~mode flags
    |> Action_builder.bind ~f:(fun (x : _ Js_of_ocaml.Flags.t) -> x.compile)
    |> Action_builder.map ~f:Config.of_flags
  in
  let libs = Compilation_context.requires_link cc in
  let spec =
    Command.Args.S
      [ Resolve.Memo.args
          (let open Resolve.Memo.O in
           let+ libs = libs in
           Command.Args.Deps (jsoo_runtime_files ~mode libs))
      ; Deps (List.map ~f:Path.build runtime_files)
      ]
  in
  let dir = Compilation_context.dir cc in
  js_of_ocaml_rule
    (Compilation_context.super_context cc)
    ~mode
    ~sub_command:Build_runtime
    ~dir
    ~flags
    ~target
    ~directory_targets:[]
    ~spec
    ~config:(Some config)
;;

let exe_rule ~mode cc ~linkall ~runtime_files ~src ~target ~directory_targets ~flags =
  let dir = Compilation_context.dir cc in
  let sctx = Compilation_context.super_context cc in
  let libs = Compilation_context.requires_link cc in
  let linkall =
    let open Action_builder.O in
    let+ linkall = linkall
    and+ jsoo_version =
      let* jsoo = jsoo ~dir sctx in
      Action_builder.of_memo @@ Version.jsoo_version jsoo
    in
    Command.Args.As
      (match jsoo_version, linkall with
       | Some version, true ->
         (match Version.compare version (5, 1) with
          | Lt -> []
          | Gt | Eq -> [ "--linkall" ])
       | None, _ | _, false -> [])
  in
  let spec =
    Command.Args.S
      [ Resolve.Memo.args
          (let open Resolve.Memo.O in
           let+ libs = libs in
           Command.Args.Deps (jsoo_runtime_files ~mode libs))
      ; Deps (List.map ~f:Path.build runtime_files)
      ; Dep (Path.build src)
      ; Dyn linkall
      ]
  in
  js_of_ocaml_rule
    sctx
    ~mode
    ~sub_command:Compile
    ~dir
    ~spec
    ~target
    ~directory_targets
    ~flags
    ~config:None
;;

let with_js_ext ~mode s =
  match Filename.split_extension s with
  | name, ".cma" -> name ^ Js_of_ocaml.Ext.cma ~mode
  | name, ".cmo" -> name ^ Js_of_ocaml.Ext.cmo ~mode
  | _ -> assert false
;;

let jsoo_archives ~mode ctx config lib =
  let info = Lib.info lib in
  let archives = Lib_info.archives info in
  match Lib.is_local lib with
  | true ->
    let obj_dir = Lib_info.obj_dir info in
    List.map archives.byte ~f:(fun archive ->
      in_obj_dir'
        ~obj_dir
        ~config:(Some config)
        [ with_js_ext ~mode (Path.basename archive) ])
  | false ->
    List.map archives.byte ~f:(fun archive ->
      Path.build
        (in_build_dir
           ctx
           ~config
           [ Lib_name.to_string (Lib.name lib)
           ; with_js_ext ~mode (Path.basename archive)
           ]))
;;

let link_rule
  ~mode
  cc
  ~runtime
  ~target
  ~directory_targets
  ~obj_dir
  cm
  ~flags
  ~linkall
  ~link_time_code_gen
  =
  let sctx = Compilation_context.super_context cc in
  let dir = Compilation_context.dir cc in
  let mod_name m =
    Module_name.Unique.artifact_filename
      (Module.obj_name m)
      ~ext:(Js_of_ocaml.Ext.cmo ~mode)
  in
  let ctx = Super_context.context sctx |> Context.build_context in
  let get_all =
    let open Action_builder.O in
    let+ config =
      js_of_ocaml_flags sctx ~dir ~mode flags
      |> Action_builder.bind ~f:(fun (x : _ Js_of_ocaml.Flags.t) -> x.compile)
      |> Action_builder.map ~f:Config.of_flags
    and+ cm = cm
    and+ linkall = linkall
    and+ libs = Resolve.Memo.read (Compilation_context.requires_link cc)
    and+ { Link_time_code_gen_type.to_link; force_linkall } =
      Resolve.read link_time_code_gen
    and+ jsoo_version =
      let* jsoo = jsoo ~dir sctx in
      Action_builder.of_memo @@ Version.jsoo_version jsoo
    in
    (* Special case for the stdlib because it is not referenced in the
       META *)
    let stdlib =
      Path.build
        (in_build_dir ctx ~config [ "stdlib"; "stdlib" ^ Js_of_ocaml.Ext.cma ~mode ])
    in
    let special_units =
      List.concat_map to_link ~f:(function
        | Lib_flags.Lib_and_module.Lib _lib -> []
        | Module (obj_dir, m) -> [ in_obj_dir' ~obj_dir ~config:None [ mod_name m ] ])
    in
    let all_libs = List.concat_map libs ~f:(jsoo_archives ~mode ctx config) in
    let all_other_modules =
      List.map cm ~f:(fun m ->
        Path.build (in_obj_dir ~obj_dir ~config:None [ mod_name m ]))
    in
    let std_exit =
      Path.build
        (in_build_dir ctx ~config [ "stdlib"; "std_exit" ^ Js_of_ocaml.Ext.cmo ~mode ])
    in
    let linkall = force_linkall || linkall in
    Command.Args.S
      [ Deps
          (List.concat
             [ [ stdlib ]; special_units; all_libs; all_other_modules; [ std_exit ] ])
      ; As
          (match jsoo_version, linkall with
           | Some version, true ->
             (match Version.compare version (5, 1) with
              | Lt -> []
              | Gt | Eq -> [ "--linkall" ])
           | None, _ | _, false -> [])
      ]
  in
  let spec = Command.Args.S [ Dep (Path.build runtime); Dyn get_all ] in
  js_of_ocaml_rule
    sctx
    ~mode
    ~sub_command:Link
    ~dir
    ~spec
    ~target
    ~directory_targets
    ~flags
    ~config:None
;;

let build_cm' sctx ~dir ~in_context ~mode ~src ~target ~config ~sourcemap =
  let spec = Command.Args.Dep src in
  let flags = in_context.Js_of_ocaml.In_context.flags in
  js_of_ocaml_rule
    sctx
    ~mode
    ~sub_command:Compile
    ~dir
    ~flags
    ~spec
    ~target
    ~directory_targets:[]
    ~config
    ~sourcemap
;;

let build_cm sctx ~dir ~in_context ~mode ~src ~obj_dir ~config =
  let name = with_js_ext ~mode (Path.basename src) in
  let target = in_obj_dir ~obj_dir ~config [ name ] in
  build_cm'
    sctx
    ~dir
    ~in_context
    ~mode
    ~src
    ~target
    ~config:(Option.map config ~f:Action_builder.return)
    ~sourcemap:Js_of_ocaml.Sourcemap.Inline
;;

let setup_separate_compilation_rules sctx components =
  match components with
  | _ :: _ :: _ :: _ | [] | [ _ ] -> Memo.return ()
  | [ s_config; s_pkg ] ->
    let config = Config.of_string s_config in
    let pkg = Lib_name.parse_string_exn (Loc.none, s_pkg) in
    let ctx = Super_context.context sctx in
    let* installed_libs = Lib.DB.installed ctx in
    Lib.DB.find installed_libs pkg
    >>= (function
     | None -> Memo.return ()
     | Some pkg ->
       let info = Lib.info pkg in
       let lib_name = Lib_name.to_string (Lib.name pkg) in
       let* archives =
         let archives = (Lib_info.archives info).byte in
         (* Special case for the stdlib because it is not referenced in the
            META *)
         match lib_name with
         | "stdlib" ->
           let+ lib_config =
             let+ ocaml = Context.ocaml ctx in
             ocaml.lib_config
           in
           let archive =
             let stdlib_dir = lib_config.stdlib_dir in
             Path.relative stdlib_dir
           in
           archive "stdlib.cma" :: archive "std_exit.cmo" :: archives
         | _ -> Memo.return archives
       in
       Memo.parallel_iter Js_of_ocaml.Mode.all ~f:(fun mode ->
         Memo.parallel_iter archives ~f:(fun fn ->
           let build_context = Context.build_context ctx in
           let name = Path.basename fn in
           let dir = in_build_dir build_context ~config [ lib_name ] in
           let src =
             let src_dir = Lib_info.src_dir info in
             Path.relative src_dir name
           in
           let target =
             in_build_dir build_context ~config [ lib_name; with_js_ext ~mode name ]
           in
           build_cm'
             sctx
             ~dir
             ~in_context:Js_of_ocaml.In_context.default
             ~mode
             ~src
             ~target
             ~config:(Some (Action_builder.return config))
             ~sourcemap:Js_of_ocaml.Sourcemap.Inline
           |> Super_context.add_rule sctx ~dir)))
;;

let js_of_ocaml_compilation_mode t ~dir ~mode =
  let+ js_of_ocaml = jsoo_env ~dir ~mode in
  match js_of_ocaml.compilation_mode with
  | Some m -> m
  | None ->
    if Super_context.context t |> Context.profile |> Profile.is_dev
    then Js_of_ocaml.Compilation_mode.Separate_compilation
    else Whole_program
;;

let js_of_ocaml_sourcemap t ~dir ~mode =
  let+ js_of_ocaml = jsoo_env ~dir ~mode in
  match js_of_ocaml.sourcemap with
  | Some sm -> sm
  | None ->
    if Super_context.context t |> Context.profile |> Profile.is_dev
    then Js_of_ocaml.Sourcemap.Inline
    else No
;;

let jsoo_enabled
  ~eval
  ~dir
  ~(in_context : Js_of_ocaml.In_context.t Js_of_ocaml.Mode.Pair.t)
  ~mode
  =
  match (Js_of_ocaml.Mode.Pair.select ~mode in_context).enabled_if with
  | Some enabled_if -> eval enabled_if
  | None ->
    let* js_of_ocaml = jsoo_env ~dir ~mode in
    (match js_of_ocaml.enabled_if with
     | Some enabled_if -> eval enabled_if
     | None -> Memo.return true)
;;

let jsoo_enabled_modes ~expander ~dir ~in_context =
  let eval = Expander.eval_blang expander in
  let+ js = jsoo_enabled ~eval ~dir ~in_context ~mode:JS
  and+ wasm = jsoo_enabled ~eval ~dir ~in_context ~mode:Wasm in
  { Js_of_ocaml.Mode.Pair.js; wasm }
;;

let jsoo_is_whole_program t ~dir =
  let is_whole_program (mode : Js_of_ocaml.Compilation_mode.t) =
    match mode with
    | Whole_program -> true
    | Separate_compilation -> false
  in
  let+ js = js_of_ocaml_compilation_mode t ~dir ~mode:JS
  and+ wasm = js_of_ocaml_compilation_mode t ~dir ~mode:Wasm in
  { Js_of_ocaml.Mode.Pair.js = is_whole_program js; wasm = is_whole_program wasm }
;;

let build_exe
  cc
  ~loc
  ~in_context
  ~src
  ~(obj_dir : Path.Build.t Obj_dir.t)
  ~(top_sorted_modules : Module.t list Action_builder.t)
  ~promote
  ~linkall
  ~link_time_code_gen
  ~jsoo_mode:mode
  =
  let sctx = Compilation_context.super_context cc in
  let dir = Compilation_context.dir cc in
  let { javascript_files; wasm_files; flags; compilation_mode; sourcemap; _ }
    : Js_of_ocaml.In_context.t
    =
    in_context
  in
  let target = Path.Build.set_extension src ~ext:(Js_of_ocaml.Ext.exe ~mode) in
  let standalone_runtime =
    in_obj_dir
      ~obj_dir
      ~config:None
      [ Path.Build.basename
          (Path.Build.set_extension src ~ext:(Js_of_ocaml.Ext.runtime ~mode))
      ]
  in
  let rule_mode : Rule.Mode.t =
    match promote with
    | None -> Standard
    | Some p -> Promote p
  in
  let* cmode =
    match compilation_mode with
    | None -> js_of_ocaml_compilation_mode sctx ~dir ~mode
    | Some x -> Memo.return x
  and* sourcemap =
    match sourcemap with
    | None -> js_of_ocaml_sourcemap sctx ~dir ~mode
    | Some x -> Memo.return x
  in
  assert (Js_of_ocaml.Mode.select ~mode ~js:(wasm_files = []) ~wasm:true);
  let runtime_files = javascript_files @ wasm_files in
  let directory_targets =
    match mode with
    | JS -> []
    | Wasm -> [ Path.Build.set_extension src ~ext:Js_of_ocaml.Ext.wasm_dir ]
  in
  match (cmode : Js_of_ocaml.Compilation_mode.t) with
  | Separate_compilation ->
    let+ () =
      standalone_runtime_rule
        ~mode
        cc
        ~runtime_files
        ~target:standalone_runtime
        ~flags
        ~sourcemap:Js_of_ocaml.Sourcemap.Inline
      |> Super_context.add_rule ~loc sctx ~dir
    and+ () =
      link_rule
        ~mode
        cc
        ~runtime:standalone_runtime
        ~target
        ~directory_targets
        ~obj_dir
        top_sorted_modules
        ~flags
        ~linkall
        ~link_time_code_gen
        ~sourcemap
      |> Super_context.add_rule sctx ~loc ~dir ~mode:rule_mode
    in
    ()
  | Whole_program ->
    exe_rule
      ~mode
      cc
      ~linkall
      ~runtime_files
      ~src
      ~target
      ~directory_targets
      ~flags
      ~sourcemap
    |> Super_context.add_rule sctx ~loc ~dir ~mode:rule_mode
;;

let runner = "node"

let js_of_ocaml_runtest_alias ~dir ~mode =
  let+ js_of_ocaml = jsoo_env ~dir ~mode in
  match js_of_ocaml.runtest_alias with
  | Some a -> a
  | None -> Alias0.runtest
;;
