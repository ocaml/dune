open Import
open Memo.O

let jsoo_env =
  let f =
    Env_stanza_db_flags.flags
      ~name:"jsoo-env"
      ~root:(fun ctx _ ->
        let+ profile = Per_context.profile ctx in
        Js_of_ocaml.Env.(map ~f:Action_builder.return (default ~profile)))
      ~f:(fun ~parent expander (local : Dune_env.config) ->
        let local = local.js_of_ocaml in
        let+ parent = parent in
        { Js_of_ocaml.Env.compilation_mode =
            Option.first_some local.compilation_mode parent.compilation_mode
        ; sourcemap = Option.first_some local.sourcemap parent.sourcemap
        ; submodes = Option.first_some local.submodes parent.submodes
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
    let open Memo.O in
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

let js_of_ocaml_flags t ~dir (spec : Js_of_ocaml.Flags.Spec.t) =
  Action_builder.of_memo
  @@
  let open Memo.O in
  let+ expander = Super_context.expander t ~dir
  and+ js_of_ocaml = jsoo_env ~dir in
  Js_of_ocaml.Flags.make
    ~spec
    ~default:js_of_ocaml.flags
    ~eval:(Expander.expand_and_eval_set expander)
;;

let js_of_ocaml_rule
  sctx
  ~(submode : Js_of_ocaml.Submode.t)
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
    match submode with
    | JS -> jsoo ~dir sctx
    | Wasm -> wasmoo ~dir sctx
  in
  let flags =
    let* flags = js_of_ocaml_flags sctx ~dir flags in
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
    ; (match (sourcemap : Js_of_ocaml.Sourcemap.t), submode with
       | No, _ -> A "--no-source-map"
       | Inline, _ | File, Wasm ->
         (* With wasm_of_ocaml, source maps are always inline *)
         A "--source-map-inline"
       | File, JS ->
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

let jsoo_runtime_files ~(submode : Js_of_ocaml.Submode.t) libs =
  List.concat_map libs ~f:(fun t ->
    (match submode with
     | JS -> Lib_info.jsoo_runtime
     | Wasm -> Lib_info.wasmoo_runtime)
      (Lib.info t))
;;

let standalone_runtime_rule ~submode cc ~runtime_files ~target ~flags =
  let dir = Compilation_context.dir cc in
  let sctx = Compilation_context.super_context cc in
  let config =
    js_of_ocaml_flags sctx ~dir flags
    |> Action_builder.bind ~f:(fun (x : _ Js_of_ocaml.Flags.t) -> x.compile)
    |> Action_builder.map ~f:Config.of_flags
  in
  let libs = Compilation_context.requires_link cc in
  let spec =
    Command.Args.S
      [ Resolve.Memo.args
          (let open Resolve.Memo.O in
           let+ libs = libs in
           Command.Args.Deps (jsoo_runtime_files ~submode libs))
      ; Deps (List.map ~f:Path.build runtime_files)
      ]
  in
  let dir = Compilation_context.dir cc in
  js_of_ocaml_rule
    (Compilation_context.super_context cc)
    ~submode
    ~sub_command:Build_runtime
    ~dir
    ~flags
    ~target
    ~directory_targets:[]
    ~spec
    ~config:(Some config)
;;

let exe_rule ~submode cc ~linkall ~runtime_files ~src ~target ~directory_targets ~flags =
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
           Command.Args.Deps (jsoo_runtime_files ~submode libs))
      ; Deps (List.map ~f:Path.build runtime_files)
      ; Dep (Path.build src)
      ; Dyn linkall
      ]
  in
  js_of_ocaml_rule
    sctx
    ~submode
    ~sub_command:Compile
    ~dir
    ~spec
    ~target
    ~directory_targets
    ~flags
    ~config:None
;;

let with_js_ext ~submode s =
  match Filename.split_extension s with
  | name, ".cma" -> name ^ Js_of_ocaml.Ext.cma ~submode
  | name, ".cmo" -> name ^ Js_of_ocaml.Ext.cmo ~submode
  | _ -> assert false
;;

let jsoo_archives ~submode ctx config lib =
  let info = Lib.info lib in
  let archives = Lib_info.archives info in
  match Lib.is_local lib with
  | true ->
    let obj_dir = Lib_info.obj_dir info in
    List.map archives.byte ~f:(fun archive ->
      in_obj_dir'
        ~obj_dir
        ~config:(Some config)
        [ with_js_ext ~submode (Path.basename archive) ])
  | false ->
    List.map archives.byte ~f:(fun archive ->
      Path.build
        (in_build_dir
           ctx
           ~config
           [ Lib_name.to_string (Lib.name lib)
           ; with_js_ext ~submode (Path.basename archive)
           ]))
;;

let link_rule
  ~submode
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
      ~ext:(Js_of_ocaml.Ext.cmo ~submode)
  in
  let ctx = Super_context.context sctx |> Context.build_context in
  let get_all =
    let open Action_builder.O in
    let+ config =
      js_of_ocaml_flags sctx ~dir flags
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
        (in_build_dir ctx ~config [ "stdlib"; "stdlib" ^ Js_of_ocaml.Ext.cma ~submode ])
    in
    let special_units =
      List.concat_map to_link ~f:(function
        | Lib_flags.Lib_and_module.Lib _lib -> []
        | Module (obj_dir, m) -> [ in_obj_dir' ~obj_dir ~config:None [ mod_name m ] ])
    in
    let all_libs = List.concat_map libs ~f:(jsoo_archives ~submode ctx config) in
    let all_other_modules =
      List.map cm ~f:(fun m ->
        Path.build (in_obj_dir ~obj_dir ~config:None [ mod_name m ]))
    in
    let std_exit =
      Path.build
        (in_build_dir ctx ~config [ "stdlib"; "std_exit" ^ Js_of_ocaml.Ext.cmo ~submode ])
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
    ~submode
    ~sub_command:Link
    ~dir
    ~spec
    ~target
    ~directory_targets
    ~flags
    ~config:None
;;

let build_cm' sctx ~dir ~in_context ~submode ~src ~target ~config ~sourcemap =
  let spec = Command.Args.Dep src in
  let flags = in_context.Js_of_ocaml.In_context.flags in
  js_of_ocaml_rule
    sctx
    ~submode
    ~sub_command:Compile
    ~dir
    ~flags
    ~spec
    ~target
    ~directory_targets:[]
    ~config
    ~sourcemap
;;

let iter_submodes ~f = Memo.parallel_iter [ Js_of_ocaml.Submode.JS; Wasm ] ~f

let build_cm sctx ~dir ~in_context ~submode ~src ~obj_dir ~config =
  let name = with_js_ext ~submode (Path.basename src) in
  let target = in_obj_dir ~obj_dir ~config [ name ] in
  build_cm'
    sctx
    ~dir
    ~in_context
    ~submode
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
    let open Memo.O in
    let* installed_libs = Lib.DB.installed ctx in
    Lib.DB.find installed_libs pkg
    >>= (function
     | None -> Memo.return ()
     | Some pkg ->
       let info = Lib.info pkg in
       let lib_name = Lib_name.to_string (Lib.name pkg) in
       let archives =
         let archives = (Lib_info.archives info).byte in
         (* Special case for the stdlib because it is not referenced in the
            META *)
         match lib_name with
         | "stdlib" ->
           let archive =
             let stdlib_dir = (Lib.lib_config pkg).stdlib_dir in
             Path.relative stdlib_dir
           in
           archive "stdlib.cma" :: archive "std_exit.cmo" :: archives
         | _ -> archives
       in
       iter_submodes ~f:(fun submode ->
         Memo.parallel_iter archives ~f:(fun fn ->
           let build_context = Context.build_context ctx in
           let name = Path.basename fn in
           let dir = in_build_dir build_context ~config [ lib_name ] in
           let in_context =
             { Js_of_ocaml.In_context.flags = Js_of_ocaml.Flags.standard
             ; submodes = None
             ; javascript_files = []
             ; wasm_files = []
             ; compilation_mode = None
             ; sourcemap = None
             }
           in
           let src =
             let src_dir = Lib_info.src_dir info in
             Path.relative src_dir name
           in
           let target =
             in_build_dir build_context ~config [ lib_name; with_js_ext ~submode name ]
           in
           build_cm'
             sctx
             ~dir
             ~in_context
             ~submode
             ~src
             ~target
             ~config:(Some (Action_builder.return config))
             ~sourcemap:Js_of_ocaml.Sourcemap.Inline
           |> Super_context.add_rule sctx ~dir)))
;;

let js_of_ocaml_compilation_mode t ~dir =
  let open Memo.O in
  let+ js_of_ocaml = jsoo_env ~dir in
  match js_of_ocaml.compilation_mode with
  | Some m -> m
  | None ->
    if Super_context.context t |> Context.profile |> Profile.is_dev
    then Js_of_ocaml.Compilation_mode.Separate_compilation
    else Whole_program
;;

let js_of_ocaml_sourcemap t ~dir =
  let open Memo.O in
  let+ js_of_ocaml = jsoo_env ~dir in
  match js_of_ocaml.sourcemap with
  | Some sm -> sm
  | None ->
    if Super_context.context t |> Context.profile |> Profile.is_dev
    then Js_of_ocaml.Sourcemap.Inline
    else No
;;

let jsoo_submodes ~dir ~submodes =
  (match submodes with
   | Some _ -> Memo.return submodes
   | None ->
     let+ js_of_ocaml = jsoo_env ~dir in
     js_of_ocaml.submodes)
  >>| function
  | None -> [ Js_of_ocaml.Submode.JS ]
  | Some m -> Js_of_ocaml.Submode.Set.to_list m
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
  =
  let sctx = Compilation_context.super_context cc in
  let dir = Compilation_context.dir cc in
  let { Js_of_ocaml.In_context.javascript_files
      ; wasm_files
      ; flags
      ; submodes
      ; compilation_mode
      ; sourcemap
      }
    =
    in_context
  in
  let mode : Rule.Mode.t =
    match promote with
    | None -> Standard
    | Some p -> Promote p
  in
  let open Memo.O in
  let* cmode =
    match compilation_mode with
    | None -> js_of_ocaml_compilation_mode sctx ~dir
    | Some x -> Memo.return x
  and* sourcemap =
    match sourcemap with
    | None -> js_of_ocaml_sourcemap sctx ~dir
    | Some x -> Memo.return x
  and* submodes = jsoo_submodes ~dir ~submodes in
  let* () =
    match List.mem ~equal:Poly.equal submodes JS with
    | false -> Memo.return ()
    | true ->
      let dst = Path.Build.set_extension src ~ext:(Js_of_ocaml.Ext.exe ~submode:JS) in
      let src =
        Path.build (Path.Build.set_extension src ~ext:(Js_of_ocaml.Ext.exe ~submode:Wasm))
      in
      Super_context.add_rule ~loc ~dir ~mode sctx (Action_builder.copy ~src ~dst)
  in
  Memo.parallel_iter submodes ~f:(fun submode ->
    let standalone_runtime =
      in_obj_dir
        ~obj_dir
        ~config:None
        [ Path.Build.basename
            (Path.Build.set_extension src ~ext:(Js_of_ocaml.Ext.runtime ~submode))
        ]
    in
    let target =
      let ext = Js_of_ocaml.Ext.exe ~submode in
      Path.Build.set_extension src ~ext
    in
    let runtime_files =
      match submode with
      | JS -> javascript_files
      | Wasm -> wasm_files
    in
    let directory_targets =
      match submode with
      | JS -> []
      | Wasm -> [ Path.Build.set_extension src ~ext:Js_of_ocaml.Ext.wasm_dir ]
    in
    match (cmode : Js_of_ocaml.Compilation_mode.t) with
    | Separate_compilation ->
      let+ () =
        standalone_runtime_rule
          ~submode
          cc
          ~runtime_files
          ~target:standalone_runtime
          ~flags
          ~sourcemap:Js_of_ocaml.Sourcemap.Inline
        |> Super_context.add_rule ~loc sctx ~dir
      and+ () =
        link_rule
          ~submode
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
        |> Super_context.add_rule sctx ~loc ~dir ~mode
      in
      ()
    | Whole_program ->
      exe_rule
        ~submode
        cc
        ~linkall
        ~runtime_files
        ~src
        ~target
        ~directory_targets
        ~flags
        ~sourcemap
      |> Super_context.add_rule sctx ~loc ~dir ~mode)
;;

let runner = "node"

let js_of_ocaml_runtest_alias ~dir =
  let open Memo.O in
  let+ js_of_ocaml = jsoo_env ~dir in
  match js_of_ocaml.runtest_alias with
  | Some a -> a
  | None -> Alias0.runtest
;;
