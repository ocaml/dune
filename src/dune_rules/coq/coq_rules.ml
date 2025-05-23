open Import
open Memo.O

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2020-2023                         *)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)

(* Utilities independent from Coq, they should be eventually moved
   elsewhere; don't mix with the rest of the code. *)
module Util : sig
  val include_flags : Lib.t list -> _ Command.Args.t
  val include_flags_legacy : Coq_lib.Legacy.t list -> _ Command.Args.t
  val ml_pack_files : Lib.t -> Path.t list

  val meta_info
    :  loc:Loc.t option
    -> version:int * int
    -> context:Context_name.t
    -> Lib.t
    -> Path.t option

  (** Given a list of library names, we try to resolve them in order, returning
      the first one that exists. *)
  val resolve_first : Lib.DB.t -> string list -> Lib.t Resolve.Memo.t
end = struct
  let include_paths ts =
    Path.Set.of_list_map ts ~f:(fun t ->
      let info = Lib.info t in
      Lib_info.src_dir info)
  ;;

  let include_flags ts = include_paths ts |> Lib_flags.L.to_iflags

  let include_flags_legacy cps =
    let cmxs_dirs = List.concat_map ~f:Coq_lib.Legacy.cmxs_directories cps in
    let f p = [ Command.Args.A "-I"; Command.Args.Path p ] in
    let l = List.concat_map ~f cmxs_dirs in
    Command.Args.S l
  ;;

  (* coqdep expects an mlpack file next to the sources otherwise it
   * will omit the cmxs deps *)
  let ml_pack_files lib =
    let plugins =
      let info = Lib.info lib in
      let plugins = Lib_info.plugins info in
      Mode.Dict.get plugins Mode.Native
    in
    let to_mlpack file =
      [ Path.set_extension file ~ext:".mlpack"; Path.set_extension file ~ext:".mllib" ]
    in
    List.concat_map plugins ~f:to_mlpack
  ;;

  let meta_info ~loc ~version ~context (lib : Lib.t) =
    let name = Lib.name lib |> Lib_name.to_string in
    match Lib_info.status (Lib.info lib) with
    | Public (_, pkg) ->
      let package = Package.name pkg in
      let meta_i =
        Path.Build.relative (Install.Context.lib_dir ~context ~package) "META"
      in
      Some (Path.build meta_i)
    | Installed -> None
    | Installed_private | Private _ ->
      let is_error = version >= (0, 6) in
      let text = if is_error then "not supported" else "deprecated" in
      User_warning.emit
        ?loc
        ~is_error
        [ Pp.textf "Using private library %s as a Coq plugin is %s" name text ];
      None
  ;;

  (* CR alizter: move this to Lib.DB *)

  (** Given a list of library names, we try to resolve them in order, returning
      the first one that exists. *)
  let rec resolve_first lib_db = function
    | [] -> Code_error.raise "resolve_first: empty list" []
    | [ n ] -> Lib.DB.resolve lib_db (Loc.none, Lib_name.of_string n)
    | n :: l ->
      let open Memo.O in
      Lib.DB.resolve_when_exists lib_db (Loc.none, Lib_name.of_string n)
      >>= (function
       | Some l -> Resolve.Memo.lift l
       | None -> resolve_first lib_db l)
  ;;
end

let coqc ~loc ~dir ~sctx =
  Super_context.resolve_program_memo
    sctx
    "coqc"
    ~where:Original_path
    ~dir
    ~loc:(Some loc)
    ~hint:"opam install coq"
;;

let select_native_mode ~sctx ~dir (buildable : Coq_stanza.Buildable.t) =
  match buildable.mode with
  | Some x ->
    Memo.return
    @@
    if
      buildable.coq_lang_version < (0, 7)
      && Super_context.context sctx |> Context.profile |> Profile.is_dev
    then Coq_mode.VoOnly
    else x
  | None ->
    if buildable.coq_lang_version < (0, 3)
    then Memo.return Coq_mode.Legacy
    else if buildable.coq_lang_version < (0, 7)
    then Memo.return Coq_mode.VoOnly
    else
      let* coqc = coqc ~sctx ~dir ~loc:buildable.loc in
      let+ config = Coq_config.make ~coqc in
      (match config with
       | Error _ -> Coq_mode.VoOnly
       | Ok config ->
         (match Coq_config.by_name config "coq_native_compiler_default" with
          | Some (String "yes") | Some (String "ondemand") -> Coq_mode.Native
          | _ -> Coq_mode.VoOnly))
;;

let coq_env =
  let f =
    Env_stanza_db_flags.flags
      ~name:"coq-env"
      ~root:(fun _ _ -> Memo.return (Action_builder.return Coq_flags.default))
      ~f:(fun ~parent expander (config : Dune_env.config) ->
        Memo.return
        @@
        let open Action_builder.O in
        let+ coq_flags =
          let standard =
            let+ x = Action_builder.of_memo_join parent in
            x.coq_flags
          in
          Expander.expand_and_eval_set expander (Coq_env.flags config.coq) ~standard
        and+ coqdep_flags =
          let standard =
            let+ x = Action_builder.of_memo_join parent in
            x.coqdep_flags
          in
          Expander.expand_and_eval_set
            expander
            (Coq_env.coqdep_flags config.coq)
            ~standard
        and+ coqdoc_flags =
          let standard =
            let+ x = Action_builder.of_memo_join parent in
            x.coqdoc_flags
          in
          Expander.expand_and_eval_set
            expander
            (Coq_env.coqdoc_flags config.coq)
            ~standard
        in
        { Coq_flags.coq_flags; coqdep_flags; coqdoc_flags })
  in
  fun ~dir ->
    (let* () = Memo.return () in
     (Staged.unstage f) dir)
    |> Action_builder.of_memo_join
;;

let coq_flags ~dir ~stanza_flags ~per_file_flags ~expander =
  let standard, flags_to_expand =
    match per_file_flags with
    | None ->
      ( Action_builder.map ~f:(fun { Coq_flags.coq_flags; _ } -> coq_flags) (coq_env ~dir)
      , stanza_flags )
    | Some per_file_flags ->
      ( Action_builder.bind
          ~f:(fun { Coq_flags.coq_flags; _ } ->
            let standard = Action_builder.return coq_flags in
            Expander.expand_and_eval_set expander stanza_flags ~standard)
          (coq_env ~dir)
      , per_file_flags )
  in
  Expander.expand_and_eval_set expander flags_to_expand ~standard
;;

let coqdep_flags ~dir ~stanza_coqdep_flags ~expander =
  Expander.expand_and_eval_set
    expander
    stanza_coqdep_flags
    ~standard:
      (Action_builder.map
         ~f:(fun { Coq_flags.coqdep_flags; _ } -> coqdep_flags)
         (coq_env ~dir))
;;

let coqdoc_flags ~dir ~stanza_coqdoc_flags ~expander =
  Expander.expand_and_eval_set
    expander
    stanza_coqdoc_flags
    ~standard:
      (Action_builder.map
         ~f:(fun { Coq_flags.coqdoc_flags; _ } -> coqdoc_flags)
         (coq_env ~dir))
;;

let theory_coqc_flag lib =
  let name = Coq_lib_name.wrapper (Coq_lib.name lib) in
  let dir = Coq_lib.obj_root lib in
  let binding_flag = if Coq_lib.implicit lib then "-R" else "-Q" in
  Command.Args.S [ A binding_flag; Path dir; A name ]
;;

let theories_flags ~theories_deps =
  Resolve.Memo.args
    (let open Resolve.Memo.O in
     let+ libs = theories_deps in
     Command.Args.S (List.map ~f:theory_coqc_flag libs))
;;

module Bootstrap : sig
  type t =
    | Implicit (** Use implicit stdlib loading, for coq lang versions < 0.8 *)
    | No_stdlib
    (** We are in >= 0.8, however the user set stdlib = no
        , or we are compiling the prelude *)
    | Stdlib of Coq_lib.t
    (** Regular case in >= 0.8 (or in < 0.8
                              (boot) was used *)

  val empty : t

  val make
    :  scope:Scope.t
    -> use_stdlib:bool
    -> wrapper_name:string
    -> coq_lang_version:Syntax.Version.t
    -> Coq_module.t
    -> t Resolve.Memo.t

  val flags : t -> 'a Command.Args.t
end = struct
  type t =
    | Implicit (** Use implicit stdlib loading, for coq lang versions < 0.8 *)
    | No_stdlib
    (** We are in >= 0.8, however the user set stdlib = no
        , or we are compiling the prelude *)
    | Stdlib of Coq_lib.t
    (** Regular case in >= 0.8 (or in < 0.8
                              (boot) was used *)

  (* For empty set of modules, we return Prelude which is kinda
     conservative. *)
  let empty = No_stdlib

  (* Hack to know if a module is a prelude module *)
  let check_init_module bootlib wrapper_name coq_module =
    String.equal (Coq_lib_name.wrapper (Coq_lib.name bootlib)) wrapper_name
    && Option.equal
         String.equal
         (List.hd_opt (Coq_module.prefix coq_module))
         (Some "Init")
  ;;

  (* [Bootstrap.t] determines, for a concrete Coq module, how the Coq
     "standard library" is being handled. See the main modes above. *)
  let make ~scope ~use_stdlib ~wrapper_name ~coq_lang_version coq_module =
    let open Resolve.Memo.O in
    let* boot_lib =
      Scope.coq_libs scope
      |> Resolve.Memo.lift_memo
      >>= Coq_lib.DB.resolve_boot ~coq_lang_version
    in
    if use_stdlib
    then (
      match boot_lib with
      | None ->
        if coq_lang_version >= (0, 8)
        then
          Resolve.Memo.fail
            (User_message.make
               [ Pp.text
                   "Couldn't find Coq standard library, and theory is not using (stdlib \
                    no)"
               ])
        else Resolve.Memo.return Implicit
      | Some (_loc, boot_lib) ->
        Resolve.Memo.return
        @@
        (* TODO: replace with per_file flags *)
        let init = check_init_module boot_lib wrapper_name coq_module in
        if init
        then No_stdlib
        else if coq_lang_version >= (0, 8)
        then Stdlib boot_lib
        else (
          (* Check if the boot library is in scope or not; note!!
             This will change once we install dune-package files
             for installed libs *)
          match boot_lib with
          | Legacy _ -> Implicit
          | Dune _ -> Stdlib boot_lib))
    else Resolve.Memo.return No_stdlib
  ;;

  let flags t : _ Command.Args.t =
    match t with
    | Implicit -> As []
    | Stdlib _ -> As [ "-boot" ]
    | No_stdlib -> As [ "-noinit"; "-boot" ]
  ;;
end

let coqc_file_flags ~dir ~theories_deps ~wrapper_name ~ml_flags : _ Command.Args.t list =
  let file_flags : _ Command.Args.t list =
    [ Dyn (Resolve.Memo.read ml_flags)
    ; theories_flags ~theories_deps
    ; A "-R"
    ; Path (Path.build dir)
    ; A wrapper_name
    ]
  in
  [ S file_flags ]
;;

let native_includes ~dir =
  let* scope = Scope.DB.find_by_dir dir in
  let lib_db = Scope.libs scope in
  (* We want the cmi files *)
  Resolve.Memo.map ~f:(fun lib ->
    let info = Lib.info lib in
    let obj_dir = Obj_dir.public_cmi_ocaml_dir (Lib_info.obj_dir info) in
    Path.Set.singleton obj_dir)
  @@ Util.resolve_first lib_db [ "coq-core.kernel"; "coq.kernel" ]
;;

let directories_of_lib ~sctx lib =
  let name = Coq_lib.name lib in
  match lib with
  | Coq_lib.Dune lib ->
    let dir = Coq_lib.Dune.src_root lib in
    let* dir_contents = Dir_contents.get sctx ~dir in
    let+ coq_sources = Dir_contents.coq dir_contents in
    Coq_sources.directories coq_sources ~name
  | Coq_lib.Legacy _ ->
    (* TODO: we could return this if we don't restrict ourselves to
       Path.Build.t here.

       EJGA: We need to understand how this interacts with globally
       installed stuff, that's more tricky than it looks actually!

       This function is used in order to determine the -nI flags that
       Coq native compiler will pass to OCaml so it can find the .cmxs
       files. For things in user-contrib we don't need to pass these
       flags but only because Coq has a very large hack adding this
       directories on require. *)
    Memo.return []
;;

let setup_native_theory_includes ~sctx ~theories_deps ~theory_dirs =
  Resolve.Memo.bind theories_deps ~f:(fun theories_deps ->
    let+ l =
      Memo.parallel_map theories_deps ~f:(fun lib ->
        let+ theory_dirs = directories_of_lib ~sctx lib in
        Path.Build.Set.of_list theory_dirs)
    in
    Resolve.return (Path.Build.Set.union_all (theory_dirs :: l)))
;;

let coqc_native_flags ~sctx ~dir ~theories_deps ~theory_dirs ~(mode : Coq_mode.t) =
  match mode with
  | Legacy -> Command.Args.empty
  | VoOnly ->
    Command.Args.As
      [ "-w"
      ; "-deprecated-native-compiler-option"
      ; "-w"
      ; "-native-compiler-disabled"
      ; "-native-compiler"
      ; "ondemand"
      ]
  | VosOnly ->
    Command.Args.As
      [ "-vos"
      ; "-w"
      ; "-deprecated-native-compiler-option"
      ; "-w"
      ; "-native-compiler-disabled"
      ; "-native-compiler"
      ; "ondemand"
      ]
  | Native ->
    let args =
      let open Action_builder.O in
      let* native_includes = Resolve.Memo.read @@ native_includes ~dir in
      let+ native_theory_includes =
        Resolve.Memo.read
        @@ setup_native_theory_includes ~sctx ~theories_deps ~theory_dirs
      in
      let include_ dir acc = Command.Args.Path dir :: A "-nI" :: acc in
      let native_include_ml_args = Path.Set.fold native_includes ~init:[] ~f:include_ in
      let native_include_theory_output =
        Path.Build.Set.fold native_theory_includes ~init:[] ~f:(fun dir acc ->
          include_ (Path.build dir) acc)
      in
      (* This dir is relative to the file, by default [.coq-native/] *)
      Command.Args.S
        [ Command.Args.As [ "-w"; "-deprecated-native-compiler-option" ]
        ; As [ "-native-output-dir"; "." ]
        ; As [ "-native-compiler"; "on" ]
        ; S (List.rev native_include_ml_args)
        ; S (List.rev native_include_theory_output)
        ]
    in
    Command.Args.Dyn args
;;

(* closure of all the ML libs a theory depends on *)
let libs_of_theory ~lib_db ~theories_deps plugins : (Lib.t list * _) Resolve.Memo.t =
  let open Resolve.Memo.O in
  let* libs =
    Resolve.Memo.List.map plugins ~f:(fun (loc, name) ->
      let+ lib = Lib.DB.resolve lib_db (loc, name) in
      loc, lib)
  in
  let* theories = theories_deps in
  (* Filter dune theories *)
  let f (t : Coq_lib.t) =
    match t with
    | Dune t -> Left t
    | Legacy t -> Right t
  in
  let dune_theories, legacy_theories = List.partition_map ~f theories in
  let* dlibs =
    Resolve.List.concat_map ~f:Coq_lib.Dune.libraries dune_theories |> Resolve.Memo.lift
  in
  let libs = libs @ dlibs in
  let+ findlib_libs = Lib.closure ~linking:false (List.map ~f:snd libs) in
  findlib_libs, legacy_theories
;;

(* compute include flags and mlpack rules *)
let ml_pack_and_meta_rule ~context ~all_libs (buildable : Coq_stanza.Buildable.t)
  : unit Action_builder.t
  =
  (* coqdep expects an mlpack file next to the sources otherwise it will
     omit the cmxs deps *)
  let coq_lang_version = buildable.coq_lang_version in
  let plugin_loc = List.hd_opt buildable.plugins |> Option.map ~f:fst in
  let meta_info = Util.meta_info ~loc:plugin_loc ~version:coq_lang_version ~context in
  (* If the mlpack files don't exist, don't fail *)
  Action_builder.all_unit
    [ Action_builder.paths (List.filter_map ~f:meta_info all_libs)
    ; Action_builder.paths_existing (List.concat_map ~f:Util.ml_pack_files all_libs)
    ]
;;

let ml_flags_and_ml_pack_rule
      ~context
      ~lib_db
      ~theories_deps
      (buildable : Coq_stanza.Buildable.t)
  =
  let res =
    let open Resolve.Memo.O in
    let+ all_libs, legacy_theories =
      libs_of_theory ~lib_db ~theories_deps buildable.plugins
    in
    let findlib_plugin_flags = Util.include_flags all_libs in
    let legacy_plugin_flags = Util.include_flags_legacy legacy_theories in
    let ml_flags = Command.Args.S [ findlib_plugin_flags; legacy_plugin_flags ] in
    ml_flags, ml_pack_and_meta_rule ~context ~all_libs buildable
  in
  let mlpack_rule =
    let open Action_builder.O in
    let* _, mlpack_rule = Resolve.Memo.read res in
    mlpack_rule
  in
  Resolve.Memo.map ~f:fst res, mlpack_rule
;;

let dep_theory_file ~dir ~wrapper_name =
  Path.Build.relative dir ("." ^ wrapper_name)
  |> Path.Build.set_extension ~ext:".theory.d"
;;

let theory_coq_args
      ~sctx
      ~dir
      ~wrapper_name
      ~boot_flags
      ~stanza_flags
      ~ml_flags
      ~theories_deps
      ~theory_dirs
  =
  let+ coq_stanza_flags =
    let+ expander = Super_context.expander sctx ~dir in
    let coq_flags =
      let coq_flags = coq_flags ~expander ~dir ~stanza_flags ~per_file_flags:None in
      (* By default we have the -q flag. We don't want to pass this to coqtop to
         allow users to load their .coqrc files for interactive development.
         Therefore we manually scrub the -q setting when passing arguments to
         coqtop. *)
      let rec remove_q = function
        | "-q" :: l -> remove_q l
        | x :: l -> x :: remove_q l
        | [] -> []
      in
      let open Action_builder.O in
      coq_flags >>| remove_q
    in
    Command.Args.dyn coq_flags (* stanza flags *)
  in
  let coq_native_flags =
    let mode = Coq_mode.VoOnly in
    coqc_native_flags ~sctx ~dir ~theories_deps ~theory_dirs ~mode
  in
  let file_flags = coqc_file_flags ~dir ~theories_deps ~wrapper_name ~ml_flags in
  [ coq_stanza_flags; coq_native_flags; Dyn boot_flags; S file_flags ]
;;

let setup_coqproject_for_theory_rule
      ~scope
      ~sctx
      ~dir
      ~loc
      ~theories_deps
      ~wrapper_name
      ~use_stdlib
      ~ml_flags
      ~coq_lang_version
      ~stanza_flags
      ~theory_dirs
      coq_modules
  =
  (* Process coqdep and generate rules *)
  let boot_type =
    match coq_modules with
    | [] -> Resolve.Memo.return Bootstrap.empty
    | m :: _ -> Bootstrap.make ~scope ~use_stdlib ~wrapper_name ~coq_lang_version m
  in
  let boot_flags = Resolve.Memo.read boot_type |> Action_builder.map ~f:Bootstrap.flags in
  let* args =
    theory_coq_args
      ~sctx
      ~dir
      ~wrapper_name
      ~boot_flags
      ~stanza_flags
      ~ml_flags
      ~theories_deps
      ~theory_dirs
  in
  let contents : string With_targets.t =
    let open With_targets.O in
    let dir = Path.build dir in
    let+ args_bld = Command.expand ~dir (Command.Args.S args)
    and+ args_src =
      let dir = Path.source (Path.drop_build_context_exn dir) in
      Command.expand ~dir (Command.Args.S args)
    in
    let contents = Buffer.create 73 in
    let rec add_args args_bld args_src =
      match args_bld, args_src with
      | (("-R" | "-Q") as o) :: db :: mb :: args_bld, _ :: ds :: ms :: args_src ->
        Buffer.add_string contents o;
        Buffer.add_char contents ' ';
        Buffer.add_string contents db;
        Buffer.add_char contents ' ';
        Buffer.add_string contents mb;
        Buffer.add_char contents '\n';
        if db <> ds
        then (
          Buffer.add_string contents o;
          Buffer.add_char contents ' ';
          Buffer.add_string contents ds;
          Buffer.add_char contents ' ';
          Buffer.add_string contents ms;
          Buffer.add_char contents '\n');
        add_args args_bld args_src
      | "-I" :: _ :: args_bld, "-I" :: d :: args_src ->
        Buffer.add_string contents "-I ";
        Buffer.add_string contents d;
        Buffer.add_char contents '\n';
        add_args args_bld args_src
      | o :: args_bld, _ :: args_src ->
        Buffer.add_string contents "-arg ";
        Buffer.add_string contents o;
        Buffer.add_char contents '\n';
        add_args args_bld args_src
      | [], [] -> ()
      | _, _ -> assert false
    in
    add_args args_bld args_src;
    Buffer.contents contents
  in
  let mode =
    let open Rule.Promote in
    let lifetime = Lifetime.Until_clean in
    Rule.Mode.Promote { lifetime; into = None; only = None }
  in
  let coqproject = Path.Build.relative dir "_CoqProject" in
  Super_context.add_rule
    ~mode
    ~loc
    sctx
    ~dir
    (Action_builder.write_file_dyn coqproject contents.build)
;;

let setup_coqdep_for_theory_rule
      ~sctx
      ~dir
      ~loc
      ~theories_deps
      ~wrapper_name
      ~source_rule
      ~ml_flags
      ~mlpack_rule
      ~boot_flags
      ~stanza_coqdep_flags
      coq_modules
  =
  (* coqdep needs the full source + plugin's mlpack to be present :( *)
  let sources = List.rev_map ~f:Coq_module.source coq_modules in
  let file_flags =
    [ Command.Args.S (coqc_file_flags ~dir ~theories_deps ~wrapper_name ~ml_flags)
    ; As [ "-dyndep"; "opt"; "-vos" ]
    ; Deps sources
    ]
  in
  let extra_coqdep_flags =
    (* Standard flags for coqdep *)
    let open Action_builder.O in
    let* expander = Action_builder.of_memo @@ Super_context.expander sctx ~dir in
    coqdep_flags ~dir ~stanza_coqdep_flags ~expander
  in
  let coqdep_flags =
    Command.Args.Dyn boot_flags :: Command.Args.dyn extra_coqdep_flags :: file_flags
  in
  let stdout_to = dep_theory_file ~dir ~wrapper_name in
  let* coqdep =
    Super_context.resolve_program_memo
      sctx
      "coqdep"
      ~dir
      ~where:Original_path
      ~loc:(Some loc)
      ~hint:"opam install coq"
  in
  (* Coqdep has to be called in the stanza's directory *)
  Super_context.add_rule
    ~loc
    sctx
    ~dir
    (let open Action_builder.With_targets.O in
     Action_builder.with_no_targets mlpack_rule
     >>> Action_builder.(with_no_targets (goal source_rule))
     >>> Command.run ~dir:(Path.build dir) ~stdout_to coqdep coqdep_flags)
;;

module Dep_map = Stdune.Map.Make (Path)

let coqdep_invalid phase line =
  Code_error.raise
    "coqdep returned invalid output"
    [ "phase", Dyn.string phase; "line", Dyn.string line ]
;;

(* Handle the case where the path contains ":" and coqdep escapes this
   as "\:" causing Dune to misinterpret the path. We revert
   the escaping, which allows dune to work on Windows.

   Note that coqdep escapes a few more things, including spaces, $, #,
   [], ?, %, homedir... How to handle that seems tricky.
*)
let escaped_colon = Re.compile (Re.str "\\:")
let unescape_coqdep string = Re.replace_string escaped_colon ~by:":" string

let parse_line ~dir line =
  match String.lsplit2 line ~on:':' with
  | None -> coqdep_invalid "split" line
  | Some (basename, deps) ->
    (* This should always have a file, but let's handle the error
       properly *)
    let target =
      match String.extract_blank_separated_words basename with
      | [] -> coqdep_invalid "target" line
      | vo :: _ -> vo
    in
    (* let depname, ext = Filename.split_extension ff in *)
    let target = Path.relative (Path.build dir) target in
    (* EJGA: XXX using `String.extract_blank_separated_words` works
       for OCaml, but not for Coq as we don't use `-modules` *)
    let deps = unescape_coqdep deps |> String.extract_blank_separated_words in
    (* Add prelude deps for when stdlib is in scope and we are not actually
       compiling the prelude *)
    let deps = List.map ~f:(Path.relative (Path.build dir)) deps in
    target, deps
;;

let get_dep_map ~dir ~wrapper_name : Path.t list Dep_map.t Action_builder.t =
  let file = dep_theory_file ~dir ~wrapper_name in
  let open Action_builder.O in
  let f = parse_line ~dir in
  Action_builder.lines_of (Path.build file)
  >>| fun lines ->
  List.map ~f lines
  |> Dep_map.of_list
  |> function
  | Ok map -> map
  | Error (k, r1, r2) ->
    Code_error.raise
      "get_dep_map: duplicate keys"
      [ "lines", Dyn.list Dyn.string lines
      ; "key", Path.to_dyn k
      ; "entry 1", Dyn.list Path.to_dyn r1
      ; "entry 2", Dyn.list Path.to_dyn r2
      ]
;;

(* EJGA: Would be nice to have a functor in [Stdune.Tuple] to do this *)
module DWInput : Memo.Input with type t = Path.Build.t * string = struct
  type t = Path.Build.t * string

  let hash = Tuple.T2.hash Path.Build.hash String.hash
  let equal = Tuple.T2.equal Path.Build.equal String.equal
  let to_dyn = Tuple.T2.to_dyn Path.Build.to_dyn String.to_dyn
end

(* EJGA: with this code, we have a combined memo node that will be
   re-executed iff [dir], [wrapper_name], or the deps of the original
   action builder (the [.v.d] file from [dep_theory_file ~dir
   ~wrapper_name]) change *)
let memo_get_dep_map =
  Action_builder.create_memo
    "coq_dep_map"
    ~input:(module DWInput)
    (fun (dir, wrapper_name) -> get_dep_map ~dir ~wrapper_name)
;;

let deps_of ~dir ~boot_type ~wrapper_name ~mode coq_module =
  let open Action_builder.O in
  let vo_target =
    let ext =
      match mode with
      | Coq_mode.VosOnly -> ".vos"
      | _ -> ".vo"
    in
    Path.set_extension ~ext (Coq_module.source coq_module)
  in
  let* dep_map = Action_builder.exec_memo memo_get_dep_map (dir, wrapper_name) in
  let* boot_type = Resolve.Memo.read boot_type in
  match Dep_map.find dep_map vo_target with
  | None ->
    Code_error.raise
      "Dep_map.find failed for"
      [ "coq_module", Coq_module.to_dyn coq_module
      ; "dep_map", Dep_map.to_dyn (Dyn.list Path.to_dyn) dep_map
      ]
  | Some deps ->
    (* Inject prelude deps *)
    let deps =
      let prelude = "Init/Prelude.vo" in
      match boot_type with
      | Bootstrap.No_stdlib | Bootstrap.Implicit -> deps
      | Bootstrap.Stdlib lib -> Path.relative (Coq_lib.obj_root lib) prelude :: deps
    in
    Action_builder.paths deps
;;

let generic_coq_args
      ~sctx
      ~dir
      ~wrapper_name
      ~boot_flags
      ~per_file_flags
      ~mode
      ~coq_prog
      ~stanza_flags
      ~ml_flags
      ~theories_deps
      ~theory_dirs
      coq_module
  =
  let+ coq_stanza_flags =
    let+ expander = Super_context.expander sctx ~dir in
    let coq_flags =
      let coq_flags = coq_flags ~expander ~dir ~stanza_flags ~per_file_flags in
      (* By default we have the -q flag. We don't want to pass this to coqtop to
         allow users to load their .coqrc files for interactive development.
         Therefore we manually scrub the -q setting when passing arguments to
         coqtop. *)
      match coq_prog with
      | `Coqtop ->
        let rec remove_q = function
          | "-q" :: l -> remove_q l
          | x :: l -> x :: remove_q l
          | [] -> []
        in
        let open Action_builder.O in
        coq_flags >>| remove_q
      | _ -> coq_flags
    in
    Command.Args.dyn coq_flags (* stanza flags *)
  in
  let coq_native_flags =
    let mode =
      (* Tweak the modes for coqtop since it has no "-vos" option *)
      match mode, coq_prog with
      | Coq_mode.VosOnly, `Coqtop -> Coq_mode.VoOnly
      | _ -> mode
    in
    coqc_native_flags ~sctx ~dir ~theories_deps ~theory_dirs ~mode
  in
  let file_flags = coqc_file_flags ~dir ~theories_deps ~wrapper_name ~ml_flags in
  match coq_prog with
  | `Coqc ->
    [ coq_stanza_flags
    ; coq_native_flags
    ; Dyn boot_flags
    ; S file_flags
    ; Dep (Coq_module.source coq_module)
    ]
  | `Coqtop -> [ coq_stanza_flags; coq_native_flags; Dyn boot_flags; S file_flags ]
;;

module Per_file = struct
  let match_ map coq_module =
    let open Option.O in
    let* map = map in
    let coq_lib_name = Coq_module.name coq_module in
    Coq_module.Name.Map.find map coq_lib_name
  ;;
end

let setup_coqc_rule
      ~scope
      ~loc
      ~dir
      ~sctx
      ~coqc_dir
      ~file_targets
      ~stanza_flags
      ~modules_flags
      ~theories_deps
      ~mode
      ~wrapper_name
      ~use_stdlib
      ~ml_flags
      ~theory_dirs
      ~coq_lang_version
      coq_module
  =
  (* Process coqdep and generate rules *)
  let boot_type =
    Bootstrap.make ~scope ~use_stdlib ~wrapper_name ~coq_lang_version coq_module
  in
  let boot_flags = Resolve.Memo.read boot_type |> Action_builder.map ~f:Bootstrap.flags in
  (* TODO: merge with boot_type *)
  let per_file_flags = Per_file.match_ modules_flags coq_module in
  let* coqc = coqc ~loc ~dir ~sctx in
  let obj_files =
    Coq_module.obj_files
      ~wrapper_name
      ~mode
      ~obj_files_mode:Coq_module.Build
      ~obj_dir:dir
      coq_module
    |> List.map ~f:fst
  in
  let target_obj_files = Command.Args.Hidden_targets obj_files in
  let* args =
    generic_coq_args
      ~sctx
      ~dir
      ~wrapper_name
      ~boot_flags
      ~per_file_flags
      ~stanza_flags
      ~ml_flags
      ~theories_deps
      ~theory_dirs
      ~mode
      ~coq_prog:`Coqc
      coq_module
  in
  let deps_of = deps_of ~dir ~boot_type ~wrapper_name ~mode coq_module in
  let open Action_builder.With_targets.O in
  Super_context.add_rule
    ~loc
    ~dir
    sctx
    (Action_builder.with_no_targets deps_of
     >>> Action_builder.With_targets.add ~file_targets
         @@ Command.run ~dir:(Path.build coqc_dir) coqc (target_obj_files :: args)
     (* The way we handle the transitive dependencies of .vo files is not safe for
        sandboxing *)
     >>| Action.Full.add_sandbox Sandbox_config.no_sandboxing)
;;

let coq_modules_of_theory ~sctx lib =
  Action_builder.of_memo
  @@
  let name = Coq_lib.name lib in
  match lib with
  | Coq_lib.Legacy lib -> Memo.return @@ Coq_lib.Legacy.vo lib
  | Coq_lib.Dune lib ->
    let dir = Coq_lib.Dune.src_root lib in
    let* dir_contents = Dir_contents.get sctx ~dir in
    let+ coq_sources = Dir_contents.coq dir_contents in
    Coq_sources.library coq_sources ~name |> List.rev_map ~f:Coq_module.source
;;

let source_rule ~sctx theories =
  (* sources for depending libraries coqdep requires all the files to be in the
     tree to produce correct dependencies, including those of dependencies *)
  Action_builder.dyn_paths_unit
    (let open Action_builder.O in
     let+ l = Action_builder.List.map theories ~f:(coq_modules_of_theory ~sctx) in
     List.concat l)
;;

let setup_coqdoc_rules ~sctx ~dir ~theories_deps (s : Coq_stanza.Theory.t) coq_modules =
  let loc, name = s.buildable.loc, snd s.name in
  let rule =
    let file_flags =
      (* BUG: We need to pass --coqlib depending on the boot_type otherwise
         coqdoc will not work. *)
      [ theories_flags ~theories_deps
      ; A "-R"
      ; Path (Path.build dir)
      ; A (Coq_lib_name.wrapper (snd s.name))
      ]
    in
    fun mode ->
      let* () =
        let* coqdoc =
          Super_context.resolve_program_memo
            sctx
            "coqdoc"
            ~dir
            ~where:Original_path
            ~loc:(Some loc)
            ~hint:"opam install coq"
        in
        (let doc_dir = Coq_doc.coqdoc_directory ~mode ~obj_dir:dir ~name in
         let file_flags =
           let globs =
             let open Action_builder.O in
             let* theories_deps = Resolve.Memo.read theories_deps in
             Action_builder.of_memo
             @@
             let open Memo.O in
             let+ deps =
               Memo.parallel_map theories_deps ~f:(fun theory ->
                 let+ theory_dirs = directories_of_lib ~sctx theory in
                 Dep.Set.of_list_map theory_dirs ~f:(fun dir ->
                   (* TODO *)
                   Glob.of_string_exn Loc.none "*.glob"
                   |> File_selector.of_glob ~dir:(Path.build dir)
                   |> Dep.file_selector))
             in
             Command.Args.Hidden_deps (Dep.Set.union_all deps)
           in
           let mode_flag =
             match mode with
             | `Html -> "--html"
             | `Latex -> "--latex"
           in
           let extra_coqdoc_flags =
             (* Standard flags for coqdoc *)
             let open Action_builder.O in
             let* expander = Action_builder.of_memo @@ Super_context.expander sctx ~dir in
             let standard =
               coqdoc_flags ~dir ~stanza_coqdoc_flags:s.coqdoc_flags ~expander
             in
             Expander.expand_and_eval_set expander s.coqdoc_flags ~standard
           in
           [ Command.Args.S file_flags
           ; Command.Args.dyn extra_coqdoc_flags
           ; A mode_flag
           ; A "-d"
           ; Path (Path.build doc_dir)
           ; Deps (List.map ~f:Coq_module.source coq_modules)
           ; Dyn globs
           ; Hidden_deps
               (Dep.Set.of_files
                @@ List.map ~f:Path.build
                @@ List.map ~f:(Coq_module.glob_file ~obj_dir:dir) coq_modules)
           ]
         in
         Command.run
           ~sandbox:Sandbox_config.needs_sandboxing
           ~dir:(Path.build dir)
           coqdoc
           file_flags
         |> Action_builder.With_targets.map
              ~f:
                (Action.Full.map ~f:(fun coqdoc ->
                   Action.Progn [ Action.mkdir doc_dir; coqdoc ]))
         |> Action_builder.With_targets.add_directories ~directory_targets:[ doc_dir ])
        |> Super_context.add_rule ~loc ~dir sctx
      in
      let alias =
        match mode with
        | `Html -> Alias.make Alias0.doc ~dir
        | `Latex -> Alias.make (Alias.Name.of_string "doc-latex") ~dir
      in
      Coq_doc.coqdoc_directory ~mode ~obj_dir:dir ~name
      |> Path.build
      |> Action_builder.path
      |> Rules.Produce.Alias.add_deps alias ~loc
  in
  rule `Html >>> rule `Latex
;;

(* Common context for a theory, deps and rules *)
let theory_context ~context ~scope ~coq_lang_version ~name buildable =
  let theory =
    let* coq_lib_db = Scope.coq_libs scope in
    Coq_lib.DB.resolve coq_lib_db ~coq_lang_version name
  in
  let theories_deps =
    Resolve.Memo.bind theory ~f:(fun theory ->
      Resolve.Memo.lift @@ Coq_lib.theories_closure theory)
  in
  (* ML-level flags for depending libraries *)
  let ml_flags, mlpack_rule =
    let lib_db = Scope.libs scope in
    ml_flags_and_ml_pack_rule ~context ~theories_deps ~lib_db buildable
  in
  theory, theories_deps, ml_flags, mlpack_rule
;;

(* Common context for extraction, almost the same than above *)
let extraction_context
      ~context
      ~scope
      ~coq_lang_version
      (buildable : Coq_stanza.Buildable.t)
  =
  let coq_lib_db = Scope.coq_libs scope in
  let theories_deps =
    let* coq_lib_db = coq_lib_db in
    Resolve.Memo.List.map
      buildable.theories
      ~f:(Coq_lib.DB.resolve coq_lib_db ~coq_lang_version)
  in
  (* Extraction requires a boot library so we do this unconditionally
     for now. We must do this because it can happen that
     s.buildable.theories is empty *)
  let boot =
    let* coq_lib_db = coq_lib_db in
    Coq_lib.DB.resolve_boot ~coq_lang_version coq_lib_db
  in
  let theories_deps =
    let open Resolve.Memo.O in
    let+ boot = boot
    and+ theories_deps = theories_deps in
    match boot with
    | None -> theories_deps
    | Some (_, boot) -> boot :: theories_deps
  in
  let ml_flags, mlpack_rule =
    let lib_db = Scope.libs scope in
    ml_flags_and_ml_pack_rule ~context ~theories_deps ~lib_db buildable
  in
  theories_deps, ml_flags, mlpack_rule
;;

let setup_theory_rules ~sctx ~dir ~dir_contents (s : Coq_stanza.Theory.t) =
  let* scope = Scope.DB.find_by_dir dir in
  let coq_lang_version = s.buildable.coq_lang_version in
  let name = s.name in
  let theory, theories_deps, ml_flags, mlpack_rule =
    let context = Super_context.context sctx |> Context.name in
    theory_context ~context ~scope ~coq_lang_version ~name s.buildable
  in
  let wrapper_name = Coq_lib_name.wrapper (snd s.name) in
  let use_stdlib = s.buildable.use_stdlib in
  let name = snd s.name in
  let loc = s.buildable.loc in
  let stanza_flags = s.buildable.flags in
  let modules_flags = Option.map s.modules_flags ~f:Coq_module.Name.Map.of_list_exn in
  let* coq_dir_contents = Dir_contents.coq dir_contents in
  let theory_dirs =
    Coq_sources.directories coq_dir_contents ~name |> Path.Build.Set.of_list
  in
  let coq_modules = Coq_sources.library coq_dir_contents ~name in
  let source_rule =
    let theories =
      let open Resolve.Memo.O in
      let+ theory = theory
      and+ theories = theories_deps in
      theory :: theories
    in
    let open Action_builder.O in
    let* theories = Resolve.Memo.read theories in
    source_rule ~sctx theories
  in
  let coqc_dir = Super_context.context sctx |> Context.build_dir in
  let* mode = select_native_mode ~sctx ~dir s.buildable in
  (* First we setup the rule calling coqdep *)
  let boot_type =
    (* If coq_modules are empty it doesn't really matter, so we take
       the more conservative path and pass -boot, we don't care here
       about -noinit as coqdep ignores it *)
    match coq_modules with
    | [] -> Resolve.Memo.return Bootstrap.empty
    | m :: _ -> Bootstrap.make ~scope ~use_stdlib ~wrapper_name ~coq_lang_version m
  in
  let boot_flags = Resolve.Memo.read boot_type |> Action_builder.map ~f:Bootstrap.flags in
  setup_coqproject_for_theory_rule
    ~scope
    ~sctx
    ~dir
    ~loc
    ~theories_deps
    ~wrapper_name
    ~use_stdlib
    ~ml_flags
    ~coq_lang_version
    ~stanza_flags
    ~theory_dirs
    coq_modules
  >>> setup_coqdep_for_theory_rule
        ~sctx
        ~dir
        ~loc
        ~theories_deps
        ~wrapper_name
        ~source_rule
        ~ml_flags
        ~mlpack_rule
        ~boot_flags
        ~stanza_coqdep_flags:s.coqdep_flags
        coq_modules
  >>> Memo.parallel_iter
        coq_modules
        ~f:
          (setup_coqc_rule
             ~scope
             ~loc
             ~dir
             ~sctx
             ~file_targets:[]
             ~stanza_flags
             ~modules_flags
             ~coqc_dir
             ~theories_deps
             ~mode
             ~wrapper_name
             ~use_stdlib
             ~ml_flags
             ~coq_lang_version
             ~theory_dirs)
  (* And finally the coqdoc rules *)
  >>> setup_coqdoc_rules ~sctx ~dir ~theories_deps s coq_modules
;;

let coqtop_args_theory ~sctx ~dir ~dir_contents (s : Coq_stanza.Theory.t) coq_module =
  let* scope = Scope.DB.find_by_dir dir in
  let coq_lang_version = s.buildable.coq_lang_version in
  let name = s.name in
  let _theory, theories_deps, ml_flags, _mlpack_rule =
    let context = Super_context.context sctx |> Context.name in
    theory_context ~context ~scope ~coq_lang_version ~name s.buildable
  in
  let wrapper_name = Coq_lib_name.wrapper (snd s.name) in
  let* mode = select_native_mode ~sctx ~dir s.buildable in
  let name = snd s.name in
  let use_stdlib = s.buildable.use_stdlib in
  let boot_type =
    Bootstrap.make ~scope ~use_stdlib ~wrapper_name ~coq_lang_version coq_module
  in
  let* coq_dir_contents = Dir_contents.coq dir_contents in
  let theory_dirs =
    Coq_sources.directories coq_dir_contents ~name |> Path.Build.Set.of_list
  in
  let boot_flags = Resolve.Memo.read boot_type |> Action_builder.map ~f:Bootstrap.flags in
  let per_file_flags = None in
  generic_coq_args
    ~sctx
    ~dir
    ~wrapper_name
    ~boot_flags
    ~per_file_flags
    ~mode
    ~coq_prog:`Coqtop
    ~stanza_flags:s.buildable.flags
    ~ml_flags
    ~theories_deps
    ~theory_dirs
    coq_module
;;

(******************************************************************************)
(* Install rules *)
(******************************************************************************)

(* This is here for compatibility with Coq < 8.11, which expects plugin files to
   be in the folder containing the `.vo` files *)
let coq_plugins_install_rules ~scope ~package ~dst_dir (s : Coq_stanza.Theory.t) =
  let lib_db = Scope.libs scope in
  let+ ml_libs =
    (* get_libraries from Coq's ML dependencies *)
    Resolve.Memo.read_memo
      (Resolve.Memo.List.map ~f:(Lib.DB.resolve lib_db) s.buildable.plugins)
  in
  let rules_for_lib lib =
    let info = Lib.info lib in
    (* Don't install libraries that don't belong to this package *)
    if
      let name = Package.name package in
      Option.equal Package.Name.equal (Lib_info.package info) (Some name)
    then (
      let loc = Lib_info.loc info in
      let plugins = Lib_info.plugins info in
      Mode.Dict.get plugins Native
      |> List.map ~f:(fun plugin_file ->
        (* Safe because all coq libraries are local for now *)
        let plugin_file = Path.as_in_build_dir_exn plugin_file in
        let plugin_file_basename = Path.Build.basename plugin_file in
        let dst = Path.Local.(to_string (relative dst_dir plugin_file_basename)) in
        let entry =
          (* TODO this [loc] should come from [s.buildable.libraries] *)
          Install.Entry.make Section.Lib_root ~dst ~kind:`File plugin_file
        in
        Install.Entry.Sourced.create ~loc entry))
    else []
  in
  List.concat_map ~f:rules_for_lib ml_libs
;;

let install_rules ~sctx ~dir s =
  match s with
  | { Coq_stanza.Theory.package = None; _ } -> Memo.return []
  | { Coq_stanza.Theory.package = Some package; buildable; _ } ->
    let loc = s.buildable.loc in
    let* mode = select_native_mode ~sctx ~dir buildable in
    let* scope = Scope.DB.find_by_dir dir in
    (* We force the coq scope for this DB as to fail early in case of
       some library conflicts that would also generate conflicting
       install rules. This is needed as now install rules less lazy
       than the theory rules. *)
    let _ = Scope.coq_libs scope in
    let* dir_contents = Dir_contents.get sctx ~dir in
    let name = snd s.name in
    (* This must match the wrapper prefix for now to remain compatible *)
    let dst_suffix = Coq_lib_name.dir name in
    (* These are the rules for now, coq lang 2.0 will make this uniform *)
    let dst_dir =
      if s.boot
      then (* We drop the "Coq" prefix (!) *)
        Path.Local.of_string "coq/theories"
      else (
        let coq_root = Path.Local.of_string "coq/user-contrib" in
        Path.Local.relative coq_root dst_suffix)
    in
    (* Also, stdlib plugins are handled in a hardcoded way, so no compat install
       is needed *)
    let* coq_plugins_install_rules =
      if s.boot
      then Memo.return []
      else coq_plugins_install_rules ~scope ~package ~dst_dir s
    in
    let wrapper_name = Coq_lib_name.wrapper name in
    let to_path f = Path.reach ~from:(Path.build dir) (Path.build f) in
    let to_dst f = Path.Local.to_string @@ Path.Local.relative dst_dir f in
    let make_entry (orig_file : Path.Build.t) (dst_file : string) =
      let entry =
        Install.Entry.make Section.Lib_root ~dst:(to_dst dst_file) orig_file ~kind:`File
      in
      Install.Entry.Sourced.create ~loc entry
    in
    let+ coq_sources = Dir_contents.coq dir_contents in
    coq_sources
    |> Coq_sources.library ~name
    |> List.concat_map ~f:(fun (vfile : Coq_module.t) ->
      let obj_files =
        Coq_module.obj_files
          ~wrapper_name
          ~mode
          ~obj_dir:dir
          ~obj_files_mode:Coq_module.Install
          vfile
        |> List.map ~f:(fun ((vo_file : Path.Build.t), (install_vo_file : string)) ->
          make_entry vo_file install_vo_file)
      in
      let vfile = Coq_module.source vfile |> Path.as_in_build_dir_exn in
      let vfile_dst = to_path vfile in
      make_entry vfile vfile_dst :: obj_files)
    |> List.rev_append coq_plugins_install_rules
;;

let setup_coqpp_rules ~sctx ~dir ({ loc; modules } : Coq_stanza.Coqpp.t) =
  let* coqpp =
    Super_context.resolve_program_memo
      sctx
      "coqpp"
      ~where:Original_path
      ~dir
      ~loc:(Some loc)
      ~hint:"opam install coq"
  and* mlg_files = Coq_sources.mlg_files ~sctx ~dir ~modules in
  let mlg_rule m =
    let source = Path.build m in
    let target = Path.Build.set_extension m ~ext:".ml" in
    let args = [ Command.Args.Dep source; Hidden_targets [ target ] ] in
    let build_dir = Super_context.context sctx |> Context.build_dir in
    Command.run ~dir:(Path.build build_dir) coqpp args
  in
  List.rev_map ~f:mlg_rule mlg_files |> Super_context.add_rules ~loc ~dir sctx
;;

let setup_extraction_rules ~sctx ~dir ~dir_contents (s : Coq_stanza.Extraction.t) =
  let wrapper_name = "DuneExtraction" in
  let* coq_module =
    let+ coq = Dir_contents.coq dir_contents in
    Coq_sources.extract coq s
  in
  let file_targets =
    Coq_stanza.Extraction.ml_target_fnames s |> List.map ~f:(Path.Build.relative dir)
  in
  let loc = s.buildable.loc in
  let use_stdlib = s.buildable.use_stdlib in
  let coq_lang_version = s.buildable.coq_lang_version in
  let* scope = Scope.DB.find_by_dir dir in
  let theories_deps, ml_flags, mlpack_rule =
    let context = Super_context.context sctx |> Context.name in
    extraction_context ~context ~scope ~coq_lang_version s.buildable
  in
  let source_rule =
    let open Action_builder.O in
    let* theories_deps = Resolve.Memo.read theories_deps in
    source_rule ~sctx theories_deps >>> Action_builder.path (Coq_module.source coq_module)
  in
  let* mode = select_native_mode ~sctx ~dir s.buildable in
  let boot_type =
    (* If coq_modules are empty it doesn't really matter, so we take
       the more conservative path and pass -boot, we don't care here
       about -noinit as coqdep ignores it *)
    match [ coq_module ] with
    | [] -> Resolve.Memo.return Bootstrap.empty
    | m :: _ -> Bootstrap.make ~scope ~use_stdlib ~wrapper_name ~coq_lang_version m
  in
  let boot_flags = Resolve.Memo.read boot_type |> Action_builder.map ~f:Bootstrap.flags in
  let modules_flags = None in
  setup_coqdep_for_theory_rule
    ~sctx
    ~dir
    ~loc
    ~theories_deps
    ~wrapper_name
    ~source_rule
    ~ml_flags
    ~mlpack_rule
    ~boot_flags
    ~stanza_coqdep_flags:Ordered_set_lang.Unexpanded.standard
    [ coq_module ]
  >>> setup_coqc_rule
        ~scope
        ~file_targets
        ~stanza_flags:s.buildable.flags
        ~modules_flags
        ~sctx
        ~loc
        ~coqc_dir:dir
        coq_module
        ~dir
        ~theories_deps
        ~mode
        ~wrapper_name
        ~use_stdlib:s.buildable.use_stdlib
        ~ml_flags
        ~coq_lang_version
        ~theory_dirs:Path.Build.Set.empty
;;

let coqtop_args_extraction ~sctx ~dir (s : Coq_stanza.Extraction.t) coq_module =
  let use_stdlib = s.buildable.use_stdlib in
  let coq_lang_version = s.buildable.coq_lang_version in
  let* scope = Scope.DB.find_by_dir dir in
  let theories_deps, ml_flags, _mlpack_rule =
    let context = Super_context.context sctx |> Context.name in
    extraction_context ~context ~scope ~coq_lang_version s.buildable
  in
  let wrapper_name = "DuneExtraction" in
  let boot_type =
    Bootstrap.make ~scope ~use_stdlib ~wrapper_name ~coq_lang_version coq_module
  in
  let boot_flags = Resolve.Memo.read boot_type |> Action_builder.map ~f:Bootstrap.flags in
  let per_file_flags = None in
  let* mode = select_native_mode ~sctx ~dir s.buildable in
  generic_coq_args
    ~sctx
    ~dir
    ~wrapper_name
    ~boot_flags
    ~per_file_flags
    ~mode
    ~coq_prog:`Coqtop
    ~stanza_flags:s.buildable.flags
    ~ml_flags
    ~theories_deps
    ~theory_dirs:Path.Build.Set.empty
    coq_module
;;

(* Version for export *)
let deps_of ~dir ~use_stdlib ~wrapper_name ~mode ~coq_lang_version coq_module =
  let boot_type =
    let open Memo.O in
    let* scope = Scope.DB.find_by_dir dir in
    Bootstrap.make ~scope ~use_stdlib ~wrapper_name ~coq_lang_version coq_module
  in
  deps_of ~dir ~boot_type ~wrapper_name ~mode coq_module
;;
