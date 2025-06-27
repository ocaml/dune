(***********************************************)
(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2019-2024                         *)
(* (c) Emilio J. Gallego Arias 2024-2025       *)
(* (c) CNRS 2025                               *)
(***********************************************)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)
(* Written by: Rodolphe Lepigre                *)
(***********************************************)

open Import
open Memo.O

(* Utilities independent from Rocq, they should be eventually moved
   elsewhere; don't mix with the rest of the code. *)
module Util : sig
  val include_flags : Lib.t list -> _ Command.Args.t
  val include_flags_legacy : Rocq_lib.Legacy.t list -> _ Command.Args.t
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
    let cmxs_dirs = List.concat_map ~f:Rocq_lib.Legacy.cmxs_directories cps in
    let f p = [ Command.Args.A "-I"; Command.Args.Path p ] in
    let l = List.concat_map ~f cmxs_dirs in
    Command.Args.S l
  ;;

  (* rocqdep expects an mlpack file next to the sources otherwise it
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
        [ Pp.textf "Using private library %s as a Rocq plugin is %s" name text ];
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

let select_native_mode ~sctx ~dir (buildable : Rocq_stanza.Buildable.t) =
  match buildable.mode with
  | Some x ->
    Memo.return
    @@
    if
      buildable.coq_lang_version < (0, 7)
      && Super_context.context sctx |> Context.profile |> Profile.is_dev
    then Rocq_mode.VoOnly
    else x
  | None ->
    if buildable.coq_lang_version < (0, 3)
    then Memo.return Rocq_mode.Legacy
    else if buildable.coq_lang_version < (0, 7)
    then Memo.return Rocq_mode.VoOnly
    else
      let* coqc = coqc ~sctx ~dir ~loc:buildable.loc in
      let+ config = Rocq_config.make ~coqc in
      (match config with
       | Error _ -> Rocq_mode.VoOnly
       | Ok config ->
         (match Rocq_config.by_name config "rocq_native_compiler_default" with
          | Some (String "yes") | Some (String "ondemand") -> Rocq_mode.Native
          | _ -> Rocq_mode.VoOnly))
;;

let rocq_env =
  let f =
    Env_stanza_db_flags.flags
      ~name:"rocq-env"
      ~root:(fun _ _ -> Memo.return (Action_builder.return Rocq_flags.default))
      ~f:(fun ~parent expander (config : Dune_env.config) ->
        Memo.return
        @@
        let open Action_builder.O in
        let+ rocq_flags =
          let standard =
            let+ x = Action_builder.of_memo_join parent in
            x.rocq_flags
          in
          Expander.expand_and_eval_set expander (Rocq_env.flags config.rocq) ~standard
        and+ rocqdep_flags =
          let standard =
            let+ x = Action_builder.of_memo_join parent in
            x.rocqdep_flags
          in
          Expander.expand_and_eval_set
            expander
            (Rocq_env.rocqdep_flags config.rocq)
            ~standard
        and+ rocqdoc_flags =
          let standard =
            let+ x = Action_builder.of_memo_join parent in
            x.rocqdoc_flags
          in
          Expander.expand_and_eval_set
            expander
            (Rocq_env.rocqdoc_flags config.rocq)
            ~standard
        in
        { Rocq_flags.rocq_flags; rocqdep_flags; rocqdoc_flags })
  in
  fun ~dir ->
    (let* () = Memo.return () in
     (Staged.unstage f) dir)
    |> Action_builder.of_memo_join
;;

let rocq_flags ~dir ~stanza_flags ~per_file_flags ~expander =
  let standard, flags_to_expand =
    match per_file_flags with
    | None ->
      ( Action_builder.map
          ~f:(fun { Rocq_flags.rocq_flags; _ } -> rocq_flags)
          (rocq_env ~dir)
      , stanza_flags )
    | Some per_file_flags ->
      ( Action_builder.bind
          ~f:(fun { Rocq_flags.rocq_flags; _ } ->
            let standard = Action_builder.return rocq_flags in
            Expander.expand_and_eval_set expander stanza_flags ~standard)
          (rocq_env ~dir)
      , per_file_flags )
  in
  Expander.expand_and_eval_set expander flags_to_expand ~standard
;;

let rocqdep_flags ~dir ~stanza_rocqdep_flags ~expander =
  Expander.expand_and_eval_set
    expander
    stanza_rocqdep_flags
    ~standard:
      (Action_builder.map
         ~f:(fun { Rocq_flags.rocqdep_flags; _ } -> rocqdep_flags)
         (rocq_env ~dir))
;;

let rocqdoc_flags ~dir ~stanza_rocqdoc_flags ~expander =
  Expander.expand_and_eval_set
    expander
    stanza_rocqdoc_flags
    ~standard:
      (Action_builder.map
         ~f:(fun { Rocq_flags.rocqdoc_flags; _ } -> rocqdoc_flags)
         (rocq_env ~dir))
;;

let theory_rocqc_flag lib =
  let name = Rocq_lib_name.wrapper (Rocq_lib.name lib) in
  let dir = Rocq_lib.obj_root lib in
  let binding_flag = if Rocq_lib.implicit lib then "-R" else "-Q" in
  Command.Args.S [ A binding_flag; Path dir; A name ]
;;

let theories_flags ~theories_deps =
  Resolve.Memo.args
    (let open Resolve.Memo.O in
     let+ libs = theories_deps in
     Command.Args.S (List.map ~f:theory_rocqc_flag libs))
;;

module Bootstrap : sig
  type t =
    | Implicit (** Use implicit stdlib loading, for coq lang versions < 0.8 *)
    | No_stdlib
    (** We are in >= 0.8, however the user set stdlib = no
        , or we are compiling the prelude *)
    | Stdlib of Rocq_lib.t
    (** Regular case in >= 0.8 (or in < 0.8
                              (boot) was used *)

  val empty : t

  val make
    :  scope:Scope.t
    -> use_stdlib:bool
    -> wrapper_name:string
    -> coq_lang_version:Syntax.Version.t
    -> Rocq_module.t
    -> t Resolve.Memo.t

  val flags : t -> 'a Command.Args.t
end = struct
  type t =
    | Implicit (** Use implicit stdlib loading, for coq lang versions < 0.8 *)
    | No_stdlib
    (** We are in >= 0.8, however the user set stdlib = no
        , or we are compiling the prelude *)
    | Stdlib of Rocq_lib.t
    (** Regular case in >= 0.8 (or in < 0.8
                              (boot) was used *)

  (* For empty set of modules, we return Prelude which is kinda
     conservative. *)
  let empty = No_stdlib

  (* Hack to know if a module is a prelude module *)
  let check_init_module bootlib wrapper_name rocq_module =
    String.equal (Rocq_lib_name.wrapper (Rocq_lib.name bootlib)) wrapper_name
    && Option.equal
         String.equal
         (List.hd_opt (Rocq_module.prefix rocq_module))
         (Some "Init")
  ;;

  (* [Bootstrap.t] determines, for a concrete Rocq module, how the Rocq
     "standard library" is being handled. See the main modes above. *)
  let make ~scope ~use_stdlib ~wrapper_name ~coq_lang_version rocq_module =
    let open Resolve.Memo.O in
    let* boot_lib =
      Scope.rocq_libs scope
      |> Resolve.Memo.lift_memo
      >>= Rocq_lib.DB.resolve_boot ~coq_lang_version
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
                   "Couldn't find Rocq standard library, and theory is not using (stdlib \
                    no)"
               ])
        else Resolve.Memo.return Implicit
      | Some (_loc, boot_lib) ->
        Resolve.Memo.return
        @@
        (* TODO: replace with per_file flags *)
        let init = check_init_module boot_lib wrapper_name rocq_module in
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

let rocqc_file_flags ~dir ~theories_deps ~wrapper_name ~ml_flags : _ Command.Args.t list =
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
  @@ Util.resolve_first lib_db [ "rocq-runtime.kernel" ]
;;

let directories_of_lib ~sctx lib =
  let name = Rocq_lib.name lib in
  match lib with
  | Rocq_lib.Dune lib ->
    let dir = Rocq_lib.Dune.src_root lib in
    let* dir_contents = Dir_contents.get sctx ~dir in
    let+ rocq_sources = Dir_contents.rocq dir_contents in
    Rocq_sources.directories rocq_sources ~name
  | Rocq_lib.Legacy _ ->
    (* TODO: we could return this if we don't restrict ourselves to
       Path.Build.t here.

       EJGA: We need to understand how this interacts with globally
       installed stuff, that's more tricky than it looks actually!

       This function is used in order to determine the -nI flags that
       Rocq native compiler will pass to OCaml so it can find the .cmxs
       files. For things in user-contrib we don't need to pass these
       flags but only because Rocq has a very large hack adding this
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

let rocqc_native_flags ~sctx ~dir ~theories_deps ~theory_dirs ~(mode : Rocq_mode.t) =
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
  let f (t : Rocq_lib.t) =
    match t with
    | Dune t -> Left t
    | Legacy t -> Right t
  in
  let dune_theories, legacy_theories = List.partition_map ~f theories in
  let* dlibs =
    Resolve.List.concat_map ~f:Rocq_lib.Dune.libraries dune_theories |> Resolve.Memo.lift
  in
  let libs = libs @ dlibs in
  let+ findlib_libs = Lib.closure ~linking:false (List.map ~f:snd libs) in
  findlib_libs, legacy_theories
;;

(* compute include flags and mlpack rules *)
let ml_pack_and_meta_rule ~context ~all_libs (buildable : Rocq_stanza.Buildable.t)
  : unit Action_builder.t
  =
  (* rocqdep expects an mlpack file next to the sources otherwise it will
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
      (buildable : Rocq_stanza.Buildable.t)
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

let setup_rocqdep_for_theory_rule
      ~sctx
      ~dir
      ~loc
      ~theories_deps
      ~wrapper_name
      ~source_rule
      ~ml_flags
      ~mlpack_rule
      ~boot_flags
      ~stanza_rocqdep_flags
      rocq_modules
  =
  (* rocqdep needs the full source + plugin's mlpack to be present :( *)
  let sources = List.rev_map ~f:Rocq_module.source rocq_modules in
  let file_flags =
    [ Command.Args.S (rocqc_file_flags ~dir ~theories_deps ~wrapper_name ~ml_flags)
    ; As [ "-dyndep"; "opt"; "-vos" ]
    ; Deps sources
    ]
  in
  let extra_rocqdep_flags =
    (* Standard flags for rocqdep *)
    let open Action_builder.O in
    let* expander = Action_builder.of_memo @@ Super_context.expander sctx ~dir in
    rocqdep_flags ~dir ~stanza_rocqdep_flags ~expander
  in
  let rocqdep_flags =
    Command.Args.Dyn boot_flags :: Command.Args.dyn extra_rocqdep_flags :: file_flags
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
  (* Rocqdep has to be called in the stanza's directory *)
  Super_context.add_rule
    ~loc
    sctx
    ~dir
    (let open Action_builder.With_targets.O in
     Action_builder.with_no_targets mlpack_rule
     >>> Action_builder.(with_no_targets (goal source_rule))
     >>> Command.run ~dir:(Path.build dir) ~stdout_to coqdep rocqdep_flags)
;;

module Dep_map = Stdune.Map.Make (Path)

let rocqdep_invalid phase line =
  Code_error.raise
    "coqdep returned invalid output"
    [ "phase", Dyn.string phase; "line", Dyn.string line ]
;;

(* Handle the case where the path contains ":" and rocqdep escapes this
   as "\:" causing Dune to misinterpret the path. We revert
   the escaping, which allows dune to work on Windows.

   Note that rocqdep escapes a few more things, including spaces, $, #,
   [], ?, %, homedir... How to handle that seems tricky.
*)
let escaped_colon = Re.compile (Re.str "\\:")
let unescape_rocqdep string = Re.replace_string escaped_colon ~by:":" string

let parse_line ~dir line =
  match String.lsplit2 line ~on:':' with
  | None -> rocqdep_invalid "split" line
  | Some (basename, deps) ->
    (* This should always have a file, but let's handle the error
       properly *)
    let target =
      match String.extract_blank_separated_words basename with
      | [] -> rocqdep_invalid "target" line
      | vo :: _ -> vo
    in
    (* let depname, ext = Filename.split_extension ff in *)
    let target = Path.relative (Path.build dir) target in
    (* EJGA: XXX using `String.extract_blank_separated_words` works
       for OCaml, but not for Rocq as we don't use `-modules` *)
    let deps = unescape_rocqdep deps |> String.extract_blank_separated_words in
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
    "rocq_dep_map"
    ~input:(module DWInput)
    (fun (dir, wrapper_name) -> get_dep_map ~dir ~wrapper_name)
;;

let deps_of ~dir ~boot_type ~wrapper_name ~mode rocq_module =
  let open Action_builder.O in
  let vo_target =
    let ext =
      match mode with
      | Rocq_mode.VosOnly -> ".vos"
      | _ -> ".vo"
    in
    Path.set_extension ~ext (Rocq_module.source rocq_module)
  in
  let* dep_map = Action_builder.exec_memo memo_get_dep_map (dir, wrapper_name) in
  let* boot_type = Resolve.Memo.read boot_type in
  match Dep_map.find dep_map vo_target with
  | None ->
    Code_error.raise
      "Dep_map.find failed for"
      [ "rocq_module", Rocq_module.to_dyn rocq_module
      ; "dep_map", Dep_map.to_dyn (Dyn.list Path.to_dyn) dep_map
      ]
  | Some deps ->
    (* Inject prelude deps *)
    let deps =
      let prelude = "Init/Prelude.vo" in
      match boot_type with
      | Bootstrap.No_stdlib | Bootstrap.Implicit -> deps
      | Bootstrap.Stdlib lib -> Path.relative (Rocq_lib.obj_root lib) prelude :: deps
    in
    Action_builder.paths deps
;;

let generic_rocq_args
      ~sctx
      ~dir
      ~wrapper_name
      ~boot_flags
      ~per_file_flags
      ~mode
      ~rocq_prog
      ~stanza_flags
      ~ml_flags
      ~theories_deps
      ~theory_dirs
      rocq_module
  =
  let+ rocq_stanza_flags =
    let+ expander = Super_context.expander sctx ~dir in
    let rocq_flags =
      let rocq_flags = rocq_flags ~expander ~dir ~stanza_flags ~per_file_flags in
      (* By default we have the -q flag. We don't want to pass this to rocqtop to
         allow users to load their .rocqrc files for interactive development.
         Therefore we manually scrub the -q setting when passing arguments to
         rocqtop. *)
      match rocq_prog with
      | `Rocqtop ->
        let rec remove_q = function
          | "-q" :: l -> remove_q l
          | x :: l -> x :: remove_q l
          | [] -> []
        in
        let open Action_builder.O in
        rocq_flags >>| remove_q
      | _ -> rocq_flags
    in
    Command.Args.dyn rocq_flags (* stanza flags *)
  in
  let rocq_native_flags =
    let mode =
      (* Tweak the modes for rocqtop since it has no "-vos" option *)
      match mode, rocq_prog with
      | Rocq_mode.VosOnly, `Rocqtop -> Rocq_mode.VoOnly
      | _ -> mode
    in
    rocqc_native_flags ~sctx ~dir ~theories_deps ~theory_dirs ~mode
  in
  let file_flags = rocqc_file_flags ~dir ~theories_deps ~wrapper_name ~ml_flags in
  match rocq_prog with
  | `Rocqc ->
    [ rocq_stanza_flags
    ; rocq_native_flags
    ; Dyn boot_flags
    ; S file_flags
    ; Dep (Rocq_module.source rocq_module)
    ]
  | `Rocqtop -> [ rocq_stanza_flags; rocq_native_flags; Dyn boot_flags; S file_flags ]
;;

module Per_file = struct
  let match_ map rocq_module =
    let open Option.O in
    let* map = map in
    let rocq_lib_name = Rocq_module.name rocq_module in
    Rocq_module.Name.Map.find map rocq_lib_name
  ;;
end

let setup_rocqc_rule
      ~scope
      ~loc
      ~dir
      ~sctx
      ~rocqc_dir
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
      rocq_module
  =
  (* Process rocqdep and generate rules *)
  let boot_type =
    Bootstrap.make ~scope ~use_stdlib ~wrapper_name ~coq_lang_version rocq_module
  in
  let boot_flags = Resolve.Memo.read boot_type |> Action_builder.map ~f:Bootstrap.flags in
  (* TODO: merge with boot_type *)
  let per_file_flags = Per_file.match_ modules_flags rocq_module in
  let* coqc = coqc ~loc ~dir ~sctx in
  let obj_files =
    Rocq_module.obj_files
      ~wrapper_name
      ~mode
      ~obj_files_mode:Rocq_module.Build
      ~obj_dir:dir
      rocq_module
    |> List.map ~f:fst
  in
  let target_obj_files = Command.Args.Hidden_targets obj_files in
  let* args =
    generic_rocq_args
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
      ~rocq_prog:`Rocqc
      rocq_module
  in
  let deps_of = deps_of ~dir ~boot_type ~wrapper_name ~mode rocq_module in
  let open Action_builder.With_targets.O in
  Super_context.add_rule
    ~loc
    ~dir
    sctx
    (Action_builder.with_no_targets deps_of
     >>> Action_builder.With_targets.add ~file_targets
         @@ Command.run ~dir:(Path.build rocqc_dir) coqc (target_obj_files :: args)
     (* The way we handle the transitive dependencies of .vo files is not safe for
        sandboxing *)
     >>| Action.Full.add_sandbox Sandbox_config.no_sandboxing)
;;

let rocq_modules_of_theory ~sctx lib =
  Action_builder.of_memo
  @@
  let name = Rocq_lib.name lib in
  match lib with
  | Rocq_lib.Legacy lib -> Memo.return @@ Rocq_lib.Legacy.vo lib
  | Rocq_lib.Dune lib ->
    let dir = Rocq_lib.Dune.src_root lib in
    let* dir_contents = Dir_contents.get sctx ~dir in
    let+ rocq_sources = Dir_contents.rocq dir_contents in
    Rocq_sources.library rocq_sources ~name |> List.rev_map ~f:Rocq_module.source
;;

let source_rule ~sctx theories =
  (* sources for depending libraries rocqdep requires all the files to be in the
     tree to produce correct dependencies, including those of dependencies *)
  Action_builder.dyn_paths_unit
    (let open Action_builder.O in
     let+ l = Action_builder.List.map theories ~f:(rocq_modules_of_theory ~sctx) in
     List.concat l)
;;

let setup_rocqdoc_rules ~sctx ~dir ~theories_deps (s : Rocq_stanza.Theory.t) rocq_modules =
  let loc, name = s.buildable.loc, snd s.name in
  let rule =
    let file_flags =
      (* BUG: We need to pass --rocqlib depending on the boot_type otherwise
         rocqdoc will not work. *)
      [ theories_flags ~theories_deps
      ; A "-R"
      ; Path (Path.build dir)
      ; A (Rocq_lib_name.wrapper (snd s.name))
      ]
    in
    fun mode ->
      let* () =
        let* rocqdoc =
          Super_context.resolve_program_memo
            sctx
            "coqdoc"
            ~dir
            ~where:Original_path
            ~loc:(Some loc)
            ~hint:"opam install coq"
        in
        (let doc_dir = Rocq_doc.rocqdoc_directory ~mode ~obj_dir:dir ~name in
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
           let extra_rocqdoc_flags =
             (* Standard flags for rocqdoc *)
             let open Action_builder.O in
             let* expander = Action_builder.of_memo @@ Super_context.expander sctx ~dir in
             let standard =
               rocqdoc_flags ~dir ~stanza_rocqdoc_flags:s.rocqdoc_flags ~expander
             in
             Expander.expand_and_eval_set expander s.rocqdoc_flags ~standard
           in
           [ Command.Args.S file_flags
           ; Command.Args.dyn extra_rocqdoc_flags
           ; A mode_flag
           ; A "-d"
           ; Path (Path.build doc_dir)
           ; Deps (List.map ~f:Rocq_module.source rocq_modules)
           ; Dyn globs
           ; Hidden_deps
               (Dep.Set.of_files
                @@ List.map ~f:Path.build
                @@ List.map ~f:(Rocq_module.glob_file ~obj_dir:dir) rocq_modules)
           ]
         in
         Command.run
           ~sandbox:Sandbox_config.needs_sandboxing
           ~dir:(Path.build dir)
           rocqdoc
           file_flags
         |> Action_builder.With_targets.map
              ~f:
                (Action.Full.map ~f:(fun rocqdoc ->
                   Action.Progn [ Action.mkdir doc_dir; rocqdoc ]))
         |> Action_builder.With_targets.add_directories ~directory_targets:[ doc_dir ])
        |> Super_context.add_rule ~loc ~dir sctx
      in
      let alias =
        match mode with
        | `Html -> Alias.make Alias0.doc ~dir
        | `Latex -> Alias.make (Alias.Name.of_string "doc-latex") ~dir
      in
      Rocq_doc.rocqdoc_directory ~mode ~obj_dir:dir ~name
      |> Path.build
      |> Action_builder.path
      |> Rules.Produce.Alias.add_deps alias ~loc
  in
  rule `Html >>> rule `Latex
;;

(* Common context for a theory, deps and rules *)
let theory_context ~context ~scope ~coq_lang_version ~name buildable =
  let theory =
    let* rocq_lib_db = Scope.rocq_libs scope in
    Rocq_lib.DB.resolve rocq_lib_db ~coq_lang_version name
  in
  let theories_deps =
    Resolve.Memo.bind theory ~f:(fun theory ->
      Resolve.Memo.lift @@ Rocq_lib.theories_closure theory)
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
      (buildable : Rocq_stanza.Buildable.t)
  =
  let rocq_lib_db = Scope.rocq_libs scope in
  let theories_deps =
    let* rocq_lib_db = rocq_lib_db in
    Resolve.Memo.List.map
      buildable.theories
      ~f:(Rocq_lib.DB.resolve rocq_lib_db ~coq_lang_version)
  in
  (* Extraction requires a boot library so we do this unconditionally
     for now. We must do this because it can happen that
     s.buildable.theories is empty *)
  let boot =
    let* rocq_lib_db = rocq_lib_db in
    Rocq_lib.DB.resolve_boot ~coq_lang_version rocq_lib_db
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

let setup_theory_rules ~sctx ~dir ~dir_contents (s : Rocq_stanza.Theory.t) =
  let* scope = Scope.DB.find_by_dir dir in
  let coq_lang_version = s.buildable.coq_lang_version in
  let name = s.name in
  let theory, theories_deps, ml_flags, mlpack_rule =
    let context = Super_context.context sctx |> Context.name in
    theory_context ~context ~scope ~coq_lang_version ~name s.buildable
  in
  let wrapper_name = Rocq_lib_name.wrapper (snd s.name) in
  let use_stdlib = s.buildable.use_stdlib in
  let name = snd s.name in
  let loc = s.buildable.loc in
  let stanza_flags = s.buildable.flags in
  let modules_flags = Option.map s.modules_flags ~f:Rocq_module.Name.Map.of_list_exn in
  let* rocq_dir_contents = Dir_contents.rocq dir_contents in
  let theory_dirs =
    Rocq_sources.directories rocq_dir_contents ~name |> Path.Build.Set.of_list
  in
  let rocq_modules = Rocq_sources.library rocq_dir_contents ~name in
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
  let rocqc_dir = Super_context.context sctx |> Context.build_dir in
  let* mode = select_native_mode ~sctx ~dir s.buildable in
  (* First we setup the rule calling rocqdep *)
  let boot_type =
    (* If rocq_modules are empty it doesn't really matter, so we take
       the more conservative path and pass -boot, we don't care here
       about -noinit as rocqdep ignores it *)
    match rocq_modules with
    | [] -> Resolve.Memo.return Bootstrap.empty
    | m :: _ -> Bootstrap.make ~scope ~use_stdlib ~wrapper_name ~coq_lang_version m
  in
  let boot_flags = Resolve.Memo.read boot_type |> Action_builder.map ~f:Bootstrap.flags in
  setup_rocqdep_for_theory_rule
    ~sctx
    ~dir
    ~loc
    ~theories_deps
    ~wrapper_name
    ~source_rule
    ~ml_flags
    ~mlpack_rule
    ~boot_flags
    ~stanza_rocqdep_flags:s.rocqdep_flags
    rocq_modules
  >>> Memo.parallel_iter
        rocq_modules
        ~f:
          (setup_rocqc_rule
             ~scope
             ~loc
             ~dir
             ~sctx
             ~file_targets:[]
             ~stanza_flags
             ~modules_flags
             ~rocqc_dir
             ~theories_deps
             ~mode
             ~wrapper_name
             ~use_stdlib
             ~ml_flags
             ~coq_lang_version
             ~theory_dirs)
  (* And finally the rocqdoc rules *)
  >>> setup_rocqdoc_rules ~sctx ~dir ~theories_deps s rocq_modules
;;

let rocqtop_args_theory ~sctx ~dir ~dir_contents (s : Rocq_stanza.Theory.t) rocq_module =
  let* scope = Scope.DB.find_by_dir dir in
  let coq_lang_version = s.buildable.coq_lang_version in
  let name = s.name in
  let _theory, theories_deps, ml_flags, _mlpack_rule =
    let context = Super_context.context sctx |> Context.name in
    theory_context ~context ~scope ~coq_lang_version ~name s.buildable
  in
  let wrapper_name = Rocq_lib_name.wrapper (snd s.name) in
  let* mode = select_native_mode ~sctx ~dir s.buildable in
  let name = snd s.name in
  let use_stdlib = s.buildable.use_stdlib in
  let boot_type =
    Bootstrap.make ~scope ~use_stdlib ~wrapper_name ~coq_lang_version rocq_module
  in
  let* rocq_dir_contents = Dir_contents.rocq dir_contents in
  let theory_dirs =
    Rocq_sources.directories rocq_dir_contents ~name |> Path.Build.Set.of_list
  in
  let boot_flags = Resolve.Memo.read boot_type |> Action_builder.map ~f:Bootstrap.flags in
  let per_file_flags = None in
  generic_rocq_args
    ~sctx
    ~dir
    ~wrapper_name
    ~boot_flags
    ~per_file_flags
    ~mode
    ~rocq_prog:`Rocqtop
    ~stanza_flags:s.buildable.flags
    ~ml_flags
    ~theories_deps
    ~theory_dirs
    rocq_module
;;

(******************************************************************************)
(* Install rules *)
(******************************************************************************)

(* This is here for compatibility with Rocq < 8.11, which expects plugin files to
   be in the folder containing the `.vo` files *)
let rocq_plugins_install_rules ~scope ~package ~dst_dir (s : Rocq_stanza.Theory.t) =
  let lib_db = Scope.libs scope in
  let+ ml_libs =
    (* get_libraries from Rocq's ML dependencies *)
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
        (* Safe because all rocq libraries are local for now *)
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
  | { Rocq_stanza.Theory.package = None; _ } -> Memo.return []
  | { Rocq_stanza.Theory.package = Some package; buildable; _ } ->
    let loc = s.buildable.loc in
    let* mode = select_native_mode ~sctx ~dir buildable in
    let* scope = Scope.DB.find_by_dir dir in
    (* We force the rocq scope for this DB as to fail early in case of
       some library conflicts that would also generate conflicting
       install rules. This is needed as now install rules less lazy
       than the theory rules. *)
    let _ = Scope.rocq_libs scope in
    let* dir_contents = Dir_contents.get sctx ~dir in
    let name = snd s.name in
    (* This must match the wrapper prefix for now to remain compatible *)
    let dst_suffix = Rocq_lib_name.dir name in
    (* These are the rules for now, rocq lang 2.0 will make this uniform *)
    let dst_dir =
      if s.boot
      then (* We drop the "Corelib" prefix (!) *)
        Path.Local.of_string "coq/theories"
      else (
        let rocq_root = Path.Local.of_string "coq/user-contrib" in
        Path.Local.relative rocq_root dst_suffix)
    in
    (* Also, stdlib plugins are handled in a hardcoded way, so no compat install
       is needed *)
    let* rocq_plugins_install_rules =
      if s.boot
      then Memo.return []
      else rocq_plugins_install_rules ~scope ~package ~dst_dir s
    in
    let wrapper_name = Rocq_lib_name.wrapper name in
    let to_path f = Path.reach ~from:(Path.build dir) (Path.build f) in
    let to_dst f = Path.Local.to_string @@ Path.Local.relative dst_dir f in
    let make_entry (orig_file : Path.Build.t) (dst_file : string) =
      let entry =
        Install.Entry.make Section.Lib_root ~dst:(to_dst dst_file) orig_file ~kind:`File
      in
      Install.Entry.Sourced.create ~loc entry
    in
    let+ rocq_sources = Dir_contents.rocq dir_contents in
    rocq_sources
    |> Rocq_sources.library ~name
    |> List.concat_map ~f:(fun (vfile : Rocq_module.t) ->
      let obj_files =
        Rocq_module.obj_files
          ~wrapper_name
          ~mode
          ~obj_dir:dir
          ~obj_files_mode:Rocq_module.Install
          vfile
        |> List.map ~f:(fun ((vo_file : Path.Build.t), (install_vo_file : string)) ->
          make_entry vo_file install_vo_file)
      in
      let vfile = Rocq_module.source vfile |> Path.as_in_build_dir_exn in
      let vfile_dst = to_path vfile in
      make_entry vfile vfile_dst :: obj_files)
    |> List.rev_append rocq_plugins_install_rules
;;

let setup_rocqpp_rules ~sctx ~dir ({ loc; modules } : Rocq_stanza.Rocqpp.t) =
  let* coqpp =
    Super_context.resolve_program_memo
      sctx
      "coqpp"
      ~where:Original_path
      ~dir
      ~loc:(Some loc)
      ~hint:"opam install rocq"
  and* mlg_files = Rocq_sources.mlg_files ~sctx ~dir ~modules in
  let mlg_rule m =
    let source = Path.build m in
    let target = Path.Build.set_extension m ~ext:".ml" in
    let args = [ Command.Args.Dep source; Hidden_targets [ target ] ] in
    let build_dir = Super_context.context sctx |> Context.build_dir in
    Command.run ~dir:(Path.build build_dir) coqpp args
  in
  List.rev_map ~f:mlg_rule mlg_files |> Super_context.add_rules ~loc ~dir sctx
;;

let setup_extraction_rules ~sctx ~dir ~dir_contents (s : Rocq_stanza.Extraction.t) =
  let wrapper_name = "DuneExtraction" in
  let* rocq_module =
    let+ rocq = Dir_contents.rocq dir_contents in
    Rocq_sources.extract rocq s
  in
  let file_targets =
    Rocq_stanza.Extraction.ml_target_fnames s |> List.map ~f:(Path.Build.relative dir)
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
    source_rule ~sctx theories_deps
    >>> Action_builder.path (Rocq_module.source rocq_module)
  in
  let* mode = select_native_mode ~sctx ~dir s.buildable in
  let boot_type =
    (* If rocq_modules are empty it doesn't really matter, so we take
       the more conservative path and pass -boot, we don't care here
       about -noinit as rocqdep ignores it *)
    match [ rocq_module ] with
    | [] -> Resolve.Memo.return Bootstrap.empty
    | m :: _ -> Bootstrap.make ~scope ~use_stdlib ~wrapper_name ~coq_lang_version m
  in
  let boot_flags = Resolve.Memo.read boot_type |> Action_builder.map ~f:Bootstrap.flags in
  let modules_flags = None in
  setup_rocqdep_for_theory_rule
    ~sctx
    ~dir
    ~loc
    ~theories_deps
    ~wrapper_name
    ~source_rule
    ~ml_flags
    ~mlpack_rule
    ~boot_flags
    ~stanza_rocqdep_flags:Ordered_set_lang.Unexpanded.standard
    [ rocq_module ]
  >>> setup_rocqc_rule
        ~scope
        ~file_targets
        ~stanza_flags:s.buildable.flags
        ~modules_flags
        ~sctx
        ~loc
        ~rocqc_dir:dir
        rocq_module
        ~dir
        ~theories_deps
        ~mode
        ~wrapper_name
        ~use_stdlib:s.buildable.use_stdlib
        ~ml_flags
        ~coq_lang_version
        ~theory_dirs:Path.Build.Set.empty
;;

let rocqtop_args_extraction ~sctx ~dir (s : Rocq_stanza.Extraction.t) rocq_module =
  let use_stdlib = s.buildable.use_stdlib in
  let coq_lang_version = s.buildable.coq_lang_version in
  let* scope = Scope.DB.find_by_dir dir in
  let theories_deps, ml_flags, _mlpack_rule =
    let context = Super_context.context sctx |> Context.name in
    extraction_context ~context ~scope ~coq_lang_version s.buildable
  in
  let wrapper_name = "DuneExtraction" in
  let boot_type =
    Bootstrap.make ~scope ~use_stdlib ~wrapper_name ~coq_lang_version rocq_module
  in
  let boot_flags = Resolve.Memo.read boot_type |> Action_builder.map ~f:Bootstrap.flags in
  let per_file_flags = None in
  let* mode = select_native_mode ~sctx ~dir s.buildable in
  generic_rocq_args
    ~sctx
    ~dir
    ~wrapper_name
    ~boot_flags
    ~per_file_flags
    ~mode
    ~rocq_prog:`Rocqtop
    ~stanza_flags:s.buildable.flags
    ~ml_flags
    ~theories_deps
    ~theory_dirs:Path.Build.Set.empty
    rocq_module
;;

(* Version for export *)
let deps_of ~dir ~use_stdlib ~wrapper_name ~mode ~coq_lang_version rocq_module =
  let boot_type =
    let open Memo.O in
    let* scope = Scope.DB.find_by_dir dir in
    Bootstrap.make ~scope ~use_stdlib ~wrapper_name ~coq_lang_version rocq_module
  in
  deps_of ~dir ~boot_type ~wrapper_name ~mode rocq_module
;;
