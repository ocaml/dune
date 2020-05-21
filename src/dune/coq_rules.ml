(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2020                              *)
(* Written by: Emilio Jesús Gallego Arias *)

open! Stdune
open Coq_stanza
module SC = Super_context

let coq_debug = false

(* Coqdep / Coq expect the deps to the directory where the plugin cmxs file are.
   This seems to correspond to src_dir. *)
module Util = struct
  let include_paths ts =
    Path.Set.of_list_map ts ~f:(fun t ->
        let info = Lib.info t in
        Lib_info.src_dir info)

  let include_flags ts = include_paths ts |> Lib.L.to_iflags

  (* coqdep expects an mlpack file next to the sources otherwise it
   * will omit the cmxs deps *)
  let ml_pack_files lib =
    let plugins =
      let info = Lib.info lib in
      let plugins = Lib_info.plugins info in
      Mode.Dict.get plugins Mode.Native
    in
    let to_mlpack file =
      [ Path.set_extension file ~ext:".mlpack"
      ; Path.set_extension file ~ext:".mllib"
      ]
    in
    List.concat_map plugins ~f:to_mlpack
end

let resolve_program sctx ~loc ~dir prog =
  SC.resolve_program ~dir sctx prog ~loc:(Some loc)
    ~hint:"try: opam install coq"

module Bootstrap = struct
  (* the internal boot flag determines if the Coq "standard library" is being
     built, in case we need to explictly tell Coq where the build artifacts are
     and add `Init.Prelude.vo` as a dependency; there is a further special case
     when compiling the prelude, in this case we also need to tell Coq not to
     try to load the prelude. *)
  type t =
    | No_boot  (** Coq's stdlib is installed globally *)
    | Bootstrap of Coq_lib.t
        (** Coq's stdlib is in scope of the composed build *)
    | Bootstrap_prelude
        (** We are compiling the prelude itself
            [should be replaced with (per_file ...) flags] *)

  let get ~boot_lib ~wrapper_name coq_module =
    match boot_lib with
    | None -> No_boot
    | Some (_loc, lib) -> (
      (* This is here as an optimization, TODO; replace with per_file flags *)
      let init =
        String.equal (Coq_lib.wrapper lib) wrapper_name
        && Option.equal String.equal
             (List.hd_opt (Coq_module.prefix coq_module))
             (Some "Init")
      in
      match init with
      | false -> Bootstrap lib
      | true -> Bootstrap_prelude )

  let flags =
    let open Command in
    function
    | No_boot -> []
    | Bootstrap _lib -> [ Args.A "-boot" ]
    | Bootstrap_prelude -> [ Args.As [ "-boot"; "-noinit" ] ]
end

(* get_libraries from Coq's ML dependencies *)
let libs_of_coq_deps ~lib_db = Result.List.map ~f:(Lib.DB.resolve lib_db)

module Context = struct
  type 'a t =
    { coqdep : Action.Prog.t
    ; coqc : Action.Prog.t * Path.Build.t
    ; wrapper_name : string
    ; dir : Path.Build.t
    ; expander : Expander.t
    ; buildable : Buildable.t
    ; theories_deps : Coq_lib.t list Or_exn.t
    ; mlpack_rule : unit Build.t
    ; ml_flags : 'a Command.Args.t
    ; scope : Scope.t
    ; boot_type : Bootstrap.t
    ; build_dir : Path.Build.t
    }

  let coqc ?stdout_to t args =
    let dir = Path.build (snd t.coqc) in
    Command.run ~dir ?stdout_to (fst t.coqc) args

  let coq_flags t =
    Expander.expand_and_eval_set t.expander t.buildable.flags
      ~standard:(Build.return [])

  let theories_flags =
    let setup_theory_flag lib =
      let wrapper = Coq_lib.wrapper lib in
      let dir = Coq_lib.src_root lib in
      let binding_flag =
        if Coq_lib.implicit lib then
          "-R"
        else
          "-Q"
      in
      [ Command.Args.A binding_flag; Path (Path.build dir); A wrapper ]
    in
    fun t ->
      Command.of_result_map t.theories_deps ~f:(fun libs ->
          Command.Args.S (List.concat_map libs ~f:setup_theory_flag))

  let coqc_file_flags cctx =
    let file_flags =
      [ cctx.ml_flags
      ; theories_flags cctx
      ; Command.Args.A "-R"
      ; Path (Path.build cctx.dir)
      ; A cctx.wrapper_name
      ]
    in
    [ Command.Args.S (Bootstrap.flags cctx.boot_type); S file_flags ]

  (* compute include flags and mlpack rules *)
  let setup_ml_deps ~lib_db libs theories =
    (* Pair of include flags and paths to mlpack *)
    let libs =
      let open Result.O in
      let* theories = theories in
      let libs = libs @ List.concat_map ~f:Coq_lib.libraries theories in
      let* libs = libs_of_coq_deps ~lib_db libs in
      Lib.closure ~linking:false libs
    in
    ( Command.of_result_map libs ~f:Util.include_flags
    , Build.of_result_map libs ~f:(fun libs ->
          (* If the mlpack files don't exist, don't fail *)
          Build.paths_existing (List.concat_map ~f:Util.ml_pack_files libs)) )

  let create ~coqc_dir sctx ~dir ~wrapper_name ~theories_deps
      (buildable : Buildable.t) =
    let loc = buildable.loc in
    let rr = resolve_program sctx ~dir ~loc in
    let expander = Super_context.expander sctx ~dir in
    let scope = SC.find_scope_by_dir sctx dir in
    let lib_db = Scope.libs scope in
    (* ML-level flags for depending libraries *)
    let ml_flags, mlpack_rule =
      setup_ml_deps ~lib_db buildable.libraries theories_deps
    in
    let build_dir = Super_context.build_dir sctx in
    { coqdep = rr "coqdep"
    ; coqc = (rr "coqc", coqc_dir)
    ; wrapper_name
    ; dir
    ; expander
    ; buildable
    ; theories_deps
    ; mlpack_rule
    ; ml_flags
    ; scope
    ; boot_type = Bootstrap.No_boot
    ; build_dir
    }

  let for_module t coq_module =
    let boot_lib = t.scope |> Scope.coq_libs |> Coq_lib.DB.boot_library in
    let boot_type =
      Bootstrap.get ~boot_lib ~wrapper_name:t.wrapper_name coq_module
    in
    { t with boot_type }
end

let parse_coqdep ~dir ~(boot_type : Bootstrap.t) ~coq_module
    (lines : string list) =
  if coq_debug then Format.eprintf "Parsing coqdep @\n%!";
  let source = Coq_module.source coq_module in
  let invalid phase =
    User_error.raise
      [ Pp.textf "coqdep returned invalid output for %s / [phase: %s]"
          (Path.Build.to_string_maybe_quoted source)
          phase
      ; Pp.verbatim (String.concat ~sep:"\n" lines)
      ]
  in
  let line =
    match lines with
    | []
    | _ :: _ :: _ :: _ ->
      invalid "line"
    | [ line ] -> line
    | [ l1; _l2 ] ->
      (* .vo is produced before .vio, this is fragile tho *)
      l1
  in
  match String.lsplit2 line ~on:':' with
  | None -> invalid "split"
  | Some (basename, deps) -> (
    let ff = List.hd @@ String.extract_blank_separated_words basename in
    let depname, _ = Filename.split_extension ff in
    let modname =
      String.concat ~sep:"/"
        Coq_module.(
          prefix coq_module @ [ Coq_module.Name.to_string (name coq_module) ])
    in
    if coq_debug then
      Format.eprintf "depname / modname: %s / %s@\n%!" depname modname;
    if depname <> modname then invalid "basename";
    let deps = String.extract_blank_separated_words deps in
    if coq_debug then
      Format.eprintf "deps for %s: %a@\n%!"
        (Path.Build.to_string source)
        (Format.pp_print_list Format.pp_print_string)
        deps;
    (* Add prelude deps for when stdlib is in scope and we are not actually
       compiling the prelude *)
    let deps = List.map ~f:(Path.relative (Path.build dir)) deps in
    match boot_type with
    | No_boot
    | Bootstrap_prelude ->
      deps
    | Bootstrap lib ->
      Path.relative (Path.build (Coq_lib.src_root lib)) "Init/Prelude.vo"
      :: deps )

let deps_of ~dir ~boot_type coq_module =
  let stdout_to = Coq_module.dep_file ~obj_dir:dir coq_module in
  Build.dyn_paths_unit
    (Build.map
       (Build.lines_of (Path.build stdout_to))
       ~f:(parse_coqdep ~dir ~boot_type ~coq_module))

let coqdep_rule (cctx : _ Context.t) ~source_rule ~file_flags coq_module =
  (* coqdep needs the full source + plugin's mlpack to be present :( *)
  let source = Coq_module.source coq_module in
  let file_flags =
    [ Command.Args.S file_flags
    ; As [ "-dyndep"; "opt" ]
    ; Dep (Path.build source)
    ]
  in
  let stdout_to = Coq_module.dep_file ~obj_dir:cctx.dir coq_module in
  (* Coqdep has to be called in the stanza's directory *)
  let open Build.With_targets.O in
  Build.with_no_targets cctx.mlpack_rule
  >>> Build.with_no_targets source_rule
  >>> Command.run ~dir:(Path.build cctx.dir) ~stdout_to cctx.coqdep file_flags

let coqc_rule (cctx : _ Context.t) ~file_flags coq_module =
  let source = Coq_module.source coq_module in
  let file_flags =
    let object_to = Coq_module.obj_file ~obj_dir:cctx.dir coq_module in
    [ Command.Args.Hidden_targets [ object_to ]
    ; S file_flags
    ; Command.Args.Dep (Path.build source)
    ]
  in
  let open Build.With_targets.O in
  (* The way we handle the transitive dependencies of .vo files is not safe for
     sandboxing *)
  Build.with_no_targets
    (Build.dep (Dep.sandbox_config Sandbox_config.no_sandboxing))
  >>>
  let coq_flags = Context.coq_flags cctx in
  Context.coqc cctx (Command.Args.dyn coq_flags :: file_flags)

module Module_rule = struct
  type t =
    { coqdep : Action.t Build.With_targets.t
    ; coqc : Action.t Build.With_targets.t
    }
end

let setup_rule cctx ~source_rule coq_module =
  let open Build.With_targets.O in
  if coq_debug then
    Format.eprintf "gen_rule coq_module: %a@\n%!" Pp.render_ignore_tags
      (Dyn.pp (Coq_module.to_dyn coq_module));

  let file_flags = Context.coqc_file_flags cctx in

  let coqdep_rule = coqdep_rule cctx ~source_rule ~file_flags coq_module in

  (* Process coqdep and generate rules *)
  let deps_of = deps_of ~dir:cctx.dir ~boot_type:cctx.boot_type coq_module in

  (* Rules for the files *)
  { Module_rule.coqdep = coqdep_rule
  ; coqc =
      Build.with_no_targets deps_of >>> coqc_rule cctx ~file_flags coq_module
  }

let coq_modules_of_theory ~sctx lib =
  let name = Coq_lib.name lib in
  let dir = Coq_lib.src_root lib in
  let dir_contents = Dir_contents.get sctx ~dir in
  let coq_sources = Dir_contents.coq dir_contents in
  Coq_sources.library coq_sources ~name

let source_rule ~sctx theories =
  (* sources for depending libraries coqdep requires all the files to be in the
     tree to produce correct dependencies, including those of dependencies *)
  Build.of_result_map theories ~f:(fun theories ->
      List.concat_map theories ~f:(coq_modules_of_theory ~sctx)
      |> List.rev_map ~f:(fun m -> Path.build (Coq_module.source m))
      |> Build.paths)

let setup_rules ~sctx ~dir ~dir_contents (s : Theory.t) =
  let name = snd s.name in
  let scope = SC.find_scope_by_dir sctx dir in
  let coq_lib_db = Scope.coq_libs scope in
  let theory = Coq_lib.DB.resolve coq_lib_db s.name |> Result.ok_exn in

  let cctx =
    let wrapper_name = Coq_lib.wrapper theory in
    (* Coq flags for depending libraries *)
    let theories_deps = Coq_lib.DB.requires coq_lib_db theory in
    let coqc_dir = Super_context.build_dir sctx in
    Context.create sctx ~coqc_dir ~dir ~wrapper_name ~theories_deps s.buildable
  in

  (* List of modules to compile for this library *)
  let coq_modules =
    let coq = Dir_contents.coq dir_contents in
    Coq_sources.library coq ~name
  in

  let source_rule =
    let theories =
      let open Result.O in
      let+ theories = cctx.theories_deps in
      theory :: theories
    in
    source_rule ~sctx theories
  in
  List.concat_map coq_modules ~f:(fun m ->
      let cctx = Context.for_module cctx m in
      let { Module_rule.coqc; coqdep } = setup_rule cctx ~source_rule m in
      [ coqc; coqdep ])

(******************************************************************************)
(* Install rules *)
(******************************************************************************)

(* This is here for compatibility with Coq < 8.11, which expects plugin files to
   be in the folder containing the `.vo` files *)
let coq_plugins_install_rules ~scope ~package ~dst_dir (s : Theory.t) =
  let lib_db = Scope.libs scope in
  let ml_libs =
    libs_of_coq_deps ~lib_db s.buildable.libraries |> Result.ok_exn
  in
  let rules_for_lib lib =
    let info = Lib.info lib in
    (* Don't install libraries that don't belong to this package *)
    if
      Option.equal Package.Name.equal (Lib_info.package info)
        (Some package.Package.name)
    then
      let loc = Lib_info.loc info in
      let plugins = Lib_info.plugins info in
      Mode.Dict.get plugins Mode.Native
      |> List.map ~f:(fun plugin_file ->
             (* Safe because all coq libraries are local for now *)
             let plugin_file = Path.as_in_build_dir_exn plugin_file in
             let plugin_file_basename = Path.Build.basename plugin_file in
             let dst =
               Path.Local.(to_string (relative dst_dir plugin_file_basename))
             in
             (Some loc, Install.(Entry.make Section.Lib_root ~dst plugin_file)))
    else
      []
  in
  List.concat_map ~f:rules_for_lib ml_libs

let install_rules ~sctx ~dir s =
  match s with
  | { Theory.package = None; _ } -> []
  | { Theory.package = Some package; _ } ->
    let loc = s.buildable.loc in
    let scope = SC.find_scope_by_dir sctx dir in
    let dir_contents = Dir_contents.get sctx ~dir in
    let name = snd s.name in
    (* This must match the wrapper prefix for now to remain compatible *)
    let dst_suffix = Coq_lib_name.dir (snd s.name) in
    (* These are the rules for now, coq lang 2.0 will make this uniform *)
    let dst_dir =
      if s.boot then
        (* We drop the "Coq" prefix (!) *)
        Path.Local.of_string "coq/theories"
      else
        let coq_root = Path.Local.of_string "coq/user-contrib" in
        Path.Local.relative coq_root dst_suffix
    in
    (* Also, stdlib plugins are handled in a hardcoded way, so no compat install
       is needed *)
    let coq_plugins_install_rules =
      if s.boot then
        []
      else
        coq_plugins_install_rules ~scope ~package ~dst_dir s
    in
    Dir_contents.coq dir_contents
    |> Coq_sources.library ~name
    |> List.concat_map ~f:(fun (vfile : Coq_module.t) ->
           let to_path f = Path.reach ~from:(Path.build dir) (Path.build f) in
           let to_dst f =
             Path.Local.to_string @@ Path.Local.relative dst_dir f
           in
           let vofile = Coq_module.obj_file ~obj_dir:dir vfile in
           let vfile = Coq_module.source vfile in
           let make_entry file =
             ( Some loc
             , Install.(
                 Entry.make Section.Lib_root ~dst:(to_dst (to_path file)) file)
             )
           in
           [ make_entry vfile; make_entry vofile ])
    |> List.rev_append coq_plugins_install_rules

let coqpp_rules ~sctx ~dir (s : Coqpp.t) =
  let coqpp = resolve_program sctx ~dir ~loc:s.loc "coqpp" in
  let mlg_rule m =
    let source = Path.build (Path.Build.relative dir (m ^ ".mlg")) in
    let target = Path.Build.relative dir (m ^ ".ml") in
    let args = [ Command.Args.Dep source; Hidden_targets [ target ] ] in
    Command.run ~dir:(Path.build (Super_context.build_dir sctx)) coqpp args
  in
  List.map ~f:mlg_rule s.modules

let extraction_rules ~sctx ~dir ~dir_contents (s : Extraction.t) =
  let cctx =
    let wrapper_name = "DuneExtraction" in
    let theories_deps =
      let scope = SC.find_scope_by_dir sctx dir in
      let coq_lib_db = Scope.coq_libs scope in
      Coq_lib.DB.requires_for_user_written coq_lib_db s.buildable.theories
    in
    Context.create sctx ~coqc_dir:dir ~dir ~wrapper_name ~theories_deps
      s.buildable
  in
  let coq_module =
    let coq = Dir_contents.coq dir_contents in
    Coq_sources.extract coq s
  in
  let ml_targets =
    Extraction.ml_target_fnames s |> List.map ~f:(Path.Build.relative dir)
  in
  let source_rule =
    let theories = source_rule ~sctx cctx.theories_deps in
    let open Build.O in
    theories >>> Build.path (Path.build (Coq_module.source coq_module))
  in
  let { Module_rule.coqc; coqdep } = setup_rule cctx ~source_rule coq_module in
  let coqc = Build.With_targets.add coqc ~targets:ml_targets in
  [ coqdep; coqc ]
