(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* Written by: Emilio Jesús Gallego Arias *)

open! Stdune
module SC = Super_context

let coq_debug = false

(* Coqdep / Coq expect the deps to the directory where the plugin cmxs file are.
   This seems to correspond to src_dir. *)
module Util = struct
  let include_paths ts =
    List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
        let info = Lib.info t in
        let src_dir = Lib_info.src_dir info in
        Path.Set.add acc src_dir)

  let include_flags ts = include_paths ts |> Lib.L.to_iflags
end

type coq_context =
  { coqdep : Action.program
  ; coqc : Action.program
  ; coqpp : Action.program
  }

(* the internal boot flag determines if the Coq "standard library" is being
   built, in case we need to explictly tell Coq where the build artifacts are
   and add `Init.Prelude.vo` as a dependency; there is a further special case
   when compiling the prelude, in this case we also need to tell Coq not to try
   to load the prelude. *)
type coq_bootstrap_type =
  | No_boot
  | Bootstrap
  | Bootstrap_prelude

let parse_coqdep ~boot_type ~coq_module (lines : string list) =
  if coq_debug then Format.eprintf "Parsing coqdep @\n%!";
  let source = Coq_module.source coq_module in
  let invalid p =
    User_error.raise
      [ Pp.textf "coqdep returned invalid output for %s / [phase: %s]"
          (Path.Build.to_string_maybe_quoted source)
          p
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
      Coq_module.(
        String.concat ~sep:"/" (prefix coq_module @ [ name coq_module ]))
    in
    if coq_debug then
      Format.eprintf "depname / modname: %s / %s@\n%!" depname modname;
    if depname <> modname then invalid "basename";
    let deps = String.extract_blank_separated_words deps in
    if coq_debug then
      Format.eprintf "deps for %a: %a@\n%!" Path.Build.pp source
        Fmt.(list text)
        deps;
    (* Add prelude deps for when stdlib is in scope and we are not actually
       compiling the prelude *)
    match boot_type with
    | No_boot
    | Bootstrap_prelude ->
      deps
    | Bootstrap -> "Init/Prelude.vo" :: deps )

let get_bootstrap_type ~boot coq_module =
  match boot with
  | false -> No_boot
  | true -> (
    (* This is inside as an optimization, TODO; replace with per_file flags *)
    let init =
      Option.equal String.equal
        (List.nth_opt (Coq_module.prefix coq_module) 0)
        (Some "Init")
    in
    match init with
    | false -> Bootstrap
    | true -> Bootstrap_prelude )

let flags_of_bootstrap_type ~boot_type =
  let open Command in
  match boot_type with
  | No_boot -> []
  | Bootstrap -> [ Args.A "-boot" ]
  | Bootstrap_prelude -> [ Args.As [ "-boot"; "-noinit" ] ]

let deps_of ~dir ~boot_type coq_module =
  let stdout_to = Coq_module.dep_file ~obj_dir:dir coq_module in
  Build.dyn_paths_unit
    (Build.map
       (Build.lines_of (Path.build stdout_to))
       ~f:(fun x ->
         List.map
           ~f:(Path.relative (Path.build dir))
           (parse_coqdep ~boot_type ~coq_module x)))

let coqdep_rule ~dir ~coqdep ~mlpack_rule ~source_rule ~file_flags coq_module =
  (* coqdep needs the full source + plugin's mlpack to be present :( *)
  let source = Coq_module.source coq_module in
  let file_flags =
    [ Command.Args.S file_flags
    ; As [ "-dyndep"; "opt" ]
    ; Dep (Path.build source)
    ]
  in
  let stdout_to = Coq_module.dep_file ~obj_dir:dir coq_module in
  let dir = Path.build dir in
  let open Build.With_targets.O in
  Build.with_no_targets mlpack_rule
  >>> Build.with_no_targets source_rule
  >>> Command.run ~dir ~stdout_to coqdep file_flags

let coqc_rule ~expander ~dir ~coqc ~coq_flags ~file_flags coq_module =
  let source = Coq_module.source coq_module in
  let file_flags =
    let object_to = Coq_module.obj_file ~obj_dir:dir coq_module in
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
  let coq_flags =
    Expander.expand_and_eval_set expander coq_flags ~standard:(Build.return [])
  in
  let dir = Path.build dir in
  Command.run ~dir coqc (Command.Args.dyn coq_flags :: file_flags)

let setup_rule ~expander ~dir ~cc ~source_rule ~coq_flags ~file_flags
    ~mlpack_rule ~boot coq_module =
  let open Build.With_targets.O in
  if coq_debug then
    Format.eprintf "gen_rule coq_module: %a@\n%!" Pp.render_ignore_tags
      (Dyn.pp (Coq_module.to_dyn coq_module));

  let boot_type = get_bootstrap_type ~boot coq_module in
  let file_flags =
    [ Command.Args.S (flags_of_bootstrap_type ~boot_type); S file_flags ]
  in

  let coqdep_rule =
    coqdep_rule ~dir ~coqdep:cc.coqdep ~mlpack_rule ~source_rule ~file_flags
      coq_module
  in

  (* Process coqdep and generate rules *)
  let deps_of = deps_of ~dir ~boot_type coq_module in

  (* Rules for the files *)
  [ coqdep_rule
  ; Build.with_no_targets deps_of
    >>> coqc_rule ~expander ~dir ~coqc:cc.coqc ~coq_flags ~file_flags coq_module
  ]

(* TODO: remove; rgrinberg points out: - resolve program is actually cached, -
   better just to ask for values that we actually use. *)
let create_ccoq sctx ~dir =
  let rr prg =
    SC.resolve_program ~dir sctx prg ~loc:None ~hint:"try: opam install coq"
  in
  { coqdep = rr "coqdep"; coqc = rr "coqc"; coqpp = rr "coqpp" }

(* get_libraries from Coq's ML dependencies *)
let libs_of_coq_deps ~lib_db libs =
  Result.List.map ~f:(Lib.DB.resolve lib_db) libs |> Result.ok_exn

(* compute include flags and mlpack rules *)
let setup_ml_deps ~lib_db libs =
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
  in
  (* Pair of include flags and paths to mlpack *)
  let ml_iflags, mlpack =
    let libs =
      libs_of_coq_deps ~lib_db libs
      |> Lib.closure ~linking:false |> Result.ok_exn
    in
    (Util.include_flags libs, List.concat_map ~f:ml_pack_files libs)
  in
  (* If the mlpack files don't exist, don't fail *)
  (ml_iflags, Build.paths_existing mlpack)

let coqlib_wrapper_name (s : Dune_file.Coq.t) =
  Coq_lib_name.wrapper (snd s.name)

let setup_rules ~sctx ~dir ~dir_contents (s : Dune_file.Coq.t) =
  let scope = SC.find_scope_by_dir sctx dir in
  let expander = SC.expander sctx ~dir in
  if coq_debug then
    Format.eprintf "[gen_rules] @[dir: %a@\nscope: %a@]@\n%!" Path.Build.pp dir
      Path.Build.pp (Scope.root scope);
  let cc = create_ccoq sctx ~dir in
  let name = snd s.name in
  let coq_modules =
    let coq = Dir_contents.coq dir_contents in
    Coq_sources.library coq ~name
  in
  (* coqdep requires all the files to be in the tree to produce correct
     dependencies *)
  let source_rule =
    Build.paths
      (List.map coq_modules ~f:(fun m -> Path.build (Coq_module.source m)))
  in
  let coq_flags = s.flags in
  let wrapper_name = coqlib_wrapper_name s in
  let lib_db = Scope.libs scope in
  let ml_iflags, mlpack_rule = setup_ml_deps ~lib_db s.libraries in
  let file_flags = [ ml_iflags; Command.Args.As [ "-R"; "."; wrapper_name ] ] in
  let boot = s.boot in
  List.concat_map
    ~f:
      (setup_rule ~expander ~dir ~cc ~source_rule ~coq_flags ~file_flags
         ~mlpack_rule ~boot)
    coq_modules

(* This is here for compatibility with Coq < 8.11, which expects plugin files to
   be in the folder containing the `.vo` files *)
let coq_plugins_install_rules ~scope ~package ~dst_dir (s : Dune_file.Coq.t) =
  let lib_db = Scope.libs scope in
  let ml_libs = libs_of_coq_deps ~lib_db s.libraries in
  let rules_for_lib lib =
    (* Don't install libraries that don't belong to this package *)
    if
      Option.equal Package.Name.equal (Lib.package lib)
        (Some package.Package.name)
    then
      let info = Lib.info lib in
      let plugins = Lib_info.plugins info in
      Mode.Dict.get plugins Mode.Native
      |> List.map ~f:(fun plugin_file ->
             let plugin_file = Path.as_in_build_dir_exn plugin_file in
             let plugin_file_basename = Path.Build.basename plugin_file in
             let dst =
               Path.Local.(to_string (relative dst_dir plugin_file_basename))
             in
             (None, Install.(Entry.make Section.Lib_root ~dst plugin_file)))
    else
      []
  in
  List.concat_map ~f:rules_for_lib ml_libs

let install_rules ~sctx ~dir s =
  match s with
  | { Dune_file.Coq.package = None; _ } -> []
  | { Dune_file.Coq.package = Some package; _ } ->
    let scope = SC.find_scope_by_dir sctx dir in
    let dir_contents = Dir_contents.get sctx ~dir in
    let name = snd s.name in
    (* This must match the wrapper prefix for now to remain compatible *)
    let dst_suffix = coqlib_wrapper_name s in
    (* These are the rules for now, coq lang 2.0 will make this uniform *)
    let dst_dir =
      if s.boot then
        (* We drop the "Coq" prefix (!) *)
        Path.Local.of_string "coq/theories"
      else
        let coq_root = Path.Local.of_string "coq/user-contrib" in
        Path.Local.relative coq_root dst_suffix
    in
    let coq_plugins_install_rules =
      if s.boot then
        []
      else
        coq_plugins_install_rules ~scope ~package ~dst_dir s
    in
    Dir_contents.coq dir_contents
    |> Coq_sources.library ~name
    |> List.map ~f:(fun (vfile : Coq_module.t) ->
           let vofile = Coq_module.obj_file ~obj_dir:dir vfile in
           let vofile_rel =
             Path.reach ~from:(Path.build dir) (Path.build vofile)
           in
           let dst = Path.Local.relative dst_dir vofile_rel in
           ( None
           , Install.(
               Entry.make Section.Lib_root ~dst:(Path.Local.to_string dst)
                 vofile) ))
    |> List.rev_append coq_plugins_install_rules

let coqpp_rules ~sctx ~build_dir ~dir (s : Dune_file.Coqpp.t) =
  let cc = create_ccoq sctx ~dir in
  let mlg_rule m =
    let source = Path.build (Path.Build.relative dir (m ^ ".mlg")) in
    let target = Path.Build.relative dir (m ^ ".ml") in
    let args = [ Command.Args.Dep source; Hidden_targets [ target ] ] in
    Command.run ~dir:(Path.build build_dir) cc.coqpp args
  in
  List.map ~f:mlg_rule s.modules
