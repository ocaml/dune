(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(*     Written by: Emilio Jesús Gallego Arias  *)

open! Stdune
module SC = Super_context

let coq_debug = false

(* Coqdep / Coq expect the deps to the directory where the plugin cmxs
   file are. This seems to correspond to src_dir. *)
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
  ; coqc   : Action.program
  ; coqpp  : Action.program
  }

let parse_coqdep ~coq_module (lines : string list) =
  if coq_debug then Format.eprintf "Parsing coqdep @\n%!";
  let source = Coq_module.source coq_module in
  let invalid p =
    User_error.raise
      [ Pp.textf "coqdep returned invalid output for %s / [phase: %s]"
          (Path.Build.to_string_maybe_quoted source) p
      ]
  in
  let line =
    match lines with
    | [] | _ :: _ :: _ :: _ -> invalid "line"
    | [line] -> line
    | [l1;_l2] ->
      (* .vo is produced before .vio, this is fragile tho *)
      l1
  in
  match String.lsplit2 line ~on:':' with
  | None -> invalid "split"
  | Some (basename,deps) ->
    let ff = List.hd @@ String.extract_blank_separated_words basename in
    let depname, _ = Filename.split_extension ff in
    let modname =
      Coq_module.(String.concat ~sep:"/"
                    (prefix coq_module @ [name coq_module])) in
    if coq_debug
    then Format.eprintf "depname / modname: %s / %s@\n%!" depname modname;
    if depname <> modname then invalid "basename";
    let deps = String.extract_blank_separated_words deps in
    if coq_debug
    then Format.eprintf "deps for %a: %a@\n%!"
           Path.Build.pp source Fmt.(list text) deps;
    deps

let setup_rule ~expander ~dir ~cc ~source_rule ~coq_flags ~file_flags
      ~mlpack_rule coq_module =

  if coq_debug
  then Format.eprintf "gen_rule coq_module: %a@\n%!"
         Dyn.pp (Coq_module.to_dyn coq_module);

  let obj_dir = dir in
  let source    = Coq_module.source coq_module in
  let stdout_to = Coq_module.obj_file ~obj_dir ~ext:".v.d" coq_module in
  let object_to = Coq_module.obj_file ~obj_dir ~ext:".vo" coq_module in
  let dir = Path.build dir in

  let file_flags = file_flags @ [Command.Args.Dep (Path.build source)] in
  let cd_arg = (Command.Args.As ["-dyndep"; "opt"]) :: file_flags in

  (* coqdep needs the full source + plugin's mlpack to be present :( *)
  let coqdep_rule =
    (* This is weird stuff in order to adapt the rule so we can reuse
       ml_iflags :( I wish we had more flexible typing. *)

  Build.S.seqs [mlpack_rule; source_rule]
    (Command.run ~dir ~stdout_to cc.coqdep cd_arg)
  in

  (* Process coqdep and generate rules *)

  let deps_of : unit Build.s = Build.dyn_paths (
    Build.S.map (Build.lines_of (Path.build stdout_to))
      ~f:(fun x -> List.map ~f:(Path.relative dir) (parse_coqdep ~coq_module x))
  ) in

  let cc_arg = (Command.Args.Hidden_targets [object_to]) :: file_flags in

  (* Rules for the files *)
  [coqdep_rule;
   Build.S.seq deps_of (
    let coq_flags = Expander.expand_and_eval_set expander coq_flags ~standard:(Build.return []) in
    Command.run ~dir cc.coqc (Command.Args.dyn coq_flags :: cc_arg))
  ]

(* TODO: remove; rgrinberg points out:
   - resolve program is actually cached,
   - better just to ask for values that we actually use.
 *)
let create_ccoq sctx ~dir =
  let rr prg =
    SC.resolve_program ~dir sctx prg ~loc:None ~hint:"try: opam install coq" in
  { coqdep = rr "coqdep"
  ; coqc   = rr "coqc"
  ; coqpp  = rr "coqpp"
  }

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
      ] in
    List.concat_map plugins ~f:to_mlpack
  in

  (* Pair of include flags and paths to mlpack *)
  let ml_iflags, mlpack =
    let libs = libs_of_coq_deps ~lib_db libs
               |> Lib.closure ~linking:false |> Result.ok_exn in
    Util.include_flags libs, List.concat_map ~f:ml_pack_files libs
  in

  (* If the mlpack files don't exist, don't fail *)
  ml_iflags, Build.S.ignore (Build.paths_existing mlpack)

let coqlib_wrapper_name (s : Dune_file.Coq.t) =
  Lib_name.Local.to_string (snd s.name)

let setup_rules ~sctx ~dir ~dir_contents (s : Dune_file.Coq.t) =
  let scope = SC.find_scope_by_dir sctx dir in
  let cc = create_ccoq sctx ~dir in
  let expander = SC.expander sctx ~dir in

  if coq_debug then begin
    Format.eprintf "[gen_rules] @[dir: %a@\nscope: %a@]@\n%!"
      Path.Build.pp dir Path.Build.pp (Scope.root scope)
  end;

  let name = Dune_file.Coq.best_name s in
  let coq_modules = Dir_contents.coq_modules_of_library dir_contents ~name in

  (* coqdep requires all the files to be in the tree to produce correct
     dependencies *)
  let source_rule =
    Build.paths (List.map coq_modules ~f:(fun m ->
      Path.build (Coq_module.source m))) in
  let coq_flags = s.flags in
  let wrapper_name = coqlib_wrapper_name s in

  let lib_db = Scope.libs scope in
  let ml_iflags, mlpack_rule = setup_ml_deps ~lib_db s.libraries in
  let file_flags = [ml_iflags; Command.Args.As ["-R"; "."; wrapper_name]] in

  List.concat_map
    ~f:(setup_rule ~expander ~dir ~cc ~source_rule ~coq_flags ~file_flags
          ~mlpack_rule) coq_modules

(* This is here for compatibility with Coq < 8.11, which expects
   plugin files to be in the folder containing the `.vo` files *)
let coq_plugins_install_rules ~scope ~package ~dst_dir (s : Dune_file.Coq.t) =
  let lib_db = Scope.libs scope in
  let ml_libs = libs_of_coq_deps ~lib_db s.libraries in
  let rules_for_lib lib =
    (* Don't install libraries that don't belong to this package *)
    if Option.equal Package.Name.equal
         (Lib.package lib) (Some (package.Package.name))
    then
      let info = Lib.info lib in
      let plugins = Lib_info.plugins info in
      Mode.Dict.get plugins Mode.Native
      |> List.map ~f:(fun plugin_file ->
        let plugin_file = Path.as_in_build_dir_exn plugin_file in
        let plugin_file_basename = Path.Build.basename plugin_file in
        let dst =
          Path.Local.(to_string (relative dst_dir plugin_file_basename)) in
        None, Install.(Entry.make Section.Lib_root ~dst plugin_file))
    else []
  in
  List.concat_map ~f:rules_for_lib ml_libs

let install_rules ~sctx ~dir s =
  match s with
  | { Dune_file.Coq. public = None; _ } ->
    []
  | { Dune_file.Coq. public = Some { package; _ } ; _ } ->
    let scope = SC.find_scope_by_dir sctx dir in
    let dir_contents =
      Dir_contents.get sctx ~dir
    in
    let name = Dune_file.Coq.best_name s in
    (* This is the usual root for now, Coq + Dune will change it! *)
    let coq_root = Path.Local.of_string "coq/user-contrib" in
    (* This must match the wrapper prefix for now to remain compatible *)
    let dst_suffix = coqlib_wrapper_name s in
    let dst_dir = Path.Local.relative coq_root dst_suffix in
    Dir_contents.coq_modules_of_library dir_contents ~name
    |> List.map ~f:(fun (vfile : Coq_module.t) ->
      let vofile =
        Coq_module.obj_file ~obj_dir:dir ~ext:".vo" vfile
      in
      let vofile_rel =
        Path.reach ~from:(Path.build dir) (Path.build vofile)
      in
      let dst = Path.Local.relative dst_dir vofile_rel in
      None, Install.(Entry.make Section.Lib_root
                       ~dst:(Path.Local.to_string dst) vofile))
    |> List.rev_append (coq_plugins_install_rules ~scope ~package ~dst_dir s)

let coqpp_rules ~sctx ~build_dir ~dir (s : Dune_file.Coqpp.t) =
  let cc = create_ccoq sctx ~dir in

  let mlg_rule m =
    let source = Path.build (Path.Build.relative dir (m ^ ".mlg")) in
    let target = Path.Build.relative dir (m ^ ".ml") in
    let args = [Command.Args.Dep source; Hidden_targets [target]] in
    Command.run ~dir:(Path.build build_dir) cc.coqpp args in

  List.map ~f:mlg_rule s.modules
