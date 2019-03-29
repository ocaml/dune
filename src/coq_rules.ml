(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(*     Written by: Emilio Jesús Gallego Arias  *)

open! Stdune
open Build.O
module SC = Super_context

let coq_debug = false

type coq_context =
  { coqdep : Action.program
  ; coqc   : Action.program
  ; coqpp  : Action.program
  }

let parse_coqdep ~coq_module (lines : string list) =
  if coq_debug then Format.eprintf "Parsing coqdep @\n%!";
  let source = Coq_module.source coq_module in
  let invalid p =
    Errors.die "coqdep returned invalid output for %s / [phase: %s]"
      (Path.to_string source) p in
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
      Coq_module.(String.concat ~sep:"/" (prefix coq_module @ [name coq_module])) in
    if coq_debug
    then Format.eprintf "depname / modname: %s / %s@\n%!" depname modname;
    if depname <> modname then invalid "basename";
    let deps = String.extract_blank_separated_words deps in
    if coq_debug
    then Format.eprintf "deps for %a: %a@\n%!" Path.pp source Fmt.(list text) deps;
    deps

let setup_rule ~expander ~dir ~cc ~source_rule ~name ~cflags coq_module =

  if coq_debug
  then Format.eprintf "gen_rule coq_module: %a@\n%!" Coq_module.pp coq_module;
  let obj_dir = dir in
  let source    = Coq_module.source coq_module in
  let stdout_to = Coq_module.obj_file ~obj_dir ~ext:".v.d" coq_module in
  let object_to = Coq_module.obj_file ~obj_dir ~ext:".vo"  coq_module in

  let iflags = Arg_spec.As ["-R"; "."; name] in
  let cd_arg = Arg_spec.[ iflags; Dep source ] in

  (* coqdep needs the full source to be present :( *)
  let coqdep_rule =
    source_rule >>>
    Build.run ~dir ~stdout_to cc.coqdep cd_arg
  in

  (* Process coqdep and generate rules *)
  let deps_of = Build.dyn_paths (
    Build.lines_of stdout_to >>^
    parse_coqdep ~coq_module >>^
    List.map ~f:(Path.relative dir)
  ) in
  let cc_arg = Arg_spec.[
    iflags;
    Dep source;
    Hidden_targets [object_to] ]
  in
  [coqdep_rule;
   deps_of >>>
   Expander.expand_and_eval_set expander cflags ~standard:(Build.return []) >>>
   Build.run ~dir cc.coqc (Dyn (fun flags -> As flags) :: cc_arg)
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

let setup_rules ~sctx ~dir ~dir_contents (s : Dune_file.Coq.t) =

  if coq_debug then begin
    let scope = SC.find_scope_by_dir sctx dir in
    Format.eprintf "[gen_rules] @[dir: %a@\nscope: %a@]@\n%!"
      Path.pp dir Path.pp (Scope.root scope)
  end;

  let cc = create_ccoq sctx ~dir in
  let name = snd s.name in
  let coq_modules = Dir_contents.coq_modules_of_library dir_contents ~name in

  (* coqdep requires all the files to be in the tree to produce correct
     dependencies *)
  let source_rule = Build.paths (List.map ~f:Coq_module.source coq_modules) in
  let cflags = s.Dune_file.Coq.flags in
  let expander = SC.expander sctx ~dir in
  let coq_rules =
    List.concat_map
      ~f:(setup_rule ~expander ~dir ~cc ~source_rule ~name ~cflags) coq_modules in
  coq_rules
