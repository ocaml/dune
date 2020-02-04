open Import

type t =
  { loc : Loc.t
  ; files : Predicate_lang.Glob.t
  }

type Stanza.t += T of t

let syntax =
  let name = "mdx" in
  let desc = "mdx extension to verify code blocks in .md and .mli" in
  Dune_lang.Syntax.create ~name ~desc [ (0, 1) ]

let default_files =
  let has_extention ext s = String.equal ext (Filename.extension s) in
  let md_files = Predicate_lang.Glob.of_pred (has_extention ".md") in
  let mli_files = Predicate_lang.Glob.of_pred (has_extention ".mli") in
  Predicate_lang.union [ md_files; mli_files ]

let decode =
  let open Dune_lang.Decoder in
  fields
    (let+ loc = loc
     and+ files =
       field "files" Predicate_lang.Glob.decode ~default:default_files
     in
     { loc; files })

let () =
  let open Dune_lang.Decoder in
  let decode = Dune_lang.Syntax.since Stanza.syntax (2, 4) >>> decode in
  Dune_project.Extension.register_simple syntax
    (return [ ("mdx", decode >>| fun x -> [ T x ]) ])

(** Returns the list of files (in _build) to be passed to mdx for the given
    stanza and context *)
let files_to_mdx ~sctx ~dir t =
  let src_dir = Path.Build.drop_build_context_exn dir in
  let src_dir_files = Path.Source.Set.to_list (File_tree.files_of src_dir) in
  let must_mdx src_path =
    let file = Path.Source.basename src_path in
    Predicate_lang.Glob.exec t.files ~standard:default_files file
  in
  let build_path src_path =
    Path.Build.append_source (Super_context.build_dir sctx) src_path
  in
  List.filter_map src_dir_files ~f:(fun src_path ->
      if must_mdx src_path then
        Some (build_path src_path)
      else
        None)

let gen_rules ~sctx ~dir t =
  let files_to_mdx = files_to_mdx ~sctx ~dir t in
  let src_and_correcteds =
    List.map files_to_mdx ~f:(fun path ->
        (path, Path.Build.extend_basename ~suffix:".corrected" path))
  in
  let mdx_prog =
    Super_context.resolve_program sctx ~dir ~loc:(Some t.loc)
      ~hint:"opam install mdx" "ocaml-mdx"
  in
  (* Add the rule for generating the .corrected files using ocaml-mdx *)
  List.iter src_and_correcteds ~f:(fun (src, corrected) ->
      Super_context.add_rule sctx ~loc:t.loc ~dir
        (Command.run ~dir:(Path.build dir) mdx_prog
           [ A "test"; A "-o"; Target corrected; Dep (Path.build src) ]));
  (* Attach diff actions to @runtest for each (x, x.corrected) pair *)
  List.iter src_and_correcteds ~f:(fun (src, corrected) ->
      let open Build.O in
      let diff_action =
        let+ () = Build.path (Path.build src)
        and+ () = Build.path (Path.build corrected) in
        Action.diff ~optional:false (Path.build src) (Path.build corrected)
      in
      Super_context.add_alias_action sctx (Alias.runtest ~dir) ~loc:(Some t.loc)
        ~dir ~stamp:("mdx", src)
        (Build.with_no_targets diff_action))
