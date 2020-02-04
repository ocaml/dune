open Import

let mdx_version_required = "1.6.0"

module Files = struct
  type t =
    { src : Path.Build.t
    ; deps : Path.Build.t
    ; corrected : Path.Build.t
    }

  let corrected_file build_path =
    Path.Build.extend_basename ~suffix:".corrected" build_path

  let deps_file build_path =
    Path.Build.extend_basename ~suffix:".mdx.deps" build_path

  let from_source_file src =
    let deps = deps_file src in
    let corrected = corrected_file src in
    { src; deps; corrected }
end

module Deps = struct
  type t =
    | File of string
    | Dir of string

  let parse_one s =
    match String.lsplit2 ~on:':' s with
    | Some ("dir", dir) -> Ok (Dir dir)
    | Some ("file", file) -> Ok (File file)
    | Some (_, _)
    | None ->
      Result.errorf "Unknown 'ocaml-mdx deps' item: %s" s

  let parse lines =
    match lines with
    | [ "" ] -> Ok []
    | [ line ] ->
      let items = String.split ~on:' ' line in
      Result.List.map ~f:parse_one items
    | _ ->
      Result.errorf
        "Invalid 'ocaml-mdx deps' output should a single line with space \
         separated items"

  let read (files : Files.t) =
    let open Build.O in
    let+ lines = Build.lines_of (Path.build files.deps) in
    match parse lines with
    | Ok deps -> deps
    | Error msg ->
      User_error.raise
        [ Pp.textf "Mdx dependencies for %s could not be interpreted:"
            (Path.to_string_maybe_quoted (Path.build files.src))
        ; Pp.text msg
        ; Pp.textf "Please make sure you are using mdx.%s or higher"
            mdx_version_required
        ]

  let rule ~dir ~mdx_prog files =
    Command.run ~dir:(Path.build dir) mdx_prog
      [ A "deps"; Dep (Path.build files.Files.src) ]
      ~stdout_to:files.Files.deps

  let to_path ~dir str =
    let local = Path.Local.of_string str in
    let build = Path.Build.append_local dir local in
    Path.build build

  let dirs_and_files ~dir t_list =
    List.partition_map t_list ~f:(function
      | Dir d -> Left (to_path ~dir d)
      | File f -> Right (to_path ~dir f))

  let dir_without_files_dep =
    let pred =
      Predicate.create ~id:(lazy (String "false")) ~f:(fun _ -> false)
    in
    fun dir -> Dep.file_selector (File_selector.create ~dir pred)

  let source_tree_dep_set dir =
    let prefix_with, dir = Path.extract_build_context_dir_exn dir in
    match File_tree.find_dir dir with
    | None -> Dep.Set.empty
    | Some dir ->
      File_tree.Dir.fold dir ~init:Dep.Set.empty
        ~traverse:Sub_dirs.Status.Set.all ~f:(fun dir acc ->
          let files = File_tree.Dir.files dir in
          let path = Path.append_source prefix_with (File_tree.Dir.path dir) in
          match String.Set.is_empty files with
          | true -> Dep.Set.add acc (dir_without_files_dep path)
          | false ->
            let paths =
              String.Set.fold files ~init:Path.Set.empty ~f:(fun fn acc ->
                  Path.Set.add acc (Path.relative path fn))
            in
            Dep.Set.add_paths acc paths)

  let to_dep_set ~dir t_list =
    let dirs, files = dirs_and_files ~dir t_list in
    let dep_set = Dep.Set.of_files files in
    List.fold_left dirs ~init:dep_set ~f:(fun acc dir ->
        Dep.Set.union acc (source_tree_dep_set dir))
end

type t =
  { loc : Loc.t
  ; files : Predicate_lang.Glob.t
  ; packages : Package.Name.t list
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
     and+ packages = field_o "packages" (repeat Package.Name.decode) in
     let packages = Option.value ~default:[] packages in
     { loc; files; packages })

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

(** Generates the rules for a single [src] file covered covered by the given
    [stanza]. *)
let gen_rules_for_single_file ~sctx ~dir ~mdx_prog ~stanza src =
  let open Build.O in
  let loc = stanza.loc in
  let files = Files.from_source_file src in
  (* Add the rule for generating the .mdx.deps file with ocaml-mdx deps *)
  Super_context.add_rule sctx ~loc ~dir (Deps.rule ~dir ~mdx_prog files);
  (* Add the rule for generating the .corrected file using ocaml-mdx test *)
  let mdx_action =
    let open Build.With_targets.O in
    let deps = Build.map (Deps.read files) ~f:(Deps.to_dep_set ~dir) in
    let dyn_deps = Build.map deps ~f:(fun d -> ((), d)) in
    let pkg_deps =
      let context = Super_context.context sctx in
      List.map stanza.packages ~f:(fun pkg ->
          Build.alias (Build_system.Alias.package_install ~context ~pkg))
    in
    Build.(with_no_targets (all_unit pkg_deps))
    >>> Build.with_no_targets (Build.dyn_deps dyn_deps)
    >>> Command.run ~dir:(Path.build dir) mdx_prog
          [ A "test"
          ; A "-o"
          ; Target files.corrected
          ; Dep (Path.build files.src)
          ]
  in
  Super_context.add_rule sctx ~loc ~dir mdx_action;
  (* Attach the diff action to the @runtest for the src and corrected files *)
  let diff_action =
    let+ () = Build.path (Path.build files.src)
    and+ () = Build.path (Path.build files.corrected) in
    Action.diff ~optional:false (Path.build files.src)
      (Path.build files.corrected)
  in
  Super_context.add_alias_action sctx (Alias.runtest ~dir) ~loc:(Some loc) ~dir
    ~stamp:("mdx", files.src)
    (Build.with_no_targets diff_action)

(** Generates the rules for a given mdx stanza *)
let gen_rules ~sctx ~dir t =
  let files_to_mdx = files_to_mdx ~sctx ~dir t in
  let mdx_prog =
    Super_context.resolve_program sctx ~dir ~loc:(Some t.loc)
      ~hint:"opam install mdx" "ocaml-mdx"
  in
  List.iter files_to_mdx
    ~f:(gen_rules_for_single_file ~sctx ~dir ~mdx_prog ~stanza:t)
