open Import
open Memo.O

(* This module implements the first stage of evaluating dune files. In other
   words, only what is to traverse the source tree. The implementation is
   somewhat messy right now, but it can be described as the following pipeline:

   1. Determine whether the dune file is OCaml or dune syntax.

   2. If it's OCaml syntax, there's nothing we can do because the source tree
   cannot evaluate the script without a context. If it's dune syntax, decode it
   into the following AST:

   {[ type t =
     | Data_only_dirs of Predicate_lang.t
     | Vendored_dirs of Predicate_lang.t
     | Normal_dirs of Predicate_lang.t
     | Subdir of Path.Local.t * t list
     | Include of Path.Local.t
   }]

   3. Resolve and expand all the includes. This eliminates the [Include]
   constructor.

   4. Group all the stanzas to the sub directories they belong to. This
   eliminates the [Subdir] constructor.

   5. Evaluate the status stanzas into a combined map for every directory. This
   eliminates the remaining constructors.

   After the last step, we're left with [Dir_map.t]. Which contains all the
   expanded stanzas for the current dune file and all subdirectories.
*)

let fname = "dune"
let alternative_fname = "dune-file"

type kind =
  | Plain
  | Ocaml_script

let dyn_of_kind = function
  | Plain -> Dyn.variant "Plain" []
  | Ocaml_script -> Dyn.variant "Ocaml_script" []
;;

let equal_kind x y =
  match x, y with
  | Plain, Plain | Ocaml_script, Ocaml_script -> true
  | _, _ -> false
;;

module Dir_map = struct
  module Per_dir = struct
    type t =
      { sexps : Dune_lang.Ast.t list
      ; subdir_status : Source_dir_status.Spec.input
      }

    let to_dyn { sexps; subdir_status = _ } =
      let open Dyn in
      record
        [ "sexps", list Dune_lang.to_dyn (List.map ~f:Dune_lang.Ast.remove_locs sexps) ]
    ;;

    let equal { sexps; subdir_status } t =
      List.equal Dune_sexp.Ast.equal sexps t.sexps
      && Source_dir_status.Map.equal
           (Option.equal (Tuple.T2.equal Loc.equal Predicate_lang.Glob.equal))
           subdir_status
           t.subdir_status
    ;;

    let empty =
      { sexps = []; subdir_status = Source_dir_status.Map.init ~f:(fun _ -> None) }
    ;;

    let merge d1 d2 =
      { sexps = d1.sexps @ d2.sexps
      ; subdir_status =
          Source_dir_status.Map.merge d1.subdir_status d2.subdir_status ~f:(fun l r ->
            Option.merge l r ~f:(fun (loc, _) (loc2, _) ->
              let main_message = Pp.text "This stanza stanza was already specified at:" in
              let annots =
                let main = User_message.make ~loc [ main_message ] in
                let related =
                  [ User_message.make ~loc:loc2 [ Pp.text "Already defined here" ] ]
                in
                User_message.Annots.singleton
                  Compound_user_error.annot
                  [ Compound_user_error.make ~main ~related ]
              in
              User_error.raise
                ~loc
                ~annots
                [ main_message; Pp.verbatim (Loc.to_file_colon_line loc2) ]))
      }
    ;;
  end

  type t =
    { data : Per_dir.t
    ; nodes : t Filename.Map.t
    }

  let rec equal { data; nodes } t =
    Per_dir.equal data t.data && Filename.Map.equal ~equal nodes t.nodes
  ;;

  let rec to_dyn { data; nodes } =
    let open Dyn in
    record [ "data", Per_dir.to_dyn data; "nodes", Filename.Map.to_dyn to_dyn nodes ]
  ;;

  let empty = { data = Per_dir.empty; nodes = Filename.Map.empty }
  let root t = t.data
  let descend t (p : Filename.t) = Filename.Map.find t.nodes p
  let sub_dirs t = Filename.Map.keys t.nodes

  let rec make_at_path path data =
    match path with
    | [] -> data
    | x :: xs ->
      let nodes = Filename.Map.singleton x (make_at_path xs data) in
      { empty with nodes }
  ;;

  let singleton data = { empty with data }

  let rec merge t1 t2 : t =
    let data = Per_dir.merge t1.data t2.data in
    let nodes = Filename.Map.union t1.nodes t2.nodes ~f:(fun _ l r -> Some (merge l r)) in
    { data; nodes }
  ;;

  let merge_all = List.fold_left ~f:merge ~init:empty
end

let descendant_path =
  Dune_lang.Decoder.plain_string (fun ~loc fn ->
    if Filename.is_relative fn
    then Path.Local.parse_string_exn ~loc fn |> Path.Local.explode
    else (
      let msg = [ Pp.textf "invalid sub-directory path %S" fn ] in
      let hints = [ Pp.textf "sub-directory path must be relative" ] in
      User_error.raise ~loc ~hints msg))
;;

let strict_subdir field_name =
  let open Dune_lang.Decoder in
  plain_string (fun ~loc dn ->
    let msg = [ Pp.textf "invalid sub-directory name %S" dn ] in
    if Filename.dirname dn <> Filename.current_dir_name
    then (
      let msg = [ Pp.textf "only immediate sub-directories may be specified." ] in
      let hints =
        [ Pp.textf
            "to ignore %s, write \"(%s %s)\" in %s/dune"
            dn
            field_name
            (Filename.basename dn)
            (Filename.dirname dn)
        ]
      in
      User_error.raise ~loc ~hints msg)
    else if match dn with
            | "" | "." ->
              let hints = [ Pp.textf "did you mean (%s *)?" field_name ] in
              User_error.raise ~loc ~hints msg
            | ".." -> true
            | _ -> false
    then User_error.raise ~loc msg
    else loc, dn)
;;

let strict_subdir_glob field_name =
  let open Dune_lang.Decoder in
  let+ globs =
    repeat
      (let+ loc, l = strict_subdir field_name in
       Predicate_lang.Glob.of_glob (Glob.of_string_exn loc l))
  in
  Predicate_lang.or_ globs
;;

let decode =
  (* CR-rgrinberg: introduce an explicit AST to make this less opaque *)
  let make ~dirs ~data_only ~ignored_sub_dirs ~vendored_dirs =
    let data_only =
      match data_only, ignored_sub_dirs with
      | None, [] -> None
      | Some (loc, data_only), [] -> Some (loc, data_only)
      | None, (loc, _) :: _ ->
        let ignored_sub_dirs = List.map ~f:snd ignored_sub_dirs in
        Some (loc, Predicate_lang.or_ ignored_sub_dirs)
      | Some _data_only, _ :: _ -> assert false
    in
    { Source_dir_status.Map.normal = dirs; data_only; vendored = vendored_dirs }
  in
  let open Dune_lang.Decoder in
  let ignored_sub_dirs =
    let ignored =
      let+ l = enter (repeat (strict_subdir "ignored_sub_dirs")) in
      Predicate_lang.Glob.of_string_list (List.rev_map ~f:snd l)
    in
    let+ version = Dune_lang.Syntax.get_exn Stanza.syntax
    and+ loc, ignored = located ignored in
    if version >= (1, 6)
    then
      User_warning.emit
        ~loc
        [ Pp.text
            "ignored_subdirs is deprecated in 1.6. Use dirs to specify visible \
             directories or data_only_dirs for ignoring only dune files."
        ];
    ignored
  in
  let dirs =
    located (Dune_lang.Syntax.since Stanza.syntax (1, 6) >>> Predicate_lang.Glob.decode)
  in
  let data_only_dirs =
    located
      (Dune_lang.Syntax.since Stanza.syntax (1, 6) >>> strict_subdir_glob "data_only_dirs")
  in
  let vendored_dirs =
    (* let decode = Predicate_lang.Glob.decode in *)
    located
      (Dune_lang.Syntax.since Stanza.syntax (1, 11) >>> strict_subdir_glob "vendored_dirs")
  in
  let rec subdir () =
    let* () = Dune_lang.Syntax.since Stanza.syntax (2, 5) in
    let* subdir = descendant_path in
    let+ node = fields (decode ~allow_ignored_subdirs:false) in
    Dir_map.make_at_path subdir node
  and decode ~allow_ignored_subdirs =
    let+ dirs = field_o "dirs" dirs
    and+ data_only = field_o "data_only_dirs" data_only_dirs
    and+ ignored_sub_dirs =
      let parser =
        if allow_ignored_subdirs
        then ignored_sub_dirs
        else
          let+ loc = loc in
          User_error.raise
            ~loc
            [ Pp.textf "ignored_subdirs is not allowed under subdir. Use dirs instead" ]
      in
      multi_field "ignored_subdirs" (located parser)
    and+ vendored_dirs = field_o "vendored_dirs" vendored_dirs
    and+ subdirs = multi_field "subdir" (subdir ())
    and+ rest = leftover_fields in
    match data_only, dirs, ignored_sub_dirs with
    | _, Some (loc, _), _ :: _ ->
      User_error.raise
        ~loc
        [ Pp.text "Cannot have both dirs and ignored_subdirs stanza in a dune file. " ]
    | Some (loc, _), _, _ :: _ ->
      User_error.raise
        ~loc
        [ Pp.text
            "Cannot have both data_only_dirs and ignored_subdirs stanza in a dune file. "
        ]
    | _ ->
      Dir_map.merge_all
        (let subdir_status = make ~dirs ~data_only ~ignored_sub_dirs ~vendored_dirs in
         Dir_map.singleton { Dir_map.Per_dir.sexps = rest; subdir_status } :: subdirs)
  in
  enter (fields (decode ~allow_ignored_subdirs:true))
;;

type decoder = { decode : 'a. Dune_lang.Ast.t list -> 'a Dune_lang.Decoder.t -> 'a }

let decode_includes ~context (decoder : decoder) =
  let rec subdir ~loc:subdir_loc ~context ~path ~inside_include sexps =
    let name, path, nodes =
      decoder.decode sexps
      @@
      let open Dune_lang.Decoder in
      enter
      @@ let* dune_version = Dune_lang.Syntax.get_exn Stanza.syntax in
         let* name, path =
           plain_string (fun ~loc s ->
             Dune_lang.Ast.atom_or_quoted_string loc s, s :: path)
         in
         let+ nodes = repeat raw in
         let required_version = 2, 7 in
         if inside_include && dune_version < required_version
         then
           Dune_lang.Syntax.Error.since
             subdir_loc
             Stanza.syntax
             required_version
             ~what:"Using a `subdir' stanza within an `include'd file";
         name, path, nodes
    in
    let open Memo.O in
    let+ nodes =
      (* CR-rgrinberg: there's no strict need to resolve the includes for sub
         directories here. We could delay it until we actually need the stanzas
         for the sub directories. *)
      decode ~context ~path ~inside_include nodes
    in
    Dune_lang.Ast.List
      (subdir_loc, Atom (Loc.none, Dune_lang.Atom.of_string "subdir") :: name :: nodes)
  and decode ~context ~path ~inside_include (sexps : Dune_lang.Ast.t list) =
    let sexps, subdirs, includes =
      decoder.decode sexps
      @@
      let open Dune_lang.Decoder in
      enter
      @@ fields
      @@ let+ includes =
           multi_field
             "include"
             (let+ loc = loc
              and+ fn = relative_file in
              let fn =
                List.fold_left ~init:fn path ~f:(fun fn dir -> Filename.concat dir fn)
              in
              loc, fn)
         and+ subdirs =
           multi_field
             "subdir"
             (let+ loc = loc
              and+ stanzas = repeat raw in
              loc, stanzas)
         and+ sexps = leftover_fields in
         sexps, subdirs, includes
    in
    let open Memo.O in
    let+ includes, subdirs =
      Memo.fork_and_join
        (fun () ->
          Memo.parallel_map includes ~f:(fun (loc, fn) ->
            let* sexps, context = Include_stanza.load_sexps ~context (loc, fn) in
            decode ~context ~path ~inside_include:true sexps))
        (fun () ->
          Memo.parallel_map subdirs ~f:(fun (loc, sexps) ->
            subdir ~loc ~context ~path ~inside_include sexps))
    in
    List.concat (sexps :: subdirs :: includes)
  in
  fun sexps -> decode ~context ~path:[] ~inside_include:false sexps
;;

let decode ~file project sexps =
  let decoder =
    { decode =
        (fun ast d ->
          let d = Dune_project.set_parsing_context project d in
          Dune_lang.Decoder.parse d Univ_map.empty (Dune_lang.Ast.List (Loc.none, ast)))
    }
  in
  let open Memo.O in
  let+ sexps = decode_includes ~context:(Include_stanza.in_file file) decoder sexps in
  decoder.decode sexps decode
;;

type t =
  { path : Path.Source.t option
  ; kind : kind
  ; (* for [kind = Ocaml_script], this is the part inserted with subdir *)
    plain : Dir_map.t
  }

let to_dyn { path; kind; plain } =
  let open Dyn in
  record
    [ "path", option Path.Source.to_dyn path
    ; "kind", dyn_of_kind kind
    ; "plain", Dir_map.to_dyn plain
    ]
;;

let equal { path; kind; plain } t =
  Option.equal Path.Source.equal path t.path
  && equal_kind kind t.kind
  && Dir_map.equal plain t.plain
;;

let get_static_sexp t = (Dir_map.root t.plain).sexps
let kind t = t.kind
let path t = t.path
let sub_dir_status t = Source_dir_status.Spec.create (Dir_map.root t.plain).subdir_status

let load_plain sexps ~file ~from_parent ~project =
  let+ parsed =
    match file with
    | None -> Memo.return Dir_map.empty
    | Some file -> decode ~file project sexps
  in
  match from_parent with
  | None -> parsed
  | Some from_parent -> Dir_map.merge parsed from_parent
;;

let sub_dirnames t = Dir_map.sub_dirs t.plain

let load file ~from_parent ~project =
  let+ kind, plain =
    let load_plain = load_plain ~file ~from_parent ~project in
    match file with
    | None ->
      let+ plain = load_plain [] in
      Plain, plain
    | Some file ->
      let* kind, ast =
        Fs_memo.with_lexbuf_from_file (In_source_dir file) ~f:(fun lb ->
          let kind, ast =
            if Dune_lang.Dune_file_script.is_script lb
            then Ocaml_script, []
            else Plain, Dune_lang.Parser.parse lb ~mode:Many
          in
          kind, ast)
      in
      let+ ast = load_plain ast in
      kind, ast
  in
  { path = file; kind; plain }
;;

let ensure_dune_project_file_exists =
  let impl ~is_error project =
    let project_file = Dune_project.file project in
    let+ exists =
      Path.Outside_build_dir.In_source_dir project_file |> Fs_memo.file_exists
    in
    if not exists
    then (
      let dir = Path.Source.parent_exn project_file in
      User_warning.emit
        ~is_error
        ~hints:[ Pp.text "generate the project file with: $ dune init project <name>" ]
        [ Pp.textf
            "No dune-project file has been found in directory %S. A default one is \
             assumed but the project might break when dune is upgraded. Please create a \
             dune-project file."
            (Path.Source.to_string dir)
        ])
  in
  let memo =
    (* memoization is here just to make sure we don't warn more than once per
       project. the computation itself is cheap *)
    Memo.create
      "ensure-dune-project-file-exists"
      ~input:(module Dune_project)
      (impl ~is_error:false)
  in
  fun project ->
    match !Clflags.on_missing_dune_project_file with
    | Ignore -> Memo.return ()
    | Warn -> Memo.exec memo project
    | Error -> impl ~is_error:true project
;;

let load ~dir (status : Source_dir_status.t) project ~files ~parent =
  let file =
    if status = Data_only
    then None
    else if Dune_project.accept_alternative_dune_file_name project
            && Filename.Set.mem files alternative_fname
    then Some alternative_fname
    else if Filename.Set.mem files fname
    then Some fname
    else None
  in
  let parent =
    Option.bind parent ~f:(fun parent ->
      Dir_map.descend parent.plain (Path.Source.basename dir))
  in
  match parent, file with
  | None, None -> Memo.return None
  | _, _ ->
    (* CR-rgrinberg: no need to warn if [file = None] *)
    let* () = ensure_dune_project_file_exists project in
    let file = Option.map file ~f:(Path.Source.relative dir) in
    load file ~from_parent:parent ~project >>| Option.some
;;
