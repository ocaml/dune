open Import
open Memo.O

(* This module implements the first stage of evaluating dune files. In other
   words, only what is to traverse the source tree. It can be described as the
   following pipeline:

   1. Determine whether the dune file is OCaml or dune syntax.

   2. If it's OCaml syntax, there's nothing we can do because the source tree
   cannot evaluate the script without a context. If it's dune syntax, decode it
   into [Ast.t list]

   3. Resolve and expand all the includes. This eliminates the [Include]
   constructor.

   4. Group all the stanzas to the sub directories they belong to. This
   eliminates the [Subdir] constructor.

   5. Evaluate the status stanzas into a combined map for every directory. This
   eliminates the remaining constructors.

   After the last step, we're left with [Dir_map.t]. Which contains all the
   expanded stanzas for the current dune file and all subdirectories.

   The implementation is quite similar to description above, except that 4-5
   are somewhat mixed together.
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

module Ast = struct
  type t =
    | Ignored_sub_dirs of Loc.t * Predicate_lang.Glob.t
    | Data_only_dirs of Loc.t * Predicate_lang.Glob.t
    | Vendored_dirs of Loc.t * Predicate_lang.Glob.Element.t Predicate_lang.t
    | Dirs of Loc.t * Predicate_lang.Glob.t
    | Subdir of Path.Local.t * t list
    | Include of
        { loc : Loc.t
        ; file : string
        }
    | Leftovers of Dune_lang.Ast.t list

  open Dune_lang.Decoder

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
    Ignored_sub_dirs (loc, ignored)
  ;;

  let ignored_sub_dirs ~inside_subdir =
    match inside_subdir with
    | false -> ignored_sub_dirs
    | true ->
      let+ loc = loc in
      User_error.raise
        ~loc
        [ Pp.textf "ignored_subdirs is not allowed under subdir. Use dirs instead" ]
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

  let vendored_dirs =
    let+ loc, vendored =
      Dune_lang.Syntax.since Stanza.syntax (1, 11)
      >>> strict_subdir_glob "vendored_dirs"
      |> located
    in
    Vendored_dirs (loc, vendored)
  ;;

  let dirs =
    let+ loc, dirs =
      Dune_lang.Syntax.since Stanza.syntax (1, 6)
      >>> Predicate_lang.Glob.decode
      |> located
    in
    Dirs (loc, dirs)
  ;;

  let data_only_dirs =
    let+ loc, glob =
      located
        (Dune_lang.Syntax.since Stanza.syntax (1, 6)
         >>> strict_subdir_glob "data_only_dirs")
    in
    Data_only_dirs (loc, glob)
  ;;

  let descendant_path =
    Dune_lang.Decoder.plain_string (fun ~loc fn ->
      if Filename.is_relative fn
      then Path.Local.parse_string_exn ~loc fn
      else (
        let msg = [ Pp.textf "invalid sub-directory path %S" fn ] in
        let hints = [ Pp.textf "sub-directory path must be relative" ] in
        User_error.raise ~loc ~hints msg))
  ;;

  let rec subdir ~inside_include =
    let* () = Dune_lang.Syntax.since Stanza.syntax (2, 5) in
    let* loc = loc in
    let* dune_version = Dune_lang.Syntax.get_exn Stanza.syntax in
    let required_version = 2, 7 in
    if inside_include && dune_version < required_version
    then
      Dune_lang.Syntax.Error.since
        loc
        Stanza.syntax
        required_version
        ~what:"Using a `subdir' stanza within an `include'd file";
    let* subdir = descendant_path in
    let+ stanzas = decode ~inside_subdir:true ~inside_include in
    Subdir (subdir, stanzas)

  and decode_include =
    let+ loc = loc
    and+ file = relative_file in
    Include { loc; file }

  and decode ~inside_subdir ~inside_include =
    fields
    @@
    let+ subdirs = multi_field "subdir" (subdir ~inside_include)
    and+ dirs = field_o "dirs" dirs
    and+ ignored_sub_dirs =
      multi_field "ignored_subdirs" (ignored_sub_dirs ~inside_subdir)
    and+ vendored_dirs = field_o "vendored_dirs" vendored_dirs
    and+ data_only_dirs = field_o "data_only_dirs" data_only_dirs
    and+ include_stanza = multi_field "include" decode_include
    and+ rest = leftover_fields in
    let ast =
      List.concat
        [ Option.to_list dirs
        ; Option.to_list vendored_dirs
        ; subdirs
        ; ignored_sub_dirs
        ; include_stanza
        ; Option.to_list data_only_dirs
        ]
    in
    match rest with
    | [] -> ast
    | rest -> Leftovers rest :: ast
  ;;

  let statically_evaluated_stanzas =
    (* This list must be kept in sync with [decode]
       [include] is excluded b/c it's also a normal stanza *)
    [ "data_only_dirs"; "vendored_dirs"; "ignored_sub_dirs"; "subdir"; "dirs" ]
  ;;

  let decode ~inside_subdir ~inside_include =
    enter (decode ~inside_subdir ~inside_include)
  ;;
end

let statically_evaluated_stanzas = Ast.statically_evaluated_stanzas

type decoder = { decode : 'a. Dune_lang.Ast.t list -> 'a Dune_lang.Decoder.t -> 'a }

let rec evaluate_includes
  ~(decoder : decoder)
  ~context
  ~inside_subdir
  ~inside_include
  (prefix : string)
  (stanzas : Ast.t list)
  =
  Memo.parallel_map stanzas ~f:(function
    | Include { loc; file } ->
      let inside_include = true in
      let* ast, context =
        let file = Filename.concat prefix file in
        Include_stanza.load_sexps ~context (loc, file)
      in
      Ast.decode ~inside_subdir ~inside_include
      |> decoder.decode ast
      |> evaluate_includes ~decoder ~context prefix ~inside_subdir ~inside_include
    | Subdir (subdir, stanzas) ->
      (* CR-rgrinberg: there's no strict need to resolve the includes for sub
         directories here. We could delay it until we actually need the stanzas
         for the sub directories. *)
      let prefix = Filename.concat prefix (Path.Local.to_string subdir) in
      let+ stanzas =
        evaluate_includes
          ~decoder
          ~context
          ~inside_subdir:true
          ~inside_include
          prefix
          stanzas
      in
      [ Ast.Subdir (subdir, stanzas) ]
    | stanza -> Memo.return [ stanza ])
  >>| List.concat
;;

module Group = struct
  (* Process the ast into a less raw form that enforces various cross-stanza
     invariants. For example, some stanzas cannot appear together. Other
     stanzas may not appear more than once *)

  type t =
    { ignored_sub_dirs : (Loc.t * Predicate_lang.Glob.t) list
    ; data_only_dirs : (Loc.t * Predicate_lang.Glob.t) option
    ; vendored_dirs : (Loc.t * Predicate_lang.Glob.Element.t Predicate_lang.t) option
    ; dirs : (Loc.t * Predicate_lang.Glob.t) option
    ; leftovers : Dune_lang.Ast.t list
    ; subdirs : (Path.Local.t * Ast.t list) list
    }

  let empty =
    { ignored_sub_dirs = []
    ; data_only_dirs = None
    ; vendored_dirs = None
    ; dirs = None
    ; subdirs = []
    ; leftovers = []
    }
  ;;

  let no_dupes name loc acc new_ =
    match acc with
    | None -> loc, new_
    | Some _ ->
      User_error.raise ~loc [ Pp.textf "may not set the %S stanza more than once" name ]
  ;;

  let subdir_status { ignored_sub_dirs; data_only_dirs; vendored_dirs; dirs; _ } =
    let data_only =
      match data_only_dirs, ignored_sub_dirs with
      | None, [] -> None
      | Some (loc, data_only_dirs), [] -> Some (loc, data_only_dirs)
      | None, (loc, _) :: _ ->
        let ignored_sub_dirs = List.map ~f:snd ignored_sub_dirs in
        Some (loc, Predicate_lang.or_ ignored_sub_dirs)
      | Some _data_only, _ :: _ -> assert false
    in
    { Source_dir_status.Map.normal = dirs; data_only; vendored = vendored_dirs }
  ;;

  let combine t (ast : Ast.t) =
    match ast with
    | Ignored_sub_dirs (loc, glob) ->
      { t with ignored_sub_dirs = (loc, glob) :: t.ignored_sub_dirs }
    | Data_only_dirs (loc, glob) ->
      { t with
        data_only_dirs = Some (no_dupes "data_only_dirs" loc t.data_only_dirs glob)
      }
    | Vendored_dirs (loc, glob) ->
      { t with vendored_dirs = Some (no_dupes "vendored_dirs" loc t.vendored_dirs glob) }
    | Dirs (loc, glob) -> { t with dirs = Some (no_dupes "dirs" loc t.dirs glob) }
    | Subdir (path, stanzas) -> { t with subdirs = (path, stanzas) :: t.subdirs }
    | Leftovers stanzas -> { t with leftovers = List.rev_append stanzas t.leftovers }
    | Include _ -> assert false
  ;;

  let of_ast (ast : Ast.t list) =
    let t = List.fold_left ast ~init:empty ~f:combine in
    let t = { t with leftovers = List.rev t.leftovers } in
    match t.data_only_dirs, t.dirs, t.ignored_sub_dirs with
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
    | _ -> t
  ;;
end

let rec to_dir_map ast =
  let group = Group.of_ast ast in
  let node =
    let subdir_status = Group.subdir_status group in
    Dir_map.singleton { Dir_map.Per_dir.sexps = group.leftovers; subdir_status }
  in
  let subdirs =
    List.map group.subdirs ~f:(fun (path, stanzas) ->
      Dir_map.make_at_path (Path.Local.explode path) (to_dir_map stanzas))
  in
  Dir_map.merge_all (node :: subdirs)
;;

let decode ~file project sexps =
  let decoder =
    { decode =
        (fun ast d ->
          let d = Dune_project.set_parsing_context project d in
          Dune_lang.Decoder.parse d Univ_map.empty (Dune_lang.Ast.List (Loc.none, ast)))
    }
  in
  let context = Include_stanza.in_src_file file in
  let inside_include = false in
  let inside_subdir = false in
  Ast.decode ~inside_include ~inside_subdir
  |> decoder.decode sexps
  |> evaluate_includes
       ~decoder
       ~context
       ~inside_subdir
       ~inside_include
       Filename.current_dir_name
  >>| to_dir_map
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
    let project_dir = Dune_project.root project in
    let+ exists =
      let supposed_project_file =
        Path.Source.relative project_dir Dune_project.filename
      in
      Path.Outside_build_dir.In_source_dir supposed_project_file |> Fs_memo.file_exists
    in
    if not exists
    then
      User_warning.emit
        ~loc:(Loc.in_dir (Path.source project_dir))
        ~is_error
        ~hints:[ Pp.text "generate the project file with: $ dune init project <name>" ]
        [ Pp.textf
            "No dune-project file has been found in directory %S. A default one is \
             assumed but the project might break when dune is upgraded. Please create a \
             dune-project file."
            (Path.Source.to_string project_dir)
        ]
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
    let* () =
      match file with
      | None -> Memo.return ()
      | Some _ -> ensure_dune_project_file_exists project
    in
    let file = Option.map file ~f:(Path.Source.relative dir) in
    load file ~from_parent:parent ~project >>| Option.some
;;
