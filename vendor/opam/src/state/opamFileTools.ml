(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamParserTypes.FullPos
open OpamTypes
open OpamTypesBase
module Re = Dune_re

let log ?level fmt = OpamConsole.log "opam-file" ?level fmt

open OpamFile.OPAM

(** manipulation utilities *)

let names_of_formula flag f =
  OpamPackageVar.filter_depends_formula
    ~build:true ~post:true ~dev:true ~test:flag ~doc:flag ~dev_setup:flag
    ~default:false ~env:OpamStd.Option.none f
  |> OpamFormula.atoms
  |> List.map fst
  |> OpamPackage.Name.Set.of_list

let all_commands t =
  t.build @ t.install @ t.remove @ t.run_test @ t.deprecated_build_doc

let all_urls t =
  let urlf_urls uf = OpamFile.URL.url uf :: OpamFile.URL.mirrors uf in
  (match t.url with Some uf -> urlf_urls uf | None -> []) @
  (match t.dev_repo with Some u -> [u] | None -> []) @
  List.fold_left (fun acc (_, uf) -> urlf_urls uf @ acc) [] t.extra_sources @
  List.map snd t.pin_depends

let filters_of_formula f =
  OpamFormula.fold_left (fun acc (_, f) ->
      OpamFormula.fold_left (fun acc -> function
          | Constraint (_,f) -> f :: acc
          | Filter f -> f :: acc)
        acc f)
    [] f

(* Doesn't include filters in commands *)
let all_filters ?(exclude_post=false) t =
  OpamStd.List.filter_map snd t.patches @
  OpamStd.List.filter_map snd t.messages @
  (if exclude_post then [] else OpamStd.List.filter_map snd t.post_messages) @
  List.map snd t.depexts @
  OpamStd.List.filter_map snd t.libraries @
  OpamStd.List.filter_map snd t.syntax @
  [t.available] @
  filters_of_formula
    (OpamFormula.ands
       (t.depends ::
        t.depopts ::
        t.conflicts ::
        List.map (fun (_,f,_) -> f) t.features))

let map_all_filters f t =
  let mapsnd x =
    List.map (fun (x, ft) -> x, f ft) x
  in
  let mapsndopt x =
    List.map (function
        | (x, Some ft) -> x, Some (f ft)
        | nf -> nf)
      x
  in
  let map_commands =
    List.map
      (fun (args, filter) ->
         List.map (function
             | s, Some ft -> s, Some (f ft)
             | nf -> nf)
           args,
         OpamStd.Option.map f filter)
  in
  let map_filtered_formula =
    OpamFormula.map (fun (name, fc) ->
        let fc =
          OpamFormula.map (function
              | Filter flt -> Atom (Filter (f flt))
              | Constraint (relop, flt) -> Atom (Constraint (relop, (f flt))))
            fc
        in
        Atom (name, fc))
  in
  let map_features =
    List.map (fun (var, fformula, doc) ->
        var, map_filtered_formula fformula, doc)
  in
  t |>
  with_patches (mapsndopt t.patches) |>
  with_messages (mapsndopt t.messages) |>
  with_post_messages (mapsndopt t.post_messages) |>
  with_depexts (mapsnd t.depexts) |>
  with_libraries (mapsndopt t.libraries) |>
  with_syntax (mapsndopt t.syntax) |>
  with_available (f t.available) |>
  with_depends (map_filtered_formula t.depends) |>
  with_depopts (map_filtered_formula t.depopts) |>
  with_conflicts (map_filtered_formula t.conflicts) |>
  with_features (map_features t.features) |>
  with_build (map_commands t.build) |>
  with_run_test (map_commands t.run_test) |>
  with_install (map_commands t.install) |>
  with_remove (map_commands t.remove) |>
  with_deprecated_build_test (map_commands t.deprecated_build_test) |>
  with_deprecated_build_doc (map_commands t.deprecated_build_doc)

(* Returns all variables from all commands (or on given [command]) and all filters *)
let all_variables ?exclude_post ?command t =
  let commands =
    match command with
    | Some cmd -> cmd
    | None -> all_commands t
  in
  OpamFilter.commands_variables commands @
  List.fold_left (fun acc f -> OpamFilter.variables f @ acc)
    [] (all_filters ?exclude_post t)

let map_all_variables f t =
  let map_fld (x, flt) = x, OpamFilter.map_variables f flt in
  let map_optfld = function
    | x, Some flt -> x, Some (OpamFilter.map_variables f flt)
    | _, None as optfld -> optfld
  in
  let map_commands =
    let map_args =
      List.map
        (fun (s, filter) ->
           (match s with
            | CString s -> CString (OpamFilter.map_variables_in_string f s)
            | CIdent id ->
              let id =
                try filter_ident_of_string id |>
                    OpamFilter.map_variables_in_fident f |>
                    string_of_filter_ident
                with Failure _ -> id
              in
              CIdent id),
           OpamStd.Option.Op.(filter >>| OpamFilter.map_variables f))
    in
    List.map
      (fun (args, filter) ->
         map_args args,
         OpamStd.Option.Op.(filter >>| OpamFilter.map_variables f))
  in
  let map_filtered_formula =
    OpamFormula.map (fun (name, fc) ->
        let fc =
          OpamFormula.map (function
              | Filter flt ->
                Atom (Filter (OpamFilter.map_variables f flt))
              | Constraint (relop, flt) ->
                Atom (Constraint (relop, (OpamFilter.map_variables f flt))))
            fc
        in
        Atom (name, fc)
      )
  in
  let map_features =
    List.map (fun (var, fformula, doc) ->
        var, map_filtered_formula fformula, doc)
  in
  t |>
  with_patches (List.map map_optfld t.patches) |>
  with_messages (List.map map_optfld t.messages) |>
  with_post_messages (List.map map_optfld t.post_messages) |>
  with_depexts (List.map map_fld t.depexts) |>
  with_libraries (List.map map_optfld t.libraries) |>
  with_syntax (List.map map_optfld t.syntax) |>
  with_build (map_commands t.build) |>
  with_run_test (map_commands t.run_test) |>
  with_install (map_commands t.install) |>
  with_remove (map_commands t.remove) |>
  with_depends (map_filtered_formula t.depends) |>
  with_depopts (map_filtered_formula t.depopts) |>
  with_conflicts (map_filtered_formula t.conflicts) |>
  with_available (OpamFilter.map_variables f t.available) |>
  with_features (map_features t.features) |>
  with_deprecated_build_test (map_commands t.deprecated_build_test) |>
  with_deprecated_build_doc (map_commands t.deprecated_build_doc)

let all_expanded_strings t =
  List.map fst t.messages @
  List.map fst t.post_messages @
  List.fold_left (fun acc (args, _) ->
      List.fold_left
        (fun acc -> function CString s, _ -> s :: acc | _ -> acc)
        acc args)
    [] (all_commands t) @
  List.fold_left
    (OpamFilter.fold_down_left
       (fun acc -> function FString s -> s :: acc | _ -> acc))
    [] (all_filters t)

let all_depends t =
  OpamPackage.Name.Set.union
    (names_of_formula true t.depends)
    (names_of_formula true t.depopts)

(* Templating & linting *)

let template nv =
  let maintainer =
    let from_git = try
        match
          OpamSystem.read_command_output
            ["git"; "config"; "--get"; "user.name"],
          OpamSystem.read_command_output
            ["git"; "config"; "--get"; "user.email"]
        with
        | [name], [email] ->
          Some [Printf.sprintf "%s <%s>" name email]
        | _ -> raise Not_found
      with e -> OpamStd.Exn.fatal e; None
    in
    match from_git with
    | Some u -> u
    | None ->
      let email =
        try Some (Sys.getenv "EMAIL") with Not_found -> None in
      try
        let open Unix in
        let pw = getpwuid (getuid ()) in
        let email = match email with
          | Some e -> e
          | None -> pw.pw_name^"@"^gethostname () in
        match OpamStd.String.split pw.pw_gecos ',' with
        | name::_ -> [Printf.sprintf "%s <%s>" name email]
        | _ -> [email]
      with Not_found -> match email with
        | Some e -> [e]
        | None -> []
  in
  create nv
  |> with_name_opt None
  |> with_maintainer maintainer
  |> with_build
    [[CString "./configure", None;
      CString "--prefix=%{prefix}%", None], None;
     [CIdent "make", None], None]
  |> with_install
    [[CIdent "make", None; CString "install", None], None]
  |> with_depends
    (Atom (OpamPackage.Name.of_string "specify-dependencies-here",
           (Atom (Constraint (`Geq, FString "optional-version")))))
  |> with_author maintainer
  |> with_homepage [""]
  |> with_license [""]
  |> with_dev_repo (OpamUrl.of_string "git+https://")
  |> with_bug_reports [""]
  |> with_synopsis ""

let extra_files_default filename =
  let dir =
    OpamFilename.Op.(OpamFilename.dirname
                       (OpamFile.filename filename) / "files")
  in
  List.map
    (fun f ->
       OpamFilename.Base.of_string (OpamFilename.remove_prefix dir f),
       OpamHash.check_file (OpamFilename.to_string f))
    (OpamFilename.rec_files dir)

let warns_to_string ws =
  OpamStd.List.concat_map "\n"
    (fun (n, w, s) ->
       let ws = match w with
         | `Warning -> OpamConsole.colorise `yellow "warning"
         | `Error -> OpamConsole.colorise `red "error"
       in
       OpamStd.Format.reformat ~indent:14
         (Printf.sprintf "  %16s %2d: %s" ws n s))
    ws

let warns_to_json ?filename ws =
  let filename =
  match filename with
  | Some f -> f
  | None -> "stdout"
  in
  let warn, err =
    List.fold_left (fun (w,e) (n,we,s) ->
        let arr =
          `O [ "id", `Float (float_of_int n);
               "message", `String s]
        in
        match we with
        | `Warning -> arr::w, e
        | `Error -> w, arr::e) ([],[]) ws
  in
  let result =
  match warn,err with
  | [],[] -> "passed"
  | _, _::_ -> "error"
  | _::_, [] -> "warning"
  in
  `O [
    "file", `String filename;
    "result", `String result;
     "warnings", `A warn;
     "errors", `A err
  ]

(* Package definition loading *)

open OpamFilename.Op
open OpamStd.Option.Op


let try_read rd f =
  try rd f, None with
  | (OpamSystem.Internal_error _ | Not_found) as exc ->
    if OpamFormatConfig.(!r.strict) then
      OpamConsole.error_and_exit `File_error
        "Could not read file %s: %s.\nAborting (strict mode)."
        (OpamFile.to_string f) (Printexc.to_string exc);
    None,
    let f = OpamFile.filename f in
    Some (OpamFilename.(Base.to_string (basename f)),
          (Some (pos_file f), Printexc.to_string exc))
  | OpamPp.Bad_format bf as exc ->
    if OpamFormatConfig.(!r.strict) then
      OpamConsole.error_and_exit `File_error
        "Errors while parsing %s: %s.\nAborting (strict mode)."
        (OpamFile.to_string f) (Printexc.to_string exc);
    None,
    let f = OpamFile.filename f in
    Some (OpamFilename.(Base.to_string (basename f)), bf)

let add_aux_files ?dir ~files_subdir_hashes opam =
  let dir = match dir with
    | None ->
      OpamFile.OPAM.get_metadata_dir ~repos_roots:(fun r ->
          failwith ("Repository "^OpamRepositoryName.to_string r^
                    " not registered for add_aux_files!"))
        opam
    | some -> some
  in
  match dir with
  | None -> opam
  | Some dir ->
    let (url_file: OpamFile.URL.t OpamFile.t) =
      OpamFile.make (dir // "url")
    in
    let (descr_file: OpamFile.Descr.t OpamFile.t)  =
      OpamFile.make (dir // "descr")
    in
    let files_dir =
      OpamFilename.Op.(dir / "files")
    in
    let opam =
      match OpamFile.OPAM.url opam, try_read OpamFile.URL.read_opt url_file with
      | None, (Some url, None) -> OpamFile.OPAM.with_url url opam
      | Some opam_url, (Some url, errs) ->
        if url = opam_url && errs = None then
          log "Duplicate definition of url in '%s' and opam file"
            (OpamFile.to_string url_file)
        else
          OpamConsole.warning
            "File '%s' ignored (conflicting url already specified in the \
             'opam' file)"
            (OpamFile.to_string url_file);
        opam
      | _, (_, Some err) ->
        OpamFile.OPAM.with_format_errors (err :: opam.format_errors) opam
      | _, (None, None) -> opam
    in
    let opam =
      match OpamFile.OPAM.descr opam,
            try_read OpamFile.Descr.read_opt descr_file with
      | None, (Some descr, None) -> OpamFile.OPAM.with_descr descr opam
      | Some _, (Some _, _) ->
        log "Duplicate descr in '%s' and opam file"
          (OpamFile.to_string descr_file);
        opam
      | _, (_, Some err) ->
        OpamFile.OPAM.with_format_errors (err :: opam.format_errors) opam
      | _, (None, None)  -> opam
    in
    let opam =
      if not files_subdir_hashes then opam else
      let extra_files =
        OpamFilename.opt_dir files_dir >>| fun dir ->
        OpamFilename.rec_files dir
        |> List.map (fun file ->
            file,
            OpamFilename.Base.of_string (OpamFilename.remove_prefix dir file))
      in
      match OpamFile.OPAM.extra_files opam, extra_files with
      | None, None -> opam
      | None, Some ef ->
        log ~level:2 "Missing extra-files field for %s, adding them."
          (OpamStd.List.concat_map ", "
             (fun (_,f) -> OpamFilename.Base.to_string f) ef);
        let ef =
          List.map
            (fun (file, basename) ->
               basename,
               OpamHash.compute (OpamFilename.to_string file))
            ef
        in
        OpamFile.OPAM.with_extra_files ef opam
      | Some ef, None ->
        log "Missing expected extra files %s at %s/files"
          (OpamStd.List.concat_map ", "
             (fun (f,_) -> OpamFilename.Base.to_string f) ef)
          (OpamFilename.Dir.to_string dir);
        opam
      | Some oef, Some ef ->
        let wr_check, nf_opam, rest =
          List.fold_left (fun (wr_check, nf_opam, rest) (file, basename) ->
              match OpamStd.List.pick_assoc
                      OpamFilename.Base.equal basename rest with
              | None, rest ->
                wr_check, (basename::nf_opam), rest
              | Some ohash, rest ->
                (if OpamHash.check_file (OpamFilename.to_string file) ohash then
                   wr_check
                 else
                   basename::wr_check),
                nf_opam, rest
            ) ([], [], oef) ef
        in
        let nf_file = List.map fst rest in
        if nf_file <> [] || wr_check <> [] || nf_opam <> [] then
          log "Mismatching extra-files at %s: %s"
            (OpamFilename.Dir.to_string dir)
            ((if nf_file = [] then None else
                Some (Printf.sprintf "missing from 'files' directory (%d)"
                        (List.length nf_file)))
             :: (if nf_opam = [] then None else
                   Some (Printf.sprintf "missing from opam file (%d)"
                           (List.length nf_opam)))
             :: (if wr_check = [] then None else
                   Some (Printf.sprintf "wrong checksum (%d)"
                           (List.length wr_check)))
             :: []
             |> OpamStd.List.filter_some
             |> OpamStd.Format.pretty_list);
        opam
    in
    opam

let read_opam dir =
  let (opam_file: OpamFile.OPAM.t OpamFile.t) =
    OpamFile.make (dir // "opam")
  in
  match try_read OpamFile.OPAM.read_opt opam_file with
  | Some opam, None -> Some (add_aux_files ~dir ~files_subdir_hashes:true opam)
  | _, Some err ->
    OpamConsole.warning
      "Could not read file %s. skipping:\n%s"
      (OpamFile.to_string opam_file)
      (OpamPp.string_of_bad_format (OpamPp.Bad_format (snd err)));
    None
  | None, None -> None

let read_repo_opam ~repo_name ~repo_root dir =
  let open OpamStd.Option.Op in
  read_opam dir >>|
  OpamFile.OPAM.with_metadata_dir
    (Some (Some repo_name, OpamFilename.remove_prefix_dir repo_root dir))

let dep_formula_to_string f =
  let pp =
    OpamFormat.V.(package_formula `Conj (constraints version))
  in
  OpamPrinter.FullPos.value (OpamPp.print pp f)

let sort_opam opam =
  log "sorting %s" (OpamPackage.to_string (package opam));
  let sort_ff =
    let compare_filters filter filter' =
      let get_vars = function
        | Constraint _ -> []
        | Filter filter ->
          List.sort compare (OpamFilter.variables filter)
      in
      match get_vars filter, get_vars filter' with
      | v::_, v'::_ -> compare v v'
      | [], _::_ -> 1
      | _::_, [] -> -1
      | [],[] -> 0
    in
    OpamFilter.sort_filtered_formula
      (fun (n,filter) (n',filter') ->
         let cmp = OpamFormula.compare_formula compare_filters filter filter' in
         if cmp <> 0 then cmp else
           OpamPackage.Name.compare n n')
  in
  let fst_sort ?comp =
    let comp = OpamStd.Option.default compare comp in
    fun l -> List.sort (fun (e,_) (e',_) -> comp e e') l
  in
  opam
  |> with_author @@ List.sort compare opam.author
  |> with_tags @@ List.sort compare opam.tags
  |> with_depends @@ sort_ff opam.depends
  |> with_depopts @@ sort_ff opam.depopts
  |> with_depexts @@ fst_sort opam.depexts
  |> with_conflicts @@ sort_ff opam.conflicts
  |> with_pin_depends @@ fst_sort ~comp:OpamPackage.compare opam.pin_depends
  |> with_extra_files_opt @@ OpamStd.Option.map fst_sort opam.extra_files
  |> with_extra_sources @@ fst_sort opam.extra_sources
