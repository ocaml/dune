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
open Rocq_stanza

(* TODO: Build reverse map and check duplicates, however, are duplicates
   harmful?

   In Rocq all libs are "wrapped" so including a module twice is not so bad. *)
type t =
  { libraries : Rocq_module.t list Rocq_lib_name.Map.t
  ; directories : Path.Build.t list Rocq_lib_name.Map.t
    (* [directories] is used to compute the include paths for Rocq's native
         mode *)
  ; extract : Rocq_module.t Loc.Map.t
  ; rev_map : [ `Theory of Theory.t | `Extraction of Extraction.t ] Rocq_module.Map.t
  ; expected_files : Path.Build.t Path.Build.Map.t
    (* Maps source .v file path to its .expected file path, if it exists *)
  }

let find_module ~source t =
  let f m = Path.equal (Path.build source) (Rocq_module.source m) in
  Rocq_lib_name.Map.Multi.find_elt t.libraries ~f
;;

let empty =
  { libraries = Rocq_lib_name.Map.empty
  ; directories = Rocq_lib_name.Map.empty
  ; extract = Loc.Map.empty
  ; rev_map = Rocq_module.Map.empty
  ; expected_files = Path.Build.Map.empty
  }
;;

let rocq_modules_of_files ~dirs =
  let build_mod_dir (sd : Source_file_dir.t) =
    let prefix = sd.path_to_root in
    let v_files =
      String.Set.filter sd.files ~f:(fun f -> String.ends_with ~suffix:".v" f)
    in
    String.Set.to_list_map v_files ~f:(fun file ->
      let name, _ = Filename.split_extension file in
      let name = Rocq_module.Name.make name in
      Rocq_module.make
        ~source:(Path.build @@ Path.Build.relative sd.dir file)
        ~prefix
        ~name)
  in
  List.concat_map ~f:build_mod_dir (Nonempty_list.to_list dirs)
;;

let expected_files_of_dirs ~dirs =
  let build_expected (sd : Source_file_dir.t) =
    let v_files =
      String.Set.filter sd.files ~f:(fun f -> String.ends_with ~suffix:".v" f)
    in
    String.Set.to_list_map v_files ~f:(fun file ->
      let name, _ = Filename.split_extension file in
      let expected_name = name ^ ".expected" in
      let source = Path.Build.relative sd.dir file in
      if String.Set.mem sd.files expected_name
      then Some (source, Path.Build.relative sd.dir expected_name)
      else None)
    |> List.filter_opt
  in
  List.concat_map ~f:build_expected (Nonempty_list.to_list dirs)
  |> Path.Build.Map.of_list_exn
;;

let library t ~name = Rocq_lib_name.Map.find_exn t.libraries name
let directories t ~name = Rocq_lib_name.Map.find_exn t.directories name

let check_no_unqualified (loc, (qualif_mode : Include_subdirs.t)) =
  if qualif_mode = Include Unqualified
  then
    User_error.raise
      ~loc
      [ Pp.text
          "(include_subdirs unqualified) is not supported yet with (rocq.theory ...) \
           stanzas"
      ]
;;

let extract t (stanza : Extraction.t) = Loc.Map.find_exn t.extract stanza.buildable.loc

let of_dir stanzas ~dir ~include_subdirs ~dirs =
  check_no_unqualified include_subdirs;
  let modules = rocq_modules_of_files ~dirs in
  let expected_files = expected_files_of_dirs ~dirs in
  let extend_rev_map e acc m =
    Rocq_module.Map.add acc m e
    |> function
    | Ok rev_map -> rev_map
    | Error prev_e ->
      let conf_mod_name = Rocq_module.name m |> Rocq_module.Name.to_string in
      let loc =
        match e with
        | `Theory rocq -> rocq.Theory.buildable.loc
        | `Extraction extr -> extr.Extraction.buildable.loc
      in
      let hints =
        let stanza_ty, stanza_name =
          match prev_e with
          | `Theory ({ name = _, lib_name; _ } : Theory.t) ->
            "rocq.theory", Rocq_lib_name.to_string lib_name
          | `Extraction ({ prelude = _, mod_name; _ } : Extraction.t) ->
            "rocq.extraction", Rocq_module.Name.to_string mod_name
        in
        [ Pp.textf
            "The Rocq module %S is already covered by %s stanza %S."
            conf_mod_name
            stanza_ty
            stanza_name
        ]
      in
      User_error.raise ~loc ~hints [ Pp.textf "Duplicate Rocq module %S." conf_mod_name ]
  in
  List.fold_left stanzas ~init:{ empty with expected_files } ~f:(fun acc stanza ->
    match Stanza.repr stanza with
    | Theory.T rocq ->
      let modules = Rocq_module.eval ~dir rocq.modules ~standard:modules in
      let directories =
        Rocq_lib_name.Map.add_exn
          acc.directories
          (snd rocq.name)
          (Nonempty_list.to_list_map dirs ~f:(fun (d : Source_file_dir.t) -> d.dir))
      in
      let libraries = Rocq_lib_name.Map.add_exn acc.libraries (snd rocq.name) modules in
      let rev_map =
        List.fold_left modules ~init:acc.rev_map ~f:(extend_rev_map (`Theory rocq))
      in
      { acc with directories; libraries; rev_map }
    | Extraction.T extr ->
      let loc, prelude = extr.prelude in
      let m =
        match
          List.find modules ~f:(fun m ->
            Rocq_module.Name.equal (Rocq_module.name m) prelude)
        with
        | Some m -> m
        | None ->
          User_error.raise
            ~loc
            [ Pp.text "no Rocq source corresponding to prelude field" ]
      in
      let extract = Loc.Map.add_exn acc.extract extr.buildable.loc m in
      let rev_map = extend_rev_map (`Extraction extr) acc.rev_map m in
      { acc with extract; rev_map }
    | _ -> acc)
;;

let lookup_module t m = Rocq_module.Map.find t.rev_map m

let expected_file ~rocq_lang_version t m =
  if rocq_lang_version >= (0, 12)
  then (
    let source = Rocq_module.source m in
    match Path.as_in_build_dir source with
    | Some source -> Path.Build.Map.find t.expected_files source
    | None -> None)
  else None
;;

let mlg_files ~sctx ~dir ~modules =
  let open Memo.O in
  let+ standard =
    (* All .mlg files in the current directory *)
    let filter_mlg file =
      if
        Filename.Extension.Or_empty.check
          (Path.Source.extension file)
          Filename.Extension.mlg
      then
        Some
          (Path.Build.append_source
             (Super_context.context sctx |> Context.build_dir)
             file)
      else None
    in
    Source_tree.files_of (Path.Build.drop_build_context_exn dir)
    >>| Path.Source.Set.to_list
    >>| List.filter_map ~f:filter_mlg
  in
  let parse ~loc:_ file = Path.Build.relative dir (file ^ ".mlg") in
  Ordered_set_lang.eval modules ~standard ~parse ~eq:Path.Build.equal
;;
