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
  }
;;

let rocq_modules_of_files ~dirs =
  let filter_v_files
        ({ Source_file_dir.dir = _; path_to_root = _; files; source_dir = _ } as sd)
    =
    { sd with files = String.Set.filter files ~f:(fun f -> Filename.check_suffix f ".v") }
  in
  let dirs = List.map dirs ~f:filter_v_files in
  let build_mod_dir { Source_file_dir.dir; path_to_root = prefix; files; source_dir = _ } =
    String.Set.to_list_map files ~f:(fun file ->
      let name, _ = Filename.split_extension file in
      let name = Rocq_module.Name.make name in
      Rocq_module.make ~source:(Path.build @@ Path.Build.relative dir file) ~prefix ~name)
  in
  List.concat_map ~f:build_mod_dir dirs
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
  List.fold_left stanzas ~init:empty ~f:(fun acc stanza ->
    match Stanza.repr stanza with
    | Theory.T rocq ->
      let modules = Rocq_module.eval ~dir rocq.modules ~standard:modules in
      let directories =
        Rocq_lib_name.Map.add_exn
          acc.directories
          (snd rocq.name)
          (List.map dirs ~f:(fun (d : Source_file_dir.t) -> d.dir))
      in
      let libraries = Rocq_lib_name.Map.add_exn acc.libraries (snd rocq.name) modules in
      let rev_map =
        List.fold_left modules ~init:acc.rev_map ~f:(fun acc m ->
          Rocq_module.Map.add acc m (`Theory rocq)
          |> function
          | Ok acc -> acc
          | Error _ ->
            User_error.raise
              ~loc:rocq.buildable.loc
              [ Pp.textf
                  "Duplicate Rocq module %S."
                  (Rocq_module.name m |> Rocq_module.Name.to_string)
              ])
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
      let rev_map = Rocq_module.Map.add_exn acc.rev_map m (`Extraction extr) in
      { acc with extract; rev_map }
    | _ -> acc)
;;

let lookup_module t m = Rocq_module.Map.find t.rev_map m

let mlg_files ~sctx ~dir ~modules =
  let open Memo.O in
  let+ standard =
    (* All .mlg files in the current directory *)
    let filter_mlg file =
      if Path.Source.extension file = ".mlg"
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
