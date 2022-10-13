open Import
open Coq_stanza

(* TODO: Build reverse map and check duplicates, however, are duplicates
   harmful?

   In Coq all libs are "wrapped" so including a module twice is not so bad. *)
type t =
  { libraries : Coq_module.t list Coq_lib_name.Map.t
  ; directories : Path.Build.t list Coq_lib_name.Map.t
        (* [directories] is used to compute the include paths for Coq's native
           mode *)
  ; extract : Coq_module.t Loc.Map.t
  ; rev_map :
      [ `Theory of Theory.t | `Extraction of Extraction.t ] Coq_module.Map.t
  }

let find_module ~source t =
  let f m = Path.Build.equal source (Coq_module.source m) in
  Coq_lib_name.Map.Multi.find_elt t.libraries ~f

let empty =
  { libraries = Coq_lib_name.Map.empty
  ; directories = Coq_lib_name.Map.empty
  ; extract = Loc.Map.empty
  ; rev_map = Coq_module.Map.empty
  }

let coq_modules_of_files ~dirs =
  let filter_v_files (dir, local, files) =
    ( dir
    , local
    , String.Set.filter files ~f:(fun f -> Filename.check_suffix f ".v") )
  in
  let dirs = List.map dirs ~f:filter_v_files in
  let build_mod_dir (dir, prefix, files) =
    String.Set.to_list_map files ~f:(fun file ->
        let name, _ = Filename.split_extension file in
        let name = Coq_module.Name.make name in
        Coq_module.make ~source:(Path.Build.relative dir file) ~prefix ~name)
  in
  List.concat_map ~f:build_mod_dir dirs

let library t ~name = Coq_lib_name.Map.find_exn t.libraries name

let directories t ~name = Coq_lib_name.Map.find_exn t.directories name

let check_no_unqualified (loc, (qualif_mode : Dune_file.Include_subdirs.t)) =
  if qualif_mode = Include Unqualified then
    User_error.raise ~loc
      [ Pp.text
          "(include_subdirs unqualified) is not supported yet with (coq.theory \
           ...) stanzas"
      ]

let extract t (stanza : Extraction.t) =
  Loc.Map.find_exn t.extract stanza.buildable.loc

let of_dir stanzas ~dir ~include_subdirs ~dirs =
  check_no_unqualified include_subdirs;
  let modules = coq_modules_of_files ~dirs in
  List.fold_left stanzas ~init:empty ~f:(fun acc -> function
    | Theory.T coq ->
      let modules = Coq_module.eval ~dir coq.modules ~standard:modules in
      let directories =
        Coq_lib_name.Map.add_exn acc.directories (snd coq.name)
          (List.map dirs ~f:(fun (d, _, _) -> d))
      in
      let libraries =
        Coq_lib_name.Map.add_exn acc.libraries (snd coq.name) modules
      in
      let rev_map =
        List.fold_left modules ~init:acc.rev_map ~f:(fun acc m ->
            Coq_module.Map.add_exn acc m (`Theory coq))
      in
      { acc with directories; libraries; rev_map }
    | Extraction.T extr ->
      let loc, prelude = extr.prelude in
      let m =
        match
          List.find modules ~f:(fun m ->
              Coq_module.Name.equal (Coq_module.name m) prelude)
        with
        | Some m -> m
        | None ->
          User_error.raise ~loc
            [ Pp.text "no coq source corresponding to prelude field" ]
      in
      let extract = Loc.Map.add_exn acc.extract extr.buildable.loc m in
      let rev_map = Coq_module.Map.add_exn acc.rev_map m (`Extraction extr) in
      { acc with extract; rev_map }
    | _ -> acc)

let lookup_module t m = Coq_module.Map.find t.rev_map m

let mlg_files ~sctx ~dir ~modules =
  let open Memo.O in
  let+ standard =
    (* All .mlg files in the current directory *)
    let filter_mlg file =
      if Path.Source.extension file = ".mlg" then
        Some
          (Path.Build.append_source (Super_context.context sctx).build_dir file)
      else None
    in
    Source_tree.files_of (Path.Build.drop_build_context_exn dir)
    >>| Path.Source.Set.to_list
    >>| List.filter_map ~f:filter_mlg
  in
  let parse ~loc:_ file = Path.Build.relative dir (file ^ ".mlg") in
  Ordered_set_lang.eval modules ~standard ~parse ~eq:Path.Build.equal
