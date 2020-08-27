open! Dune_engine
open Stdune
open Coq_stanza

(* TODO: Build reverse map and check duplicates, however, are duplicates
   harmful?

   In Coq all libs are "wrapped" so including a module twice is not so bad. *)
type t =
  { libraries : Coq_module.t list Coq_lib_name.Map.t
  ; extract : Coq_module.t Loc.Map.t
  }

let empty = { libraries = Coq_lib_name.Map.empty; extract = Loc.Map.empty }

let coq_modules_of_files ~dirs =
  let filter_v_files (dir, local, files) =
    ( dir
    , local
    , String.Set.filter files ~f:(fun f -> Filename.check_suffix f ".v") )
  in
  let dirs = List.map dirs ~f:filter_v_files in
  let build_mod_dir (dir, prefix, files) =
    String.Set.to_list files
    |> List.map ~f:(fun file ->
           let name, _ = Filename.split_extension file in
           let name = Coq_module.Name.make name in
           Coq_module.make ~source:(Path.Build.relative dir file) ~prefix ~name)
  in
  List.concat_map ~f:build_mod_dir dirs

let library t ~name = Coq_lib_name.Map.find_exn t.libraries name

let check_no_unqualified (loc, (qualif_mode : Dune_file.Include_subdirs.t)) =
  if qualif_mode = Include Unqualified then
    User_error.raise ~loc
      [ Pp.text "(include_subdirs unqualified) is not supported yet" ]

let extract t (stanza : Extraction.t) =
  Loc.Map.find_exn t.extract stanza.buildable.loc

let of_dir (d : _ Dir_with_dune.t) ~include_subdirs ~dirs =
  check_no_unqualified include_subdirs;
  let modules = coq_modules_of_files ~dirs in
  List.fold_left d.data ~init:empty ~f:(fun acc ->
    function
    | Coq_stanza.Theory.T coq ->
      let modules =
        Coq_module.eval ~dir:d.ctx_dir coq.modules ~standard:modules
      in
      let libraries =
        Coq_lib_name.Map.add_exn acc.libraries (snd coq.name) modules
      in
      { acc with libraries }
    | Coq_stanza.Extraction.T extract ->
      let loc, prelude = extract.prelude in
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
      let extract = Loc.Map.add_exn acc.extract extract.buildable.loc m in
      { acc with extract }
    | _ -> acc)
