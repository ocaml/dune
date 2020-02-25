open Stdune

(* TODO: Build reverse map and check duplicates, however, are duplicates
   harmful?

   In Coq all libs are "wrapped" so including a module twice is not so bad. *)
type t = { libraries : Coq_module.t list Coq_lib_name.Map.t }

let empty = { libraries = Coq_lib_name.Map.empty }

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

let build_coq_modules_map (d : _ Dir_with_dune.t) ~dir ~modules =
  List.fold_left d.data ~init:Coq_lib_name.Map.empty ~f:(fun map ->
    function
    | Dune_file.Coq.T coq ->
      let modules = Coq_module.eval ~dir coq.modules ~standard:modules in
      Coq_lib_name.Map.add_exn map (snd coq.name) modules
    | _ -> map)

let library t ~name = Coq_lib_name.Map.find_exn t.libraries name

let check_no_unqualified (loc, (qualif_mode : Dune_file.Include_subdirs.t)) =
  if qualif_mode = Include Unqualified then
    User_error.raise ~loc
      [ Pp.text "(include_subdirs unqualified) is not supported yet" ]

let of_dir d ~include_subdirs ~dirs =
  check_no_unqualified include_subdirs;
  { libraries =
      build_coq_modules_map d ~dir:d.ctx_dir
        ~modules:(coq_modules_of_files ~dirs)
  }
