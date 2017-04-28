open Import
open Jbuild_types

module Dir_with_jbuild = struct
  type t =
    { src_dir : Path.t
    ; ctx_dir : Path.t
    ; stanzas : Stanzas.t
    }
end

type t =
  { context                                 : Context.t
  ; libs                                    : Lib_db.t
  ; stanzas                                 : Dir_with_jbuild.t list
  ; packages                                : Package.t String_map.t
  ; aliases                                 : Alias.Store.t
  ; file_tree                               : File_tree.t
  ; artifacts                               : Artifacts.t
  ; mutable rules                           : Build_interpret.Rule.t list
  ; stanzas_to_consider_for_install         : (Path.t * Stanza.t) list
  ; mutable known_targets_by_src_dir_so_far : String_set.t Path.Map.t
  ; libs_vfile                              : (module Vfile_kind.S with type t = Lib.t list)
  }

let context t = t.context
let aliases t = t.aliases
let stanzas t = t.stanzas
let packages t = t.packages
let artifacts t = t.artifacts
let file_tree t = t.file_tree
let rules t = t.rules
let stanzas_to_consider_for_install t = t.stanzas_to_consider_for_install

let create
      ~(context:Context.t)
      ~aliases
      ~dirs_with_dot_opam_files
      ~file_tree
      ~packages
      ~stanzas
      ~filter_out_optional_stanzas_with_missing_deps
  =
  let stanzas =
    List.map stanzas
      ~f:(fun (dir, stanzas) ->
        { Dir_with_jbuild.
          src_dir = dir
        ; ctx_dir = Path.append context.build_dir dir
          ; stanzas
        })
  in
  let internal_libraries =
    List.concat_map stanzas ~f:(fun { ctx_dir;  stanzas; _ } ->
      List.filter_map stanzas ~f:(fun stanza ->
        match (stanza : Stanza.t) with
        | Library lib -> Some (ctx_dir, lib)
        | _ -> None))
  in
  let dirs_with_dot_opam_files =
    Path.Set.elements dirs_with_dot_opam_files
    |> List.map ~f:(Path.append context.build_dir)
    |> Path.Set.of_list
  in
  let libs =
    Lib_db.create context.findlib internal_libraries
      ~dirs_with_dot_opam_files
  in
  let stanzas_to_consider_for_install =
    if filter_out_optional_stanzas_with_missing_deps then
      List.concat_map stanzas ~f:(fun { ctx_dir; stanzas; _ } ->
        List.filter_map stanzas ~f:(function
          | Library _ -> None
          | stanza    -> Some (ctx_dir, stanza)))
      @ List.map
          (Lib_db.internal_libs_without_non_installable_optional_ones libs)
          ~f:(fun (dir, lib) -> (dir, Stanza.Library lib))
    else
      List.concat_map stanzas ~f:(fun { ctx_dir; stanzas; _ } ->
        List.map stanzas ~f:(fun s -> (ctx_dir, s)))
  in
  let module Libs_vfile =
    Vfile_kind.Make_full
      (struct type t = Lib.t list end)
      (struct
        open Sexp.To_sexp
        let t _dir l = list string (List.map l ~f:Lib.best_name)
      end)
      (struct
        open Sexp.Of_sexp
        let t dir sexp =
          List.map (list string sexp) ~f:(Lib_db.find_exn libs ~from:dir)
      end)
  in
  let artifacts =
    Artifacts.create context (List.map stanzas ~f:(fun (d : Dir_with_jbuild.t) ->
      (d.ctx_dir, d.stanzas)))
  in
  { context
  ; libs
  ; stanzas
  ; packages
  ; aliases
  ; file_tree
  ; rules = []
  ; stanzas_to_consider_for_install
  ; known_targets_by_src_dir_so_far = Path.Map.empty
  ; libs_vfile = (module Libs_vfile)
  ; artifacts
  }

let add_rule t ?sandbox build =
  let rule = Build_interpret.Rule.make ?sandbox build in
  t.rules <- rule :: t.rules;
  t.known_targets_by_src_dir_so_far <-
    List.fold_left rule.targets ~init:t.known_targets_by_src_dir_so_far
      ~f:(fun acc target ->
        match Path.extract_build_context (Build_interpret.Target.path target) with
        | None -> acc
        | Some (_, path) ->
          let dir = Path.parent path in
          let fn = Path.basename path in
          let files =
            match Path.Map.find dir acc with
            | None -> String_set.singleton fn
            | Some set -> String_set.add fn set
          in
          Path.Map.add acc ~key:dir ~data:files)

let sources_and_targets_known_so_far t ~src_path =
  let sources =
    match File_tree.find_dir t.file_tree src_path with
    | None -> String_set.empty
    | Some dir -> File_tree.Dir.files dir
  in
  match Path.Map.find src_path t.known_targets_by_src_dir_so_far with
  | None -> sources
  | Some set -> String_set.union sources set


module Libs = struct
  open Build.O
  open Lib_db

  let find t ~from name = find t.libs ~from name

  let vrequires t ~dir ~item =
    let fn = Path.relative dir (item ^ ".requires.sexp") in
    Build.Vspec.T (fn, t.libs_vfile)

  let load_requires t ~dir ~item =
    Build.vpath (vrequires t ~dir ~item)

  let vruntime_deps t ~dir ~item =
    let fn = Path.relative dir (item ^ ".runtime-deps.sexp") in
    Build.Vspec.T (fn, t.libs_vfile)

  let load_runtime_deps t ~dir ~item =
    Build.vpath (vruntime_deps t ~dir ~item)

  let with_fail ~fail build =
    match fail with
    | None -> build
    | Some f -> Build.fail f >>> build

  let closure t ~dir ~dep_kind lib_deps =
    let internals, externals, fail = Lib_db.interpret_lib_deps t.libs ~dir lib_deps in
    with_fail ~fail
      (Build.record_lib_deps ~dir ~kind:dep_kind lib_deps
       >>>
       Build.all
         (List.map internals ~f:(fun ((dir, lib) : Lib.Internal.t) ->
            load_requires t ~dir ~item:lib.name))
       >>^ (fun internal_deps ->
         let externals =
           Findlib.closure externals
             ~required_by:dir
             ~local_public_libs:(local_public_libs t.libs)
           |> List.map ~f:(fun pkg -> Lib.External pkg)
         in
         Lib.remove_dups_preserve_order
           (List.concat (externals :: internal_deps) @
            List.map internals ~f:(fun x -> Lib.Internal x))))

  let closed_ppx_runtime_deps_of t ~dir ~dep_kind lib_deps =
    let internals, externals, fail = Lib_db.interpret_lib_deps t.libs ~dir lib_deps in
    with_fail ~fail
      (Build.record_lib_deps ~dir ~kind:dep_kind lib_deps
       >>>
       Build.all
         (List.map internals ~f:(fun ((dir, lib) : Lib.Internal.t) ->
            load_runtime_deps t ~dir ~item:lib.name))
       >>^ (fun libs ->
         let externals =
           Findlib.closed_ppx_runtime_deps_of externals
             ~required_by:dir
             ~local_public_libs:(local_public_libs t.libs)
           |> List.map ~f:(fun pkg -> Lib.External pkg)
         in
         Lib.remove_dups_preserve_order (List.concat (externals :: libs))))

  let lib_is_available t ~from name = lib_is_available t.libs ~from name

  let add_select_rules t ~dir lib_deps =
    List.iter (Lib_db.resolve_selects t.libs ~from:dir lib_deps) ~f:(fun { dst_fn; src_fn } ->
      let src = Path.relative dir src_fn in
      let dst = Path.relative dir dst_fn in
      add_rule t
        (Build.path src
         >>>
         Build.action_context_independent ~targets:[dst]
           (Copy_and_add_line_directive (src, dst))))
end
