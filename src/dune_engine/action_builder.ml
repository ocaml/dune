open Import
include Action_builder0
open O

open struct
  module List = Stdune.List
end

let register_action_deps :
    type a. a eval_mode -> Dep.Set.t -> a Dep.Map.t Memo.t =
 fun mode deps ->
  match mode with
  | Eager -> Build_system.build_deps deps
  | Lazy -> Memo.return deps

let deps d =
  of_thunk
    { f =
        (fun mode ->
          let open Memo.O in
          let+ deps = register_action_deps mode d in
          ((), deps))
    }

let dep d = deps (Dep.Set.singleton d)

let dyn_deps t =
  of_thunk
    { f =
        (fun mode ->
          let open Memo.O in
          let* (x, deps), deps_x = run t mode in
          let+ deps = register_action_deps mode deps in
          (x, Deps_or_facts.union mode deps deps_x))
    }

let path p = deps (Dep.Set.singleton (Dep.file p))

let paths ps = deps (Dep.Set.of_files ps)

let path_set ps = deps (Dep.Set.of_files_set ps)

let paths_matching :
    type a. File_selector.t -> a eval_mode -> (Path.Set.t * a Dep.Map.t) Memo.t
    =
 fun g mode ->
  let open Memo.O in
  match mode with
  | Eager ->
    let+ files = Build_system.build_pred g in
    ( Path.Map.keys (Dep.Fact.Files.paths files) |> Path.Set.of_list
    , Dep.Map.singleton (Dep.file_selector g) (Dep.Fact.file_selector g files)
    )
  | Lazy ->
    let+ files = Build_system.eval_pred g in
    (files, Dep.Set.singleton (Dep.file_selector g))

let paths_matching ~loc:_ g =
  of_thunk { f = (fun mode -> paths_matching g mode) }

let paths_matching_unit ~loc g = ignore (paths_matching ~loc g)

let dyn_paths paths =
  dyn_deps (paths >>| fun (x, paths) -> (x, Dep.Set.of_files paths))

let dyn_paths_unit paths =
  dyn_deps (paths >>| fun paths -> ((), Dep.Set.of_files paths))

let dyn_path_set paths =
  dyn_deps (paths >>| fun (x, paths) -> (x, Dep.Set.of_files_set paths))

let dyn_path_set_reuse paths =
  dyn_deps (paths >>| fun paths -> (paths, Dep.Set.of_files_set paths))

let env_var s = deps (Dep.Set.singleton (Dep.env s))

let alias a = dep (Dep.alias a)

let contents p =
  of_thunk
    { f =
        (fun _mode ->
          let open Memo.O in
          let+ x = Build_system.read_file p ~f:Io.read_file in
          (x, Dep.Map.empty))
    }

let lines_of p =
  of_thunk
    { f =
        (fun _mode ->
          let open Memo.O in
          let+ x = Build_system.read_file p ~f:Io.lines_of_file in
          (x, Dep.Map.empty))
    }

let read_sexp p =
  let+ s = contents p in
  Dune_lang.Parser.parse_string s ~fname:(Path.to_string p) ~mode:Single

let if_file_exists p ~then_ ~else_ =
  of_thunk
    { f =
        (fun mode ->
          let open Memo.O in
          Build_system.file_exists p >>= function
          | true -> run then_ mode
          | false -> run else_ mode)
    }

let file_exists p = if_file_exists p ~then_:(return true) ~else_:(return false)

let paths_existing paths =
  all_unit
    (List.map paths ~f:(fun file ->
         if_file_exists file ~then_:(path file) ~else_:(return ())))

let fail x =
  let+ () = return () in
  x.fail ()

let source_tree ~dir =
  of_thunk
    { f =
        (fun mode ->
          let open Memo.O in
          let* deps, paths = Dep.Set.source_tree_with_file_set dir in
          let+ deps = register_action_deps mode deps in
          (paths, deps))
    }

(* CR-someday amokhov: The set of targets is accumulated using information from
   multiple sources by calling [Targets.combine], which performs set union and
   hence duplicate declarations of the very same target can go unnoticed. I
   think such redeclarations are not erroneous but are merely redundant; perhaps
   we should detect and disallow them. *)
module With_targets = struct
  type nonrec 'a t =
    { build : 'a t
    ; targets : Targets.t
    }

  let map_build t ~f = { t with build = f t.build }

  let return x = { build = return x; targets = Targets.empty }

  let add t ~file_targets =
    { build = t.build
    ; targets =
        Targets.combine t.targets
          (Targets.Files.create (Path.Build.Set.of_list file_targets))
    }

  let add_directories t ~directory_targets =
    { build = t.build
    ; targets =
        Targets.combine t.targets
          (Targets.create
             ~dirs:(Path.Build.Set.of_list directory_targets)
             ~files:Path.Build.Set.empty)
    }

  let map { build; targets } ~f = { build = map build ~f; targets }

  let map2 x y ~f =
    { build = map2 x.build y.build ~f
    ; targets = Targets.combine x.targets y.targets
    }

  let both x y =
    { build = both x.build y.build
    ; targets = Targets.combine x.targets y.targets
    }

  let seq x y =
    { build = x.build >>> y.build
    ; targets = Targets.combine x.targets y.targets
    }

  module O = struct
    let ( >>> ) = seq

    let ( >>| ) t f = map t ~f

    let ( and+ ) = both

    let ( let+ ) a f = map ~f a
  end

  open O

  let all xs =
    match xs with
    | [] -> return []
    | xs ->
      let build, targets =
        List.fold_left xs ~init:([], Targets.empty)
          ~f:(fun (builds, targets) x ->
            (x.build :: builds, Targets.combine x.targets targets))
      in
      { build = all (List.rev build); targets }

  let write_file_dyn ?(perm = Action.File_perm.Normal) fn s =
    add ~file_targets:[ fn ]
      (let+ s = s in
       Action.Full.make (Action.Write_file (fn, perm, s)))

  let memoize name t = { build = memoize name t.build; targets = t.targets }
end

let with_targets build ~targets : _ With_targets.t = { build; targets }

let with_file_targets build ~file_targets : _ With_targets.t =
  { build
  ; targets = Targets.Files.create (Path.Build.Set.of_list file_targets)
  }

let with_no_targets build : _ With_targets.t =
  { build; targets = Targets.empty }

let write_file ?(perm = Action.File_perm.Normal) fn s =
  with_file_targets ~file_targets:[ fn ]
    (return (Action.Full.make (Action.Write_file (fn, perm, s))))

let write_file_dyn ?(perm = Action.File_perm.Normal) fn s =
  with_file_targets ~file_targets:[ fn ]
    (let+ s = s in
     Action.Full.make (Action.Write_file (fn, perm, s)))

let with_stdout_to ?(perm = Action.File_perm.Normal) fn t =
  with_targets ~targets:(Targets.File.create fn)
    (let+ (act : Action.Full.t) = t in
     Action.Full.map act ~f:(Action.with_stdout_to ~perm fn))

let copy ~src ~dst =
  with_file_targets ~file_targets:[ dst ]
    (path src >>> return (Action.Full.make (Action.Copy (src, dst))))

let symlink ~src ~dst =
  with_file_targets ~file_targets:[ dst ]
    (path src >>> return (Action.Full.make (Action.Symlink (src, dst))))

let symlink_dir ~src ~dst =
  with_targets
    ~targets:
      (Targets.create ~files:Path.Build.Set.empty
         ~dirs:(Path.Build.Set.singleton dst))
    (path src >>> return (Action.Full.make (Action.Symlink (src, dst))))

let create_file ?(perm = Action.File_perm.Normal) fn =
  with_file_targets ~file_targets:[ fn ]
    (return
       (Action.Full.make (Action.Redirect_out (Stdout, fn, perm, Action.empty))))

let progn ts =
  let open With_targets.O in
  With_targets.all ts >>| Action.Full.reduce

let dyn_of_memo_deps t = dyn_deps (dyn_of_memo t)

let dep_on_alias_if_exists alias =
  of_thunk
    { f =
        (fun mode ->
          let open Memo.O in
          let* definition = Load_rules.alias_exists alias in
          match definition with
          | false -> Memo.return (false, Dep.Map.empty)
          | true ->
            let deps = Dep.Set.singleton (Dep.alias alias) in
            let+ deps = register_action_deps mode deps in
            (true, deps))
    }

module Source_tree_map_reduce =
  Source_tree.Dir.Make_map_reduce (Action_builder0) (Monoid.Exists)

let dep_on_alias_rec name context_name dir =
  let build_dir = Context_name.build_dir context_name in
  let f dir =
    let path = Path.Build.append_source build_dir (Source_tree.Dir.path dir) in
    dep_on_alias_if_exists (Alias.make ~dir:path name)
  in
  Source_tree_map_reduce.map_reduce dir
    ~traverse:Sub_dirs.Status.Set.normal_only ~f
