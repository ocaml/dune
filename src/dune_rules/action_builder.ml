open Import
include Dune_engine.Action_builder
open O
module With_targets = With_targets

let register_action_deps : type a. a eval_mode -> Dep.Set.t -> a Dep.Map.t Memo.t =
  fun mode deps ->
  match mode with
  | Eager -> Build_system.build_deps deps
  | Lazy -> Memo.return deps
;;

let dyn_memo_deps deps =
  of_thunk
    { f =
        (fun mode ->
          let open Memo.O in
          let* deps, paths = deps in
          let+ deps = register_action_deps mode deps in
          paths, deps)
    }
;;

let deps d = dyn_memo_deps (Memo.return (d, ()))
let dep d = deps (Dep.Set.singleton d)

let dyn_deps t =
  of_thunk
    { f =
        (fun mode ->
          let open Memo.O in
          let* (x, deps), deps_x = run t mode in
          let+ deps = register_action_deps mode deps in
          x, Deps_or_facts.union mode deps deps_x)
    }
;;

let path p = deps (Dep.Set.singleton (Dep.file p))
let paths ps = deps (Dep.Set.of_files ps)
let path_set ps = deps (Dep.Set.of_files_set ps)
let dyn_paths paths = dyn_deps (paths >>| fun (x, paths) -> x, Dep.Set.of_files paths)
let dyn_paths_unit paths = dyn_deps (paths >>| fun paths -> (), Dep.Set.of_files paths)

let contents =
  let read_file =
    Memo.exec
      (Memo.create_with_store
         "Action_builder_fs.contents"
         ~store:(module Path.Table)
         ~input:(module Path)
         ~cutoff:String.equal
         (fun p -> Build_system.read_file p ~f:Io.read_file))
  in
  fun p ->
    of_thunk
      { f =
          (fun _mode ->
            let open Memo.O in
            let+ x = read_file p in
            x, Dep.Map.empty)
      }
;;

let lines_of p = contents p >>| String.split_lines

let read_sexp p =
  let+ s = contents p in
  Dune_sexp.Parser.parse_string s ~fname:(Path.to_string p) ~mode:Single
;;

let with_targets build ~targets : _ With_targets.t = { build; targets }
let with_no_targets build : _ With_targets.t = { build; targets = Targets.empty }

let with_file_targets build ~file_targets : _ With_targets.t =
  { build; targets = Targets.Files.create (Path.Build.Set.of_list file_targets) }
;;

let write_file ?(perm = Action.File_perm.Normal) fn s =
  with_file_targets
    ~file_targets:[ fn ]
    (return (Action.Full.make (Action.Write_file (fn, perm, s))))
;;

let write_file_dyn ?(perm = Action.File_perm.Normal) fn s =
  with_file_targets
    ~file_targets:[ fn ]
    (let+ s = s in
     Action.Full.make (Action.Write_file (fn, perm, s)))
;;

let with_stdout_to ?(perm = Action.File_perm.Normal) fn t =
  with_targets
    ~targets:(Targets.File.create fn)
    (let+ (act : Action.Full.t) = t in
     Action.Full.map act ~f:(Action.with_stdout_to ~perm fn))
;;

let copy ~src ~dst =
  with_file_targets
    ~file_targets:[ dst ]
    (path src >>> return (Action.Full.make (Action.Copy (src, dst))))
;;

let symlink ~src ~dst =
  with_file_targets
    ~file_targets:[ dst ]
    (path src >>> return (Action.Full.make (Action.Symlink (src, dst))))
;;

let symlink_dir ~src ~dst =
  with_targets
    ~targets:
      (Targets.create ~files:Path.Build.Set.empty ~dirs:(Path.Build.Set.singleton dst))
    (path src >>> return (Action.Full.make (Action.Symlink (src, dst))))
;;

let progn ts =
  let open With_targets.O in
  With_targets.all ts >>| Action.Full.reduce
;;

let if_file_exists p ~then_ ~else_ =
  of_thunk
    { f =
        (fun mode ->
          let open Memo.O in
          Build_system.file_exists p
          >>= function
          | true -> run then_ mode
          | false -> run else_ mode)
    }
;;

let file_exists p = if_file_exists p ~then_:(return true) ~else_:(return false)

let paths_existing paths =
  all_unit
    (Stdune.List.map paths ~f:(fun file ->
       if_file_exists file ~then_:(path file) ~else_:(return ())))
;;

let paths_matching
  : type a. File_selector.t -> a eval_mode -> (Filename_set.t * a Dep.Map.t) Memo.t
  =
  fun g mode ->
  let open Memo.O in
  match mode with
  | Eager ->
    let+ facts = Build_system.build_pred g in
    ( Dep.Fact.Files.filenames_exn facts ~expected_parent:(File_selector.dir g)
    , Dep.Map.singleton (Dep.file_selector g) (Dep.Fact.file_selector g facts) )
  | Lazy ->
    let+ filenames = Build_system.eval_pred g in
    filenames, Dep.Set.singleton (Dep.file_selector g)
;;

let paths_matching ~loc:_ g = of_thunk { f = (fun mode -> paths_matching g mode) }
let paths_matching_unit ~loc g = ignore (paths_matching ~loc g)
let env_var s = deps (Dep.Set.singleton (Dep.env s))
