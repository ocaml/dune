open Import
include Dune_engine.Action_builder
open O
module With_targets = With_targets

type fail = { fail : 'a. unit -> 'a }

let fail x =
  let+ () = return () in
  x.fail ()
;;

let delayed f =
  let+ () = return () in
  f ()
;;

let of_memo_join f = of_memo f >>= Fun.id

let dyn_memo_deps deps =
  let* deps, a = of_memo deps in
  let+ () = Build_system.record_deps deps in
  a
;;

let deps d = dyn_memo_deps (Memo.return (d, ()))
let dep d = deps (Dep.Set.singleton d)

let dyn_deps t =
  let* a, deps = t in
  let+ () = Build_system.record_deps deps in
  a
;;

let source_dir_exists path =
  match Path.extract_build_context_dir path with
  | None -> Memo.return false
  | Some (_ctx_dir, source_dir) ->
    Memo.map (Source_tree.find_dir source_dir) ~f:Option.is_some
;;

(* If a path in the build context corresponds to a source directory, we treat
   it like [source_tree]. This currently tracks the files under that directory,
   but it does not materialize empty directories in [_build] or sandboxes. *)
let source_dir_deps_if_any path =
  Memo.map (source_dir_exists path) ~f:(fun exists ->
    Option.some_if exists (Source_deps.files path))
;;

let path path =
  let* source_dir_deps = of_memo (source_dir_deps_if_any path) in
  match source_dir_deps with
  | Some source_dir_deps ->
    let+ (_ : Path.Set.t) = dyn_memo_deps source_dir_deps in
    ()
  | None -> deps (Dep.Set.singleton (Dep.file path))
;;

let paths paths = all_unit (Stdune.List.map paths ~f:path)
let path_set paths = all_unit (Path.Set.to_list paths |> Stdune.List.map ~f:path)
let dyn_paths paths = dyn_deps (paths >>| fun (x, paths) -> x, Dep.Set.of_files paths)
let dyn_paths_unit paths = dyn_deps (paths >>| fun paths -> (), Dep.Set.of_files paths)
let contents p = of_memo (Build_system.read_file p)
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

let file_exists path =
  let* source_dir_exists = of_memo (source_dir_exists path) in
  if source_dir_exists
  then return true
  else
    let* exists = of_memo (Build_system.file_exists path) in
    if exists
    then return true
    else
      of_memo
        (Memo.map (Load_rules.is_target path) ~f:(function
           | Load_rules.Yes Load_rules.Directory -> true
           | Load_rules.No
           | Load_rules.Yes Load_rules.File
           | Load_rules.Under_directory_target_so_cannot_say -> false))
;;

let if_file_exists p ~then_ ~else_ =
  let* exists = file_exists p in
  if exists then then_ else else_
;;

let paths_existing paths =
  all_unit
    (Stdune.List.map paths ~f:(fun file ->
       if_file_exists file ~then_:(path file) ~else_:(return ())))
;;

let paths_matching g =
  let* filenames = of_memo @@ Build_system.eval_pred g in
  let+ () = Build_system.record_deps (Dep.Set.singleton (Dep.file_selector g)) in
  filenames
;;

let ignore x = map x ~f:ignore

let paths_matching ~loc:_ g =
  (* CR-someday rgrinberg: how about doing something with this location? Like pushing a
     stack frame with it for example *)
  let* () = return () in
  paths_matching g
;;

let paths_matching_unit ~loc g = ignore (paths_matching ~loc g)
let env_var s = deps (Dep.Set.singleton (Dep.env s))
