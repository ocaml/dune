open! Stdune
open Import

type 'a eval_mode = 'a Rule.eval_mode =
  | Lazy : unit eval_mode
  | Eager : Dep.Fact.t eval_mode

module T = struct
  type 'a t =
    | Pure : 'a -> 'a t
    | Map : ('a -> 'b) * 'a t -> 'b t
    | Bind : 'a t * ('a -> 'b t) -> 'b t
    | Both : 'a t * 'b t -> ('a * 'b) t
    | Seq : unit t * 'b t -> 'b t
    | All : 'a t list -> 'a list t
    | Map2 : ('a -> 'b -> 'c) * 'a t * 'b t -> 'c t
    | Paths_glob : File_selector.t -> Path.Set.t t
    | Source_tree : Path.t -> Path.Set.t t
    | Dep_on_alias_if_exists : Alias.t -> bool t
    | If_file_exists : Path.t * 'a t * 'a t -> 'a t
    | Contents : Path.t -> string t
    | Lines_of : Path.t -> string list t
    | Dyn_paths : ('a * Path.Set.t) t -> 'a t
    | Dyn_deps : ('a * Dep.Set.t) t -> 'a t
    | Fail : fail -> _ t
    | Deps : Dep.Set.t -> unit t
    | Memo_build : 'a Memo.Build.t -> 'a t
    | Dyn_memo_build : 'a Memo.Build.t t -> 'a t
    | Goal : 'a t -> 'a t
    | Push_stack_frame :
        (unit -> User_message.Style.t Pp.t) * (unit -> 'a t)
        -> 'a t
    | Thunk : 'a Rule.thunk -> 'a t

  let return x = Pure x

  let map x ~f = Map (f, x)

  let bind x ~f = Bind (x, f)

  let both x y = Both (x, y)

  let all xs = All xs

  let memo_build f = Memo_build f

  module O = struct
    let ( >>> ) a b = Seq (a, b)

    let ( >>= ) t f = Bind (t, f)

    let ( >>| ) t f = Map (f, t)

    let ( and+ ) a b = Both (a, b)

    let ( and* ) a b = Both (a, b)

    let ( let+ ) t f = Map (f, t)

    let ( let* ) t f = Bind (t, f)
  end

  open O

  module List = struct
    let map l ~f = all (List.map l ~f)

    let concat_map l ~f = map l ~f >>| List.concat
  end
end

module Expander = String_with_vars.Make_expander (T)
include T
open O

open struct
  module List = Stdune.List
end

let ignore x = Map (Fun.const (), x)

let map2 x y ~f = Map2 (f, x, y)

let push_stack_frame ~human_readable_description f =
  Push_stack_frame (human_readable_description, f)

let delayed f = Map (f, Pure ())

let all_unit xs =
  let+ (_ : unit list) = all xs in
  ()

let deps d = Deps d

let dep d = Deps (Dep.Set.singleton d)

let dyn_deps x = Dyn_deps x

let path p = Deps (Dep.Set.singleton (Dep.file p))

let paths ps = Deps (Dep.Set.of_files ps)

let path_set ps = Deps (Dep.Set.of_files_set ps)

let paths_matching ~loc:_ dir_glob = Paths_glob dir_glob

let paths_matching_unit ~loc:_ dir_glob = ignore (Paths_glob dir_glob)

let dyn_paths paths =
  Dyn_paths
    (let+ x, paths = paths in
     (x, Path.Set.of_list paths))

let dyn_paths_unit paths =
  Dyn_paths
    (let+ paths = paths in
     ((), Path.Set.of_list paths))

let dyn_path_set paths = Dyn_paths paths

let dyn_path_set_reuse paths =
  Dyn_paths
    (let+ paths = paths in
     (paths, paths))

let env_var s = Deps (Dep.Set.singleton (Dep.env s))

let alias a = dep (Dep.alias a)

let contents p = Contents p

let lines_of p = Lines_of p

let strings p =
  let f x =
    match Scanf.unescaped x with
    | Error () ->
      User_error.raise
        [ Pp.textf "Unable to parse %s" (Path.to_string_maybe_quoted p)
        ; Pp.textf
            "This file must be a list of lines escaped using OCaml's \
             conventions"
        ]
    | Ok s -> s
  in
  Map ((fun l -> List.map l ~f), lines_of p)

let read_sexp p =
  let+ s = contents p in
  Dune_lang.Parser.parse_string s ~fname:(Path.to_string p) ~mode:Single

let if_file_exists p ~then_ ~else_ = If_file_exists (p, then_, else_)

let file_exists p = if_file_exists p ~then_:(return true) ~else_:(return false)

let paths_existing paths =
  all_unit
    (List.map paths ~f:(fun file ->
         if_file_exists file ~then_:(path file) ~else_:(return ())))

let fail x = Fail x

let source_tree ~dir = Source_tree dir

(* CR-someday amokhov: The set of targets is accumulated using information from
   multiple sources by calling [Path.Build.Set.union] and hence occasionally
   duplicate declarations of the very same target go unnoticed. I think such
   redeclarations are not erroneous but are merely redundant; it seems that it
   would be better to rule them out completely.

   Another improvement is to cache [Path.Build.Set.to_list targets] which is
   currently performed multiple times on the very same
   [Action_builder.With_targets.t]. *)
module With_targets0 = struct
  type nonrec 'a t =
    { build : 'a t
    ; targets : Path.Build.Set.t
    }

  let map_build t ~f = { t with build = f t.build }

  let return x = { build = Pure x; targets = Path.Build.Set.empty }

  let add t ~targets =
    { build = t.build
    ; targets = Path.Build.Set.union t.targets (Path.Build.Set.of_list targets)
    }

  let map { build; targets } ~f = { build = map build ~f; targets }

  let map2 x y ~f =
    { build = Map2 (f, x.build, y.build)
    ; targets = Path.Build.Set.union x.targets y.targets
    }

  let both x y =
    { build = Both (x.build, y.build)
    ; targets = Path.Build.Set.union x.targets y.targets
    }

  let seq x y =
    { build = Seq (x.build, y.build)
    ; targets = Path.Build.Set.union x.targets y.targets
    }

  module O = struct
    let ( >>> ) = seq

    let ( and+ ) = both

    let ( let+ ) a f = map ~f a
  end

  open O

  let all xs =
    match xs with
    | [] -> return []
    | xs ->
      let build, targets =
        List.fold_left xs ~init:([], Path.Build.Set.empty)
          ~f:(fun (xs, set) x ->
            (x.build :: xs, Path.Build.Set.union set x.targets))
      in
      { build = All (List.rev build); targets }

  let write_file_dyn ?(perm = Action.File_perm.Normal) fn s =
    add ~targets:[ fn ]
      (let+ s = s in
       Action.Write_file (fn, perm, s))
end

let with_targets build ~targets : _ With_targets0.t =
  { build; targets = Path.Build.Set.of_list targets }

let with_targets_set build ~targets : _ With_targets0.t = { build; targets }

let with_no_targets build : _ With_targets0.t =
  { build; targets = Path.Build.Set.empty }

let write_file ?(perm = Action.File_perm.Normal) fn s =
  with_targets ~targets:[ fn ] (return (Action.Write_file (fn, perm, s)))

let write_file_dyn ?(perm = Action.File_perm.Normal) fn s =
  with_targets ~targets:[ fn ]
    (let+ s = s in
     Action.Write_file (fn, perm, s))

let copy ~src ~dst =
  with_targets ~targets:[ dst ] (path src >>> return (Action.Copy (src, dst)))

let copy_and_add_line_directive ~src ~dst =
  with_targets ~targets:[ dst ]
    (path src >>> return (Action.Copy_and_add_line_directive (src, dst)))

let symlink ~src ~dst =
  with_targets ~targets:[ dst ] (path src >>> return (Action.Symlink (src, dst)))

let create_file ?(perm = Action.File_perm.Normal) fn =
  with_targets ~targets:[ fn ]
    (return (Action.Redirect_out (Stdout, fn, perm, Action.empty)))

let progn ts =
  let open With_targets0.O in
  let+ actions = With_targets0.all ts in
  Action.Progn actions

let goal t = Goal t

let memo_build_join f = Memo_build f |> bind ~f:Fun.id

let dyn_memo_build f = Dyn_memo_build f

let dyn_memo_build_deps t = dyn_deps (dyn_memo_build t)

let dep_on_alias_if_exists t = Dep_on_alias_if_exists t

module Source_tree_map_reduce =
  Source_tree.Dir.Make_map_reduce (T) (Monoid.Exists)

let dep_on_alias_rec name context_name dir =
  let build_dir = Context_name.build_dir context_name in
  let f dir =
    let path = Path.Build.append_source build_dir (Source_tree.Dir.path dir) in
    dep_on_alias_if_exists (Alias.make ~dir:path name)
  in
  Source_tree_map_reduce.map_reduce dir
    ~traverse:Sub_dirs.Status.Set.normal_only ~f

(* Execution *)

open Memo.Build.O

let register_action_deps :
    type a. a eval_mode -> Dep.Set.t -> a Dep.Map.t Memo.Build.t =
 fun mode deps ->
  match mode with
  | Eager -> Build_system.build_deps deps
  | Lazy -> Memo.Build.return deps

let register_action_dep_pred :
    type a. a eval_mode -> File_selector.t -> (Path.Set.t * a) Memo.Build.t =
 fun mode g ->
  match mode with
  | Eager ->
    let+ files = Build_system.build_pred g in
    ( Path.Map.keys (Dep.Fact.Files.paths files) |> Path.Set.of_list
    , Dep.Fact.file_selector g files )
  | Lazy ->
    let+ files = Build_system.eval_pred g in
    (files, ())

let union : type a. a eval_mode -> a Dep.Map.t -> a Dep.Map.t -> a Dep.Map.t =
 fun mode a b ->
  match mode with
  | Lazy -> Dep.Set.union a b
  | Eager -> Dep.Facts.union a b

let rec run : type a b. a t -> b eval_mode -> (a * b Dep.Map.t) Memo.Build.t =
 fun t mode ->
  match t with
  | Pure x -> Memo.Build.return (x, Dep.Map.empty)
  | Map (f, a) ->
    let+ a, deps_a = run a mode in
    (f a, deps_a)
  | Both (a, b) ->
    let+ (a, deps_a), (b, deps_b) =
      Memo.Build.fork_and_join (fun () -> run a mode) (fun () -> run b mode)
    in
    ((a, b), union mode deps_a deps_b)
  | Seq (a, b) ->
    let+ ((), deps_a), (b, deps_b) =
      Memo.Build.fork_and_join (fun () -> run a mode) (fun () -> run b mode)
    in
    (b, union mode deps_a deps_b)
  | Map2 (f, a, b) ->
    let+ (a, deps_a), (b, deps_b) =
      Memo.Build.fork_and_join (fun () -> run a mode) (fun () -> run b mode)
    in
    (f a b, union mode deps_a deps_b)
  | All xs ->
    let+ res = Memo.Build.parallel_map xs ~f:(fun x -> run x mode) in
    let res, deps = List.split res in
    (res, List.fold_left deps ~init:Dep.Map.empty ~f:(union mode))
  | Deps deps ->
    let+ deps = register_action_deps mode deps in
    ((), deps)
  | Paths_glob g ->
    let+ ps, fact = register_action_dep_pred mode g in
    (ps, Dep.Map.singleton (Dep.file_selector g) fact)
  | Source_tree dir ->
    let* deps, paths = Dep.Set.source_tree_with_file_set dir in
    let+ deps = register_action_deps mode deps in
    (paths, deps)
  | Contents p ->
    let+ x = Build_system.read_file p ~f:Io.read_file in
    (x, Dep.Map.empty)
  | Lines_of p ->
    let+ x = Build_system.read_file p ~f:Io.lines_of_file in
    (x, Dep.Map.empty)
  | Dyn_paths t ->
    let* (x, paths), deps_x = run t mode in
    let deps = Dep.Set.of_files_set paths in
    let+ deps = register_action_deps mode deps in
    (x, union mode deps deps_x)
  | Dyn_deps t ->
    let* (x, deps), deps_x = run t mode in
    let+ deps = register_action_deps mode deps in
    (x, union mode deps deps_x)
  | Fail { fail } -> fail ()
  | If_file_exists (p, then_, else_) -> (
    Build_system.file_exists p >>= function
    | true -> run then_ mode
    | false -> run else_ mode)
  | Memo_build f ->
    let+ f = f in
    (f, Dep.Map.empty)
  | Dyn_memo_build f ->
    let* f, deps = run f mode in
    let+ f = f in
    (f, deps)
  | Bind (t, f) ->
    let* x, deps0 = run t mode in
    let+ r, deps1 = run (f x) mode in
    (r, union mode deps0 deps1)
  | Dep_on_alias_if_exists alias -> (
    let* definition = Build_system.alias_exists alias in
    match definition with
    | false -> Memo.Build.return (false, Dep.Map.empty)
    | true ->
      let deps = Dep.Set.singleton (Dep.alias alias) in
      let+ deps = register_action_deps mode deps in
      (true, deps))
  | Goal t ->
    let+ a, (_irrelevant_for_goals : b Dep.Map.t) = run t mode in
    (a, Dep.Map.empty)
  | Push_stack_frame (human_readable_description, f) ->
    Memo.push_stack_frame ~human_readable_description (fun () ->
        run (f ()) mode)
  | Thunk { f } -> f mode

let memoize name t =
  Thunk (Rule.memoize_thunk name { f = (fun mode -> run t mode) })

module With_targets = struct
  include With_targets0

  let memoize name t = { build = memoize name t.build; targets = t.targets }
end

let prefix_rules prefix ~f =
  let* res, rules = Rules.collect f in
  let+ () =
    Rules.produce
      (Rules.map_rules rules ~f:(fun (rule : Rule.t) ->
           let t =
             let open O in
             prefix >>> Thunk rule.action
           in
           Rule.set_action rule { f = (fun mode -> run t mode) }))
  in
  res

let add_alias_deps alias ?loc t =
  Rules.Produce.Alias.add_deps alias ?loc { f = (fun mode -> run t mode) }
