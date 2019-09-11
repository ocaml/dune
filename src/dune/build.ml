open! Stdune
open Import

type 'a t =
  | Pure : 'a -> 'a t
  | Map : ('a -> 'b) * 'a t -> 'b t
  | Map2 : ('a -> 'b -> 'c) * 'a t * 'b t -> 'c t
  | Targets : Path.Build.Set.t -> unit t
  | Paths_for_rule : Path.Set.t -> unit t
  | Paths_glob : File_selector.t -> Path.Set.t t
  (* The reference gets decided in Build_interpret.deps *)
  | If_file_exists : Path.t * 'a if_file_exists_state ref -> 'a t
  | Contents : Path.t -> string t
  | Lines_of : Path.t -> string list t
  | Dyn_paths : ('a * Path.Set.t) t -> 'a t
  | Dyn_deps : ('a * Dep.Set.t) t -> 'a t
  | Record_lib_deps : Lib_deps_info.t -> unit t
  | Fail : fail -> _ t
  | Memo : 'a memo -> 'a t
  | Catch : 'a t * (exn -> 'a) -> 'a t
  | Lazy_no_targets : 'a t Lazy.t -> 'a t
  | Deps : Dep.Set.t -> unit t

and 'a memo =
  { name : string
  ; t : 'a t
  ; mutable state : 'a memo_state
  }

and 'a memo_state =
  | Unevaluated
  | Evaluating
  | Evaluated of 'a * Dep.Set.t

and 'a if_file_exists_state =
  | Undecided of 'a t * 'a t
  | Decided of bool * 'a t

let get_if_file_exists_exn state =
  match !state with
  | Decided (_, t) -> t
  | Undecided _ ->
    Code_error.raise "Build.get_if_file_exists_exn: got undecided" []

let return x = Pure x

let ignore x = Map (Fn.const (), x)

let map x ~f = Map (f, x)

let map2 x y ~f = Map2 (f, x, y)

let delayed f = Map (f, return ())

module O = struct
  let ( >>> ) a b = Map2 ((fun () y -> y), a, b)

  let ( and+ ) a b = Map2 ((fun x y -> (x, y)), a, b)

  let ( let+ ) t f = Map (f, t)
end

open O

let rec all xs =
  match xs with
  | [] -> return []
  | x :: xs -> Map2 ((fun x xs -> x :: xs), x, all xs)

let all_unit xs =
  let+ (_ : unit list) = all xs in
  ()

let record_lib_deps lib_deps = Record_lib_deps lib_deps

let lazy_no_targets t = Lazy_no_targets t

let deps d = Deps d

let dep d = Deps (Dep.Set.singleton d)

let path p = Deps (Dep.Set.singleton (Dep.file p))

let paths ps = Deps (Dep.Set.of_files ps)

let path_set ps = Deps (Dep.Set.of_files_set ps)

let paths_matching ~loc:_ dir_glob = Paths_glob dir_glob

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

let paths_for_rule ps = Paths_for_rule ps

let env_var s = Deps (Dep.Set.singleton (Dep.env s))

let alias a = dep (Dep.alias a)

let declare_targets a = Targets a

let catch t ~on_error = Catch (t, on_error)

let contents p = Contents p

let lines_of p = Lines_of p

let strings p = Map ((fun l -> List.map l ~f:Scanf.unescaped), lines_of p)

let read_sexp p =
  let+ s = contents p in
  Dune_lang.parse_string s ~lexer:Dune_lang.Lexer.token
    ~fname:(Path.to_string p) ~mode:Single

let if_file_exists p ~then_ ~else_ =
  If_file_exists (p, ref (Undecided (then_, else_)))

let file_exists p = if_file_exists p ~then_:(return true) ~else_:(return false)

let file_exists_opt p t =
  if_file_exists p ~then_:(map ~f:Option.some t) ~else_:(return None)

let paths_existing paths =
  all_unit
    (List.map paths ~f:(fun file ->
         if_file_exists file ~then_:(path file) ~else_:(return ())))

let fail ?targets x =
  match targets with
  | None -> Fail x
  | Some l -> Targets (Path.Build.Set.of_list l) >>> Fail x

let of_result ?targets = function
  | Ok x -> x
  | Error e -> fail ?targets { fail = (fun () -> raise e) }

let of_result_map ?targets res ~f =
  match res with
  | Ok x -> f x
  | Error e -> fail ?targets { fail = (fun () -> raise e) }

let memoize name t = Memo { name; t; state = Unevaluated }

(* This is to force the rules to be loaded for directories without files when
   depending on [(source_tree x)]. Otherwise, we wouldn't clean up stale
   directories in directories that contain no file. *)
let depend_on_dir_without_files =
  let pred =
    Predicate.create ~id:(lazy (String "false")) ~f:(fun _ -> false)
  in
  fun dir -> Paths_glob (File_selector.create ~dir pred) |> ignore

let source_tree ~dir ~file_tree =
  let prefix_with, dir = Path.extract_build_context_dir_exn dir in
  let paths, dirs_without_files =
    let init = (Path.Set.empty, return ()) in
    match File_tree.find_dir file_tree dir with
    | None -> init
    | Some dir ->
      File_tree.Dir.fold dir ~init ~traverse:Sub_dirs.Status.Set.all
        ~f:(fun dir (acc_files, acc_dirs_without_files) ->
          let path = Path.append_source prefix_with (File_tree.Dir.path dir) in
          let files = File_tree.Dir.files dir in
          match String.Set.is_empty files with
          | true ->
            ( acc_files
            , depend_on_dir_without_files path >>> acc_dirs_without_files )
          | false ->
            ( String.Set.fold files ~init:acc_files ~f:(fun fn acc ->
                  Path.Set.add acc (Path.relative path fn))
            , acc_dirs_without_files ))
  in
  dirs_without_files >>> path_set paths >>> return paths

let action ?dir ~targets action =
  Targets (Path.Build.Set.of_list targets)
  >>> return
        ( match dir with
        | None -> action
        | Some dir -> Action.Chdir (dir, action) )

let action_dyn ?dir ~targets action =
  Targets (Path.Build.Set.of_list targets)
  >>>
  match dir with
  | None -> action
  | Some dir ->
    let+ action = action in
    Action.Chdir (dir, action)

let write_file fn s = action ~targets:[ fn ] (Write_file (fn, s))

let write_file_dyn fn s =
  Targets (Path.Build.Set.singleton fn)
  >>> let+ s = s in
      Action.Write_file (fn, s)

let copy ~src ~dst = path src >>> action ~targets:[ dst ] (Copy (src, dst))

let copy_and_add_line_directive ~src ~dst =
  path src >>> action ~targets:[ dst ] (Copy_and_add_line_directive (src, dst))

let symlink ~src ~dst =
  path src >>> action ~targets:[ dst ] (Symlink (src, dst))

let create_file fn =
  action ~targets:[ fn ] (Redirect_out (Stdout, fn, Progn []))

let remove_tree dir = return (Action.Remove_tree dir)

let mkdir dir =
  let dir = Path.build dir in
  return (Action.Mkdir dir)

let progn ts =
  let+ actions = all ts in
  Action.Progn actions

let merge_files_dyn ~target paths =
  let action =
    let+ sources, extras =
      dyn_paths
        (let+ sources, extras = paths in
         ((sources, extras), sources))
    in
    Action.Merge_files_into (sources, extras, target)
  in
  action_dyn ~targets:[ target ] action

(* Analysis *)

let no_targets_allowed () =
  Code_error.raise
    "No targets allowed under a [Build.lazy_no_targets] or \
     [Build.if_file_exists]"
    []
  [@@inline never]

let static_deps t ~list_targets =
  let rec loop : type a. a t -> Static_deps.t -> bool -> Static_deps.t =
   fun t acc targets_allowed ->
    match t with
    | Pure _ -> acc
    | Map (_, a) -> loop a acc targets_allowed
    | Map2 (_, a, b) ->
      let acc = loop a acc targets_allowed in
      loop b acc targets_allowed
    | Targets _ ->
      if not targets_allowed then no_targets_allowed ();
      acc
    | Deps deps -> Static_deps.add_action_deps acc deps
    | Paths_for_rule fns -> Static_deps.add_rule_paths acc fns
    | Paths_glob g -> Static_deps.add_action_dep acc (Dep.file_selector g)
    | If_file_exists (p, state) -> (
      match !state with
      | Decided (_, t) -> loop t acc false
      | Undecided (then_, else_) ->
        let dir = Path.parent_exn p in
        let targets = list_targets ~dir in
        if Path.Set.mem targets p then (
          state := Decided (true, then_);
          loop then_ acc false
        ) else (
          state := Decided (false, else_);
          loop else_ acc false
        ) )
    | Dyn_paths t -> loop t acc targets_allowed
    | Dyn_deps t -> loop t acc targets_allowed
    | Contents p -> Static_deps.add_rule_path acc p
    | Lines_of p -> Static_deps.add_rule_path acc p
    | Record_lib_deps _ -> acc
    | Fail _ -> acc
    | Memo m -> loop m.t acc targets_allowed
    | Catch (t, _) -> loop t acc targets_allowed
    | Lazy_no_targets t -> loop (Lazy.force t) acc false
  in
  loop t Static_deps.empty true

let lib_deps =
  let rec loop : type a. a t -> Lib_deps_info.t -> Lib_deps_info.t =
   fun t acc ->
    match t with
    | Pure _ -> acc
    | Map (_, a) -> loop a acc
    | Map2 (_, a, b) ->
      let acc = loop a acc in
      loop b acc
    | Targets _ -> acc
    | Paths_for_rule _ -> acc
    | Paths_glob _ -> acc
    | Deps _ -> acc
    | Dyn_paths t -> loop t acc
    | Dyn_deps t -> loop t acc
    | Contents _ -> acc
    | Lines_of _ -> acc
    | Record_lib_deps deps -> Lib_deps_info.merge deps acc
    | Fail _ -> acc
    | If_file_exists (_, state) -> loop (get_if_file_exists_exn state) acc
    | Memo m -> loop m.t acc
    | Catch (t, _) -> loop t acc
    | Lazy_no_targets t -> loop (Lazy.force t) acc
  in
  fun t -> loop t Lib_name.Map.empty

let targets =
  let rec loop : type a. a t -> Path.Build.Set.t -> Path.Build.Set.t =
   fun t acc ->
    match t with
    | Pure _ -> acc
    | Map (_, a) -> loop a acc
    | Map2 (_, a, b) ->
      let acc = loop a acc in
      loop b acc
    | Targets targets -> Path.Build.Set.union targets acc
    | Paths_for_rule _ -> acc
    | Paths_glob _ -> acc
    | Deps _ -> acc
    | Dyn_paths t -> loop t acc
    | Dyn_deps t -> loop t acc
    | Contents _ -> acc
    | Lines_of _ -> acc
    | Record_lib_deps _ -> acc
    | Fail _ -> acc
    | If_file_exists (_, state) -> (
      match !state with
      | Decided (v, _) ->
        Code_error.raise "Build_interpret.targets got decided if_file_exists"
          [ ("exists", Dyn.Encoder.bool v) ]
      | Undecided (a, b) ->
        let a = loop a Path.Build.Set.empty in
        let b = loop b Path.Build.Set.empty in
        if Path.Build.Set.is_empty a && Path.Build.Set.is_empty b then
          acc
        else
          Code_error.raise
            "Build_interpret.targets: cannot have targets under a \
             [if_file_exists]"
            [ ("targets-a", Path.Build.Set.to_dyn a)
            ; ("targets-b", Path.Build.Set.to_dyn b)
            ] )
    | Memo m -> loop m.t acc
    | Catch (t, _) -> loop t acc
    | Lazy_no_targets _ -> acc
  in
  fun t -> loop t Path.Build.Set.empty

(* Execution *)

let exec ~(eval_pred : Dep.eval_pred) (t : 'a t) : 'a * Dep.Set.t =
  let rec exec : type a. Dep.Set.t ref -> a t -> a =
   fun dyn_deps t ->
    match t with
    | Pure x -> x
    | Map (f, a) ->
      let a = exec dyn_deps a in
      f a
    | Map2 (f, a, b) ->
      let a = exec dyn_deps a in
      let b = exec dyn_deps b in
      f a b
    | Targets _ -> ()
    | Deps _ -> ()
    | Paths_for_rule _ -> ()
    | Paths_glob g -> eval_pred g
    | Contents p -> Io.read_file p
    | Lines_of p -> Io.lines_of_file p
    | Dyn_paths t ->
      let x, fns = exec dyn_deps t in
      dyn_deps := Dep.Set.add_paths !dyn_deps fns;
      x
    | Dyn_deps t ->
      let x, fns = exec dyn_deps t in
      dyn_deps := Dep.Set.union !dyn_deps fns;
      x
    | Record_lib_deps _ -> ()
    | Fail { fail } -> fail ()
    | If_file_exists (_, state) -> exec dyn_deps (get_if_file_exists_exn state)
    | Catch (t, on_error) -> ( try exec dyn_deps t with exn -> on_error exn )
    | Lazy_no_targets t -> exec dyn_deps (Lazy.force t)
    | Memo m -> (
      match m.state with
      | Evaluated (x, deps) ->
        dyn_deps := Dep.Set.union !dyn_deps deps;
        x
      | Evaluating ->
        User_error.raise
          [ Pp.textf
              "Dependency cycle evaluating memoized build description %s"
              m.name
          ]
      | Unevaluated -> (
        m.state <- Evaluating;
        let dyn_deps' = ref Dep.Set.empty in
        match exec dyn_deps' m.t with
        | x ->
          m.state <- Evaluated (x, !dyn_deps');
          dyn_deps := Dep.Set.union !dyn_deps !dyn_deps';
          x
        | exception exn ->
          m.state <- Unevaluated;
          reraise exn ) )
  in
  let dyn_deps = ref Dep.Set.empty in
  let result = exec dyn_deps t in
  (result, !dyn_deps)

let dyn_deps x = Dyn_deps x
