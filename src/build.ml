open! Stdune
open Import

type ('a, 'b) t =
  | Arr : ('a -> 'b) -> ('a, 'b) t
  | Targets : Path.Build.Set.t -> ('a, 'a) t
  | Compose : ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t
  | First : ('a, 'b) t -> ('a * 'c, 'b * 'c) t
  | Second : ('a, 'b) t -> ('c * 'a, 'c * 'b) t
  | Split : ('a, 'b) t * ('c, 'd) t -> ('a * 'c, 'b * 'd) t
  | Fanout : ('a, 'b) t * ('a, 'c) t -> ('a, 'b * 'c) t
  | Paths_for_rule : Path.Set.t -> ('a, 'a) t
  | Paths_glob : File_selector.t -> ('a, Path.Set.t) t
  (* The reference gets decided in Build_interpret.deps *)
  | If_file_exists : Path.t * ('a, 'b) if_file_exists_state ref -> ('a, 'b) t
  | Contents : Path.t -> ('a, string) t
  | Lines_of : Path.t -> ('a, string list) t
  | Dyn_paths : ('a, Path.Set.t) t -> ('a, 'a) t
  | Dyn_deps : ('a, Dep.Set.t) t -> ('a, 'a) t
  | Record_lib_deps : Lib_deps_info.t -> ('a, 'a) t
  | Fail : fail -> (_, _) t
  | Memo : 'a memo -> (unit, 'a) t
  | Catch : ('a, 'b) t * (exn -> 'b) -> ('a, 'b) t
  | Lazy_no_targets : ('a, 'b) t Lazy.t -> ('a, 'b) t
  | Deps : Dep.Set.t -> ('a, 'a) t

and 'a memo =
  { name          : string
  ; t             : (unit, 'a) t
  ; mutable state : 'a memo_state
  }

and 'a memo_state =
  | Unevaluated
  | Evaluating
  | Evaluated of 'a * Dep.Set.t

and ('a, 'b) if_file_exists_state =
  | Undecided of ('a, 'b) t * ('a, 'b) t
  | Decided   of bool * ('a, 'b) t

type 'a s = (unit, 'a) t

let get_if_file_exists_exn state =
  match !state with
  | Decided (_, t) -> t
  | Undecided _ ->
    Exn.code_error "Build.get_if_file_exists_exn: got undecided" []

let arr f = Arr f
let return x = Arr (fun _ -> x)

let record_lib_deps lib_deps =
  Record_lib_deps lib_deps

module O = struct
  let ( >>> ) a b =
    match a, b with
    | Arr a, Arr b -> Arr (fun x -> (b (a x)))
    | _ -> Compose (a, b)

  let ( >>^ ) t f = t >>> arr f
  let ( ^>> ) f t = arr f >>> t

  let ( *** ) a b = Split (a, b)
  let ( &&& ) a b = Fanout (a, b)
end
open O

let first t = First t
let second t = Second t
let fanout a b = Fanout (a, b)
let fanout3 a b c =
  let open O in
  (a &&& (b &&& c))
  >>>
  arr (fun (a, (b, c)) -> (a, b, c))
let fanout4 a b c d =
  let open O in
  (a &&& (b &&& (c &&& d)))
  >>>
  arr (fun (a, (b, (c, d))) -> (a, b, c, d))

let rec all = function
  | [] -> arr (fun _ -> [])
  | t :: ts ->
    t &&& all ts
    >>>
    arr (fun (x, y) -> x :: y)

let lazy_no_targets t = Lazy_no_targets t

let deps d = Deps d
let dep d = Deps (Dep.Set.singleton d)
let path p = Deps (Dep.Set.singleton (Dep.file p))
let paths ps = Deps (Dep.Set.of_files ps)
let path_set ps = Deps (Dep.Set.of_files_set ps)
let paths_matching ~loc:_ dir_glob = Paths_glob dir_glob
let dyn_paths t = Dyn_paths (t >>^ Path.Set.of_list)
let dyn_path_set t = Dyn_paths t
let paths_for_rule ps = Paths_for_rule ps
let env_var s = Deps (Dep.Set.singleton (Dep.env s))
let alias a = dep (Dep.alias a)
let declare_targets a = Targets a

let catch t ~on_error = Catch (t, on_error)

let contents p = Contents p
let lines_of p = Lines_of p

let strings p =
  lines_of p
  >>^ fun l ->
  List.map l ~f:Scanf.unescaped

let read_sexp p syntax =
  contents p
  >>^ fun s ->
  Dune_lang.parse_string s
    ~lexer:(Dune_lang.Lexer.of_syntax syntax)
    ~fname:(Path.to_string p) ~mode:Single

let if_file_exists p ~then_ ~else_ =
  If_file_exists (p, ref (Undecided (then_, else_)))

let file_exists p =
  if_file_exists p
    ~then_:(return true)
    ~else_:(return false)

let file_exists_opt p t =
  if_file_exists p
    ~then_:(t >>^ Option.some)
    ~else_:(arr (Fn.const None))

let paths_existing paths =
  List.fold_left paths
    ~init:(return true)
    ~f:(fun acc file ->
      if_file_exists file
        ~then_:(path file)
        ~else_:(arr Fn.id)
      >>>
      acc)

let fail ?targets x =
  match targets with
  | None -> Fail x
  | Some l ->
    Targets (Path.Build.Set.of_list l)
    >>> Fail x

let of_result ?targets = function
  | Ok    x -> x
  | Error e -> fail ?targets { fail = fun () -> raise e }

let of_result_map ?targets res ~f =
  match res with
  | Ok    x -> f x
  | Error e -> fail ?targets { fail = fun () -> raise e }

let memoize name t =
  Memo { name; t; state = Unevaluated }

let source_tree ~dir ~file_tree =
  let (prefix_with, dir) = Path.extract_build_context_dir_exn dir in
  let paths = File_tree.files_recursively_in file_tree dir ~prefix_with in
  path_set paths >>^ fun _ -> paths

let action ?dir ~targets action =
  Targets (Path.Build.Set.of_list targets)
  >>^ fun _ ->
  match dir with
  | None -> action
  | Some dir -> Action.Chdir (dir, action)

let action_dyn ?dir ~targets () =
  match dir with
  | None -> Targets (Path.Build.Set.of_list targets)
  | Some dir ->
    Targets (Path.Build.Set.of_list targets)
    >>^ fun action ->
    Action.Chdir (dir, action)

let write_file fn s =
  action ~targets:[fn] (Write_file ((Path.build fn), s))

let write_file_dyn fn =
  Targets (Path.Build.Set.singleton fn)
  >>^ fun s ->
  Action.Write_file (Path.build fn, s)

let copy ~src ~dst =
  path src >>>
  action ~targets:[dst] (Copy (src, Path.build dst))

let copy_and_add_line_directive ~src ~dst =
  path src >>>
  action ~targets:[dst]
    (Copy_and_add_line_directive (src, Path.build dst))

let symlink ~src ~dst =
  path src >>>
  action ~targets:[dst] (Symlink (src, Path.build dst))

let create_file fn =
  action ~targets:[fn] (Redirect (Stdout, Path.build fn, Progn []))

let remove_tree dir =
  let dir = Path.build dir in
  arr (fun _ -> Action.Remove_tree dir)

let mkdir dir =
  let dir = Path.build dir in
  arr (fun _ -> Action.Mkdir dir)

let progn ts =
  all ts >>^ fun actions ->
  Action.Progn actions

let merge_files_dyn ~target =
  dyn_paths (arr fst)
  >>^ (fun (sources, extras) ->
    Action.Merge_files_into (sources, extras, Path.build target))
  >>> action_dyn ~targets:[target] ()

(* Analysis *)

let no_targets_allowed () =
  Exn.code_error "No targets allowed under a [Build.lazy_no_targets] \
                  or [Build.if_file_exists]" []
[@@inline never]

let static_deps t ~all_targets =
  let rec loop : type a b. (a, b) t -> Static_deps.t -> bool -> Static_deps.t
    = fun t acc targets_allowed ->
    match t with
    | Arr _ -> acc
    | Targets _ -> if not targets_allowed then no_targets_allowed (); acc
    | Compose (a, b) -> loop a (loop b acc targets_allowed) targets_allowed
    | First t -> loop t acc targets_allowed
    | Second t -> loop t acc targets_allowed
    | Split (a, b) -> loop a (loop b acc targets_allowed) targets_allowed
    | Fanout (a, b) -> loop a (loop b acc targets_allowed) targets_allowed
    | Deps deps ->
      Static_deps.add_action_deps acc deps
    | Paths_for_rule fns ->
      Static_deps.add_rule_paths acc fns
    | Paths_glob g ->
      Static_deps.add_action_dep acc (Dep.glob g)
    | If_file_exists (p, state) -> begin
        match !state with
        | Decided (_, t) -> loop t acc false
        | Undecided (then_, else_) ->
          let dir = Path.parent_exn p in
          let targets = all_targets ~dir in
          if Path.Set.mem targets p then begin
            state := Decided (true, then_);
            loop then_ acc false
          end else begin
            state := Decided (false, else_);
            loop else_ acc false
          end
      end
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
  let rec loop : type a b. (a, b) t -> Lib_deps_info.t -> Lib_deps_info.t
    = fun t acc ->
      match t with
      | Arr _ -> acc
      | Targets _ -> acc
      | Compose (a, b) -> loop a (loop b acc)
      | First t -> loop t acc
      | Second t -> loop t acc
      | Split (a, b) -> loop a (loop b acc)
      | Fanout (a, b) -> loop a (loop b acc)
      | Paths_for_rule _ -> acc
      | Paths_glob _ -> acc
      | Deps _ -> acc
      | Dyn_paths t -> loop t acc
      | Dyn_deps t -> loop t acc
      | Contents _ -> acc
      | Lines_of _ -> acc
      | Record_lib_deps deps -> Lib_deps_info.merge deps acc
      | Fail _ -> acc
      | If_file_exists (_, state) ->
        loop (get_if_file_exists_exn state) acc
      | Memo m -> loop m.t acc
      | Catch (t, _) -> loop t acc
      | Lazy_no_targets t -> loop (Lazy.force t) acc
  in
  fun t -> loop t Lib_name.Map.empty

let targets =
  let rec loop : type a b. (a, b) t -> Path.Build.Set.t -> Path.Build.Set.t
    = fun t acc ->
      match t with
      | Arr _ -> acc
      | Targets targets -> Path.Build.Set.union targets acc
      | Compose (a, b) -> loop a (loop b acc)
      | First t -> loop t acc
      | Second t -> loop t acc
      | Split (a, b) -> loop a (loop b acc)
      | Fanout (a, b) -> loop a (loop b acc)
      | Paths_for_rule _ -> acc
      | Paths_glob _ -> acc
      | Deps _ -> acc
      | Dyn_paths t -> loop t acc
      | Dyn_deps t -> loop t acc
      | Contents _ -> acc
      | Lines_of _ -> acc
      | Record_lib_deps _ -> acc
      | Fail _ -> acc
      | If_file_exists (_, state) -> begin
          match !state with
          | Decided (v, _) ->
            Exn.code_error "Build_interpret.targets got decided if_file_exists"
              ["exists", Sexp.Encoder.bool v]
          | Undecided (a, b) ->
            let a = loop a Path.Build.Set.empty in
            let b = loop b Path.Build.Set.empty in
            if Path.Build.Set.is_empty a && Path.Build.Set.is_empty b then
              acc
            else begin
              Exn.code_error "Build_interpret.targets: cannot have targets \
                              under a [if_file_exists]"
                [ "targets-a", Path.Build.Set.to_sexp a
                ; "targets-b", Path.Build.Set.to_sexp b
                ]
            end
        end
      | Memo m -> loop m.t acc
      | Catch (t, _) -> loop t acc
      | Lazy_no_targets _ -> acc
  in
  fun t -> loop t Path.Build.Set.empty

(* Execution *)

let exec ~(eval_pred : Dep.eval_pred) (t : ('a, 'b) t) (x : 'a)
  : 'b * Dep.Set.t =
  let rec exec
    : type a b. Dep.Set.t ref -> (a, b) t -> a -> b = fun dyn_deps t x ->
    match t with
    | Arr f -> f x
    | Targets _ -> x
    | Compose (a, b) ->
      exec dyn_deps a x |> exec dyn_deps b
    | First t ->
      let x, y = x in
      (exec dyn_deps t x, y)
    | Second t ->
      let x, y = x in
      (x, exec dyn_deps t y)
    | Split (a, b) ->
      let x, y = x in
      let x = exec dyn_deps a x in
      let y = exec dyn_deps b y in
      (x, y)
    | Fanout (a, b) ->
      let a = exec dyn_deps a x in
      let b = exec dyn_deps b x in
      (a, b)
    | Deps _ -> x
    | Paths_for_rule _ -> x
    | Paths_glob g -> eval_pred g
    | Contents p -> Io.read_file p
    | Lines_of p -> Io.lines_of_file p
    | Dyn_paths t ->
      let fns = exec dyn_deps t x in
      dyn_deps := Dep.Set.add_paths !dyn_deps fns;
      x
    | Dyn_deps t ->
      let fns = exec dyn_deps t x in
      dyn_deps := Dep.Set.union !dyn_deps fns;
      x
    | Record_lib_deps _ -> x
    | Fail { fail } -> fail ()
    | If_file_exists (_, state) ->
      exec dyn_deps (get_if_file_exists_exn state) x
    | Catch (t, on_error) -> begin
        try
          exec dyn_deps t x
        with exn ->
          on_error exn
      end
    | Lazy_no_targets t ->
      exec dyn_deps (Lazy.force t) x
    | Memo m ->
      begin match m.state with
      | Evaluated (x, deps) ->
        dyn_deps := Dep.Set.union !dyn_deps deps;
        x
      | Evaluating ->
        User_error.raise
          [ Pp.textf "Dependency cycle evaluating memoized build arrow \
                      %s" m.name ]
      | Unevaluated ->
        m.state <- Evaluating;
        let dyn_deps' = ref Dep.Set.empty in
        match exec dyn_deps' m.t x with
        | x ->
          m.state <- Evaluated (x, !dyn_deps');
          dyn_deps := Dep.Set.union !dyn_deps !dyn_deps';
          x
        | exception exn ->
          m.state <- Unevaluated;
          reraise exn
      end
  in
  let dyn_deps = ref Dep.Set.empty in
  let result = exec dyn_deps t x in
  (result, !dyn_deps)

module S = struct
  open O
  module O = struct
    let (and+) = (&&&)
    let (let+) = (>>^)
  end
  let apply x f = (x &&& f) >>^ (fun (x, f) -> f x)
  let map  x ~f = apply x (return f)
  let ignore  x = x >>^ (fun _ -> ())
  let seq   x y = (x &&& y) >>^ (fun ((), y) -> y)
  let seqs xs y = seq (ignore (all xs)) y

  let dyn_deps x = x >>> (Dyn_deps (arr (fun (_args, deps) -> deps))) >>> (arr fst)
end
