open! Stdune
open Import

type 'a t =
  | Pure : 'a -> 'a t
  | Map : ('a -> 'b) * 'a t -> 'b t
  | Map2 : ('a -> 'b -> 'c) * 'a t * 'b t -> 'c t
  | Paths_for_rule : Path.Set.t -> unit t
  | Paths_glob : File_selector.t -> Path.Set.t t
  | If_file_exists : Path.t * 'a t * 'a t -> 'a t
  | Contents : Path.t -> string t
  | Lines_of : Path.t -> string list t
  | Dyn_paths : ('a * Path.Set.t) t -> 'a t
  | Dyn_deps : ('a * Dep.Set.t) t -> 'a t
  | Record_lib_deps : Lib_deps_info.t -> unit t
  | Fail : fail -> _ t
  | Memo : 'a memo -> 'a t
  | Catch : 'a t * (exn -> 'a) -> 'a t
  (* CR-soon amokhov: We do not take advantage of laziness here and we already
     know there are no targets anywhere in [t], so we should remove this
     constructor. *)
  | Lazy_no_targets : 'a t Lazy.t -> 'a t
  | Deps : Dep.Set.t -> unit t

(* CR-soon amokhov: Reimplement this ad-hoc memoization using [Memo]. *)
and 'a memo =
  { name : string
  ; t : 'a t
  ; mutable state : 'a memo_state
  }

and 'a memo_state =
  | Unevaluated
  | Evaluating
  | Evaluated of 'a * Dep.Set.t

let return x = Pure x

let ignore x = Map (Fn.const (), x)

let map x ~f = Map (f, x)

let map2 x y ~f = Map2 (f, x, y)

let delayed f = Map (f, Pure ())

module O = struct
  let ( >>> ) a b = Map2 ((fun () y -> y), a, b)

  let ( and+ ) a b = Map2 ((fun x y -> (x, y)), a, b)

  let ( let+ ) t f = Map (f, t)
end

open O

let rec all xs =
  match xs with
  | [] -> return []
  | x :: xs -> Map2 (List.cons, x, all xs)

let all_unit xs =
  let+ (_ : unit list) = all xs in
  ()

let record_lib_deps lib_deps = Record_lib_deps lib_deps

let lazy_no_targets t = Lazy_no_targets t

let deps d = Deps d

let dep d = Deps (Dep.Set.singleton d)

let dyn_deps x = Dyn_deps x

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

let catch t ~on_error = Catch (t, on_error)

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
  Dune_lang.Parser.parse_string s ~lexer:Dune_lang.Lexer.token
    ~fname:(Path.to_string p) ~mode:Single

let if_file_exists p ~then_ ~else_ = If_file_exists (p, then_, else_)

let file_exists p = if_file_exists p ~then_:(return true) ~else_:(return false)

let paths_existing paths =
  all_unit
    (List.map paths ~f:(fun file ->
         if_file_exists file ~then_:(path file) ~else_:(return ())))

let fail x = Fail x

let of_result = function
  | Ok x -> x
  | Error e -> fail { fail = (fun () -> raise e) }

let of_result_map res ~f =
  match res with
  | Ok x -> f x
  | Error e -> fail { fail = (fun () -> raise e) }

let memoize name t = Memo { name; t; state = Unevaluated }

(* This is to force the rules to be loaded for directories without files when
   depending on [(source_tree x)]. Otherwise, we wouldn't clean up stale
   directories in directories that contain no file. *)
let depend_on_dir_without_files =
  let pred = Predicate.create ~id:(lazy (String "false")) ~f:(fun _ -> false) in
  fun dir -> Paths_glob (File_selector.create ~dir pred) |> ignore

let source_tree ~dir =
  let prefix_with, dir = Path.extract_build_context_dir_exn dir in
  let paths, dirs_without_files =
    let init = (Path.Set.empty, return ()) in
    match File_tree.find_dir dir with
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

module With_targets = struct
  (* CR-soon amokhov: It seems to me that we can switch from [Path.Build.Set.t]
     to [Path.Build.t list] since we should never have repeated targets. Or we
     could use [union_exn] to check that targets are never repeated. *)
  type nonrec 'a t =
    { build : 'a t
    ; targets : Path.Build.Set.t
    }

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

  module O = struct
    let ( >>> ) = map2 ~f:(fun () y -> y)

    let ( and+ ) = map2 ~f:(fun x y -> (x, y))

    let ( let+ ) a f = map ~f a
  end

  open O

  let rec all xs =
    match xs with
    | [] -> return []
    | x :: xs -> map2 ~f:List.cons x (all xs)

  let write_file_dyn fn s =
    add ~targets:[ fn ]
      (let+ s = s in
       Action.Write_file (fn, s))

  let of_result_map res ~f ~targets =
    add ~targets
      ( match res with
      | Ok x -> f x
      | Error e ->
        { build = Fail { fail = (fun () -> raise e) }
        ; targets = Path.Build.Set.empty
        } )
end

let add build ~targets : _ With_targets.t =
  { build; targets = Path.Build.Set.of_list targets }

let no_targets build : _ With_targets.t =
  { build; targets = Path.Build.Set.empty }

let write_file fn s = add ~targets:[ fn ] (return (Action.Write_file (fn, s)))

let write_file_dyn fn s =
  add ~targets:[ fn ]
    (let+ s = s in
     Action.Write_file (fn, s))

let copy ~src ~dst =
  add ~targets:[ dst ] (path src >>> return (Action.Copy (src, dst)))

let copy_and_add_line_directive ~src ~dst =
  add ~targets:[ dst ]
    (path src >>> return (Action.Copy_and_add_line_directive (src, dst)))

let symlink ~src ~dst =
  add ~targets:[ dst ] (path src >>> return (Action.Symlink (src, dst)))

let create_file fn =
  add ~targets:[ fn ] (return (Action.Redirect_out (Stdout, fn, Action.empty)))

let progn ts =
  let open With_targets.O in
  let+ actions = With_targets.all ts in
  Action.Progn actions

(* Analysis *)

(* CR-soon amokhov: We do no memoization here and so we enter the very same
   [Memo] node multiple times. For example, when building Dune, we enter [Memo]
   around 16000 times, whereas [exec] that does proper memoization does this
   only ~1000 times. *)
let static_deps t ~file_exists =
  let rec loop : type a. a t -> Static_deps.t -> Static_deps.t =
   fun t acc ->
    match t with
    | Pure _ -> acc
    | Map (_, a) -> loop a acc
    | Map2 (_, a, b) ->
      let acc = loop a acc in
      loop b acc
    | Deps deps -> Static_deps.add_action_deps acc deps
    | Paths_for_rule fns -> Static_deps.add_rule_paths acc fns
    | Paths_glob g -> Static_deps.add_action_dep acc (Dep.file_selector g)
    | If_file_exists (p, then_, else_) ->
      if file_exists p then
        loop then_ acc
      else
        loop else_ acc
    | Dyn_paths t -> loop t acc
    | Dyn_deps t -> loop t acc
    | Contents p -> Static_deps.add_rule_path acc p
    | Lines_of p -> Static_deps.add_rule_path acc p
    | Record_lib_deps _ -> acc
    | Fail _ -> acc
    | Memo m -> loop m.t acc
    | Catch (t, _) -> loop t acc
    | Lazy_no_targets t -> loop (Lazy.force t) acc
  in
  loop t Static_deps.empty

let lib_deps t ~file_exists =
  let rec loop : type a. a t -> Lib_deps_info.t -> Lib_deps_info.t =
   fun t acc ->
    match t with
    | Pure _ -> acc
    | Map (_, a) -> loop a acc
    | Map2 (_, a, b) ->
      let acc = loop a acc in
      loop b acc
    | Paths_for_rule _ -> acc
    | Paths_glob _ -> acc
    | Deps _ -> acc
    | Dyn_paths t -> loop t acc
    | Dyn_deps t -> loop t acc
    | Contents _ -> acc
    | Lines_of _ -> acc
    | Record_lib_deps deps -> Lib_deps_info.merge deps acc
    | Fail _ -> acc
    | If_file_exists (p, then_, else_) ->
      if file_exists p then
        loop then_ acc
      else
        loop else_ acc
    | Memo m -> loop m.t acc
    | Catch (t, _) -> loop t acc
    | Lazy_no_targets t -> loop (Lazy.force t) acc
  in
  loop t Lib_name.Map.empty

(* Execution *)

let exec t ~file_exists ~eval_pred =
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
    | Deps _ -> ()
    | Paths_for_rule _ -> ()
    | Paths_glob g -> (eval_pred g : Path.Set.t)
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
    | If_file_exists (p, then_, else_) ->
      if file_exists p then
        exec dyn_deps then_
      else
        exec dyn_deps else_
    | Catch (t, on_error) -> ( try exec dyn_deps t with exn -> on_error exn )
    | Lazy_no_targets t -> exec dyn_deps (Lazy.force t)
    | Memo m -> (
      match m.state with
      | Evaluated (x, deps) ->
        dyn_deps := Dep.Set.union !dyn_deps deps;
        x
      | Evaluating ->
        User_error.raise
          [ Pp.textf "Dependency cycle evaluating memoized build description %s"
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
