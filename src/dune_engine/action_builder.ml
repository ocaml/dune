open! Stdune
open Import

module Action_desc = struct
  type nonrec t =
    { context : Build_context.t option
    ; action : Action.Full.t
    ; loc : Loc.t option
    ; dir : Path.Build.t
    ; alias : Alias.Name.t option
    }
end

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
    | Memo : 'a memo -> 'a t
    | Deps : Dep.Set.t -> unit t
    | Memo_build : 'a Memo.Build.t -> 'a t
    | Dyn_memo_build : 'a Memo.Build.t t -> 'a t
    | Goal : 'a t -> 'a t
    | Action : Action_desc.t t -> unit t
    | Action_stdout : Action_desc.t t -> string t

  and 'a memo =
    { name : string
    ; id : 'a Type_eq.Id.t
    ; t : 'a t
    }

  let return x = Pure x

  let map x ~f = Map (f, x)

  let bind x ~f = Bind (x, f)

  let both x y = Both (x, y)

  let all xs = All xs

  module O = struct
    let ( >>> ) a b = Seq (a, b)

    let ( >>= ) t f = Bind (t, f)

    let ( >>| ) t f = Map (f, t)

    let ( and+ ) a b = Both (a, b)

    let ( and* ) a b = Both (a, b)

    let ( let+ ) t f = Map (f, t)

    let ( let* ) t f = Bind (t, f)
  end
end

module Expander = String_with_vars.Make_expander (T)
include T
open O

let ignore x = Map (Fun.const (), x)

let map2 x y ~f = Map2 (f, x, y)

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

let memoize name t = Memo { name; id = Type_eq.Id.create (); t }

let source_tree ~dir = Source_tree dir

let action t = Action t

let action_stdout t = Action_stdout t

(* CR-someday amokhov: The set of targets is accumulated using information from
   multiple sources by calling [Path.Build.Set.union] and hence occasionally
   duplicate declarations of the very same target go unnoticed. I think such
   redeclarations are not erroneous but are merely redundant; it seems that it
   would be better to rule them out completely.

   Another improvement is to cache [Path.Build.Set.to_list targets] which is
   currently performed multiple times on the very same
   [Action_builder.With_targets.t]. *)
module With_targets = struct
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

  let memoize name t = { build = memoize name t.build; targets = t.targets }
end

let with_targets build ~targets : _ With_targets.t =
  { build; targets = Path.Build.Set.of_list targets }

let with_targets_set build ~targets : _ With_targets.t = { build; targets }

let with_no_targets build : _ With_targets.t =
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
  let open With_targets.O in
  let+ actions = With_targets.all ts in
  Action.Progn actions

let goal t = Goal t

(* Execution *)

let memo_build f = Memo_build f

let memo_build_join f = Memo_build f |> bind ~f:Fun.id

let dyn_memo_build f = Dyn_memo_build f

module Make_exec (Build_deps : sig
  type fact

  val merge_facts : fact Dep.Map.t -> fact Dep.Map.t -> fact Dep.Map.t

  val read_file : Path.t -> f:(Path.t -> 'a) -> 'a Memo.Build.t

  val register_action_deps : Dep.Set.t -> fact Dep.Map.t Memo.Build.t

  val register_action_dep_pred :
    File_selector.t -> (Path.Set.t * fact) Memo.Build.t

  val file_exists : Path.t -> bool Memo.Build.t

  val alias_exists : Alias.t -> bool Memo.Build.t

  val execute_action :
    observing_facts:fact Dep.Map.t -> Action_desc.t -> unit Memo.Build.t

  val execute_action_stdout :
    observing_facts:fact Dep.Map.t -> Action_desc.t -> string Memo.Build.t
end) =
struct
  module rec Execution : sig
    val exec : 'a t -> ('a * Build_deps.fact Dep.Map.t) Memo.Build.t
  end = struct
    module Function = struct
      type 'a input = 'a memo

      type 'a output = 'a * Build_deps.fact Dep.Map.t

      let name = "exec-memo"

      let id m = m.id

      let to_dyn m = Dyn.String m.name

      let eval m = Execution.exec m.t
    end

    module Memo_poly = Memo.Poly (Function)
    open Memo.Build.O

    let merge_facts = Build_deps.merge_facts

    let rec exec : type a. a t -> (a * Build_deps.fact Dep.Map.t) Memo.Build.t =
      function
      | Pure x -> Memo.Build.return (x, Dep.Map.empty)
      | Map (f, a) ->
        let+ a, deps_a = exec a in
        (f a, deps_a)
      | Both (a, b) ->
        let+ (a, deps_a), (b, deps_b) =
          Memo.Build.fork_and_join (fun () -> exec a) (fun () -> exec b)
        in
        ((a, b), merge_facts deps_a deps_b)
      | Seq (a, b) ->
        let+ ((), deps_a), (b, deps_b) =
          Memo.Build.fork_and_join (fun () -> exec a) (fun () -> exec b)
        in
        (b, merge_facts deps_a deps_b)
      | Map2 (f, a, b) ->
        let+ (a, deps_a), (b, deps_b) =
          Memo.Build.fork_and_join (fun () -> exec a) (fun () -> exec b)
        in
        (f a b, merge_facts deps_a deps_b)
      | All xs ->
        let+ res = Memo.Build.parallel_map xs ~f:exec in
        let res, deps = List.split res in
        (res, List.fold_left deps ~init:Dep.Map.empty ~f:merge_facts)
      | Deps deps ->
        let+ deps = Build_deps.register_action_deps deps in
        ((), deps)
      | Paths_glob g ->
        let+ ps, fact = Build_deps.register_action_dep_pred g in
        (ps, Dep.Map.singleton (Dep.file_selector g) fact)
      | Source_tree dir ->
        let* deps, paths = Dep.Set.source_tree_with_file_set dir in
        let+ deps = Build_deps.register_action_deps deps in
        (paths, deps)
      | Contents p ->
        let+ x = Build_deps.read_file p ~f:Io.read_file in
        (x, Dep.Map.empty)
      | Lines_of p ->
        let+ x = Build_deps.read_file p ~f:Io.lines_of_file in
        (x, Dep.Map.empty)
      | Dyn_paths t ->
        let* (x, paths), deps_x = exec t in
        let deps = Dep.Set.of_files_set paths in
        let+ deps = Build_deps.register_action_deps deps in
        (x, merge_facts deps deps_x)
      | Dyn_deps t ->
        let* (x, deps), deps_x = exec t in
        let+ deps = Build_deps.register_action_deps deps in
        (x, merge_facts deps deps_x)
      | Fail { fail } -> fail ()
      | If_file_exists (p, then_, else_) -> (
        Build_deps.file_exists p >>= function
        | true -> exec then_
        | false -> exec else_)
      | Memo m -> Memo_poly.eval m
      | Memo_build f ->
        let+ f = f in
        (f, Dep.Map.empty)
      | Dyn_memo_build f ->
        let* f, deps = exec f in
        let+ f = f in
        (f, deps)
      | Bind (t, f) ->
        let* x, deps0 = exec t in
        let+ r, deps1 = exec (f x) in
        (r, merge_facts deps0 deps1)
      | Dep_on_alias_if_exists alias -> (
        let* definition = Build_deps.alias_exists alias in
        match definition with
        | false -> Memo.Build.return (false, Dep.Map.empty)
        | true ->
          let deps = Dep.Set.singleton (Dep.alias alias) in
          let+ deps = Build_deps.register_action_deps deps in
          (true, deps))
      | Goal t ->
        let+ a, (_irrelevant_for_goals : Build_deps.fact Dep.Map.t) = exec t in
        (a, Dep.Map.empty)
      | Action t ->
        let* act, facts = exec t in
        let+ () = Build_deps.execute_action ~observing_facts:facts act in
        ((), Dep.Map.empty)
      | Action_stdout t ->
        let* act, facts = exec t in
        let+ s = Build_deps.execute_action_stdout ~observing_facts:facts act in
        (s, Dep.Map.empty)
  end

  include Execution
end

(* Static evaluation *)

(* Note: there is some duplicated logic between [can_eval_statically] and
   [static_eval]. More precisely, [can_eval_statically] returns [false] exactly
   for the nodes [static_eval] produces [assert false]. The duplication is not
   ideal, but the code is simpler this way and also we expect that we will get
   rid of this function eventually, once we have pushed the [Memo.Build.t] monad
   enough in the code base.

   If this code ends being more permanent that we expected, we should probably
   get rid of the duplication. This code was introduced on February 2021, to
   give an idea of how long it has been here. *)

let rec can_eval_statically : type a. a t -> bool = function
  | Pure _ -> true
  | Map (_, a) -> can_eval_statically a
  | Both (a, b) -> can_eval_statically a && can_eval_statically b
  | Seq (a, b) -> can_eval_statically a && can_eval_statically b
  | Map2 (_, a, b) -> can_eval_statically a && can_eval_statically b
  | All xs -> List.for_all xs ~f:can_eval_statically
  | Paths_glob _ -> false
  | Deps _ -> true
  | Dyn_paths b -> can_eval_statically b
  | Dyn_deps b -> can_eval_statically b
  | Source_tree _ -> false
  | Contents _ -> false
  | Lines_of _ -> false
  | Fail _ -> true
  | If_file_exists (_, _, _) -> false
  | Memo _ -> false
  | Memo_build _ -> false
  | Dyn_memo_build _ -> false
  | Bind _ ->
    (* TODO jeremiedimino: This should be [can_eval_statically t], however it
       breaks the [Expander.set_artifacts_dynamic] trick that it used to break a
       cycle. The cycle is as follow:

       - [(rule (deps %{cmo:x}) ..)] requires expanding %{cmo:x}

       - expanding %{cmo:x} requires computing the artifacts DB

       - computing the artifacts DB requires computing the module<->library
       assignment

       - computing the above requires knowing the set of source files (static
       and generated) in a given directory

       - computing the above works by looking at the source tree and adding all
       targets of user rules

       - computing targets of user rules is done by effectively generating the
       rules for the user rules, which means interpreting the [(deps
       %{cmo:...})] thing

       If we find another way to break this cycle we should be able to change
       this code. *)
    false
  | Dep_on_alias_if_exists _ -> false
  | Goal t -> can_eval_statically t
  | Action _ -> false
  | Action_stdout _ -> false

let static_eval =
  let rec loop : type a. a t -> Dep.Set.t -> a * Dep.Set.t =
   fun t acc ->
    match t with
    | Pure x -> (x, acc)
    | Map (f, a) ->
      let x, acc = loop a acc in
      (f x, acc)
    | Both (a, b) ->
      let a, acc = loop a acc in
      let b, acc = loop b acc in
      ((a, b), acc)
    | Seq (a, b) ->
      let (), acc = loop a acc in
      let b, acc = loop b acc in
      (b, acc)
    | Map2 (f, a, b) ->
      let a, acc = loop a acc in
      let b, acc = loop b acc in
      (f a b, acc)
    | All xs -> loop_many [] xs acc
    | Paths_glob _ -> assert false
    | Deps deps -> ((), Dep.Set.union deps acc)
    | Dyn_paths b ->
      let (x, ps), acc = loop b acc in
      (x, Dep.Set.union (Dep.Set.of_files_set ps) acc)
    | Dyn_deps b ->
      let (x, deps), acc = loop b acc in
      (x, Dep.Set.union deps acc)
    | Source_tree _ -> assert false
    | Contents _ -> assert false
    | Lines_of _ -> assert false
    | Fail { fail } -> fail ()
    | If_file_exists (_, _, _) -> assert false
    | Memo _ -> assert false
    | Memo_build _ -> assert false
    | Dyn_memo_build _ -> assert false
    | Bind _ -> assert false
    | Dep_on_alias_if_exists _ -> assert false
    | Goal t -> loop t acc
    | Action _ -> assert false
    | Action_stdout _ -> assert false
  and loop_many : type a. a list -> a t list -> Dep.Set.t -> a list * Dep.Set.t
      =
   fun acc_res l acc ->
    match l with
    | [] -> (List.rev acc_res, acc)
    | t :: l ->
      let x, acc = loop t acc in
      loop_many (x :: acc_res) l acc
  in
  fun t ->
    if can_eval_statically t then
      Some (loop t Dep.Set.empty)
    else
      None

let dyn_memo_build_deps t = dyn_deps (dyn_memo_build t)

let dep_on_alias_if_exists t = Dep_on_alias_if_exists t

module List = struct
  let map l ~f = all (List.map l ~f)
end
