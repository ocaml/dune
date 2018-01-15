open Import
open Build.Repr

module Pset = Path.Set
module Pmap = Path.Map
module Vspec = Build.Vspec

module Target = struct
  type t =
    | Normal of Path.t
    | Vfile : _ Vspec.t -> t

  let path = function
    | Normal p -> p
    | Vfile (Vspec.T (p, _)) -> p

  let paths ts =
    List.fold_left ts ~init:Pset.empty ~f:(fun acc t ->
      Pset.add (path t) acc)
end

module Static_deps = struct
  type t =
    { rule_deps   : Path.Set.t
    ; action_deps : Path.Set.t
    }
end

let static_deps t ~all_targets_by_dir =
  let rec loop : type a b. (a, b) t -> Static_deps.t -> Static_deps.t = fun t acc ->
    match t with
    | Arr _ -> acc
    | Targets _ -> acc
    | Store_vfile _ -> acc
    | Compose (a, b) -> loop a (loop b acc)
    | First t -> loop t acc
    | Second t -> loop t acc
    | Split (a, b) -> loop a (loop b acc)
    | Fanout (a, b) -> loop a (loop b acc)
    | Paths fns -> { acc with action_deps = Pset.union fns acc.action_deps }
    | Paths_glob state -> begin
        match !state with
        | G_evaluated l ->
          { acc with action_deps = Pset.union acc.action_deps (Pset.of_list l) }
        | G_unevaluated (loc, dir, re) ->
          match Pmap.find dir (Lazy.force all_targets_by_dir) with
          | None ->
            Loc.warn loc "Directory %s doesn't exist."
              (Path.to_string_maybe_quoted dir);
            state := G_evaluated [];
            acc
          | Some targets ->
            let result =
              Pset.filter targets ~f:(fun path ->
                Re.execp re (Path.basename path))
            in
            state := G_evaluated (Pset.elements result);
            let action_deps = Pset.union result acc.action_deps in
            { acc with action_deps }
      end
    | If_file_exists (p, state) -> begin
        match !state with
        | Decided (_, t) -> loop t acc
        | Undecided (then_, else_) ->
          let dir = Path.parent p in
          let targets =
            Option.value (Pmap.find dir (Lazy.force all_targets_by_dir))
              ~default:Pset.empty
          in
          if Pset.mem p targets then begin
            state := Decided (true, then_);
            loop then_ acc
          end else begin
            state := Decided (false, else_);
            loop else_ acc
          end
      end
    | Dyn_paths t -> loop t acc
    | Vpath (Vspec.T (p, _)) -> { acc with rule_deps = Pset.add p acc.rule_deps }
    | Contents p -> { acc with rule_deps = Pset.add p acc.rule_deps }
    | Lines_of p -> { acc with rule_deps = Pset.add p acc.rule_deps }
    | Record_lib_deps _ -> acc
    | Fail _ -> acc
    | Memo m -> loop m.t acc
  in
  loop (Build.repr t) { rule_deps = Pset.empty; action_deps = Pset.empty }

let lib_deps =
  let rec loop : type a b. (a, b) t -> Build.lib_deps Pmap.t -> Build.lib_deps Pmap.t
    = fun t acc ->
      match t with
      | Arr _ -> acc
      | Targets _ -> acc
      | Store_vfile _ -> acc
      | Compose (a, b) -> loop a (loop b acc)
      | First t -> loop t acc
      | Second t -> loop t acc
      | Split (a, b) -> loop a (loop b acc)
      | Fanout (a, b) -> loop a (loop b acc)
      | Paths _ -> acc
      | Vpath _ -> acc
      | Paths_glob _ -> acc
      | Dyn_paths t -> loop t acc
      | Contents _ -> acc
      | Lines_of _ -> acc
      | Record_lib_deps (dir, deps) ->
        let data =
          match Pmap.find dir acc with
          | None -> deps
          | Some others -> Build.merge_lib_deps deps others
        in
        Pmap.add acc ~key:dir ~data
      | Fail _ -> acc
      | If_file_exists (_, state) ->
        loop (get_if_file_exists_exn state) acc
      | Memo m -> loop m.t acc
  in
  fun t -> loop (Build.repr t) Pmap.empty

let targets =
  let rec loop : type a b. (a, b) t -> Target.t list -> Target.t list = fun t acc ->
    match t with
    | Arr _ -> acc
    | Targets targets ->
      List.fold_left targets ~init:acc ~f:(fun acc fn -> Target.Normal fn :: acc)
    | Store_vfile spec -> Vfile spec :: acc
    | Compose (a, b) -> loop a (loop b acc)
    | First t -> loop t acc
    | Second t -> loop t acc
    | Split (a, b) -> loop a (loop b acc)
    | Fanout (a, b) -> loop a (loop b acc)
    | Paths _ -> acc
    | Vpath _ -> acc
    | Paths_glob _ -> acc
    | Dyn_paths t -> loop t acc
    | Contents _ -> acc
    | Lines_of _ -> acc
    | Record_lib_deps _ -> acc
    | Fail _ -> acc
    | If_file_exists (_, state) -> begin
        match !state with
        | Decided _ -> code_errorf "Build_interpret.targets got decided if_file_exists"
        | Undecided (a, b) ->
          match loop a [], loop b [] with
          | [], [] -> acc
          | _ ->
            code_errorf "Build_interpret.targets: cannot have targets \
                         under a [if_file_exists]"
      end
    | Memo m -> loop m.t acc
  in
  fun t -> loop (Build.repr t) []

module Rule = struct
  type t =
    { context  : Context.t option
    ; build    : (unit, Action.t) Build.t
    ; targets  : Target.t list
    ; sandbox  : bool
    ; fallback : Jbuild.Rule.Fallback.t
    ; locks    : Path.t list
    ; loc      : Loc.t option
    }

  let make ?(sandbox=false) ?(fallback=Jbuild.Rule.Fallback.Not_possible)
        ?context ?(locks=[]) ?loc build =
    { context
    ; build
    ; targets = targets build
    ; sandbox
    ; fallback
    ; locks
    ; loc
    }
end
