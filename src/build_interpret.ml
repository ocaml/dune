open Import
open Build.Repr

module Pset = Path.Set
module Pmap = Path.Map

let deps t ~all_targets_by_dir =
  let rec loop : type a b. (a, b) t -> Pset.t -> Pset.t = fun t acc ->
    match t with
    | Arr _ -> acc
    | Targets _ -> acc
    | Compose (a, b) -> loop a (loop b acc)
    | First t -> loop t acc
    | Second t -> loop t acc
    | Split (a, b) -> loop a (loop b acc)
    | Fanout (a, b) -> loop a (loop b acc)
    | Paths fns -> Pset.union fns acc
    | Paths_glob (dir, re) -> begin
        match Pmap.find dir (Lazy.force all_targets_by_dir) with
        | None -> acc
        | Some targets ->
          Pset.filter targets ~f:(fun path ->
            Re.execp re (Path.basename path))
          |> Pset.union acc
      end
    | If_file_exists (p, state) -> begin
        match !state with
        | Decided (exists, t) -> loop t (if exists then Pset.add p acc else acc)
        | Undecided (then_, else_) ->
          let dir = Path.parent p in
          let targets =
            Option.value (Pmap.find dir (Lazy.force all_targets_by_dir))
              ~default:Pset.empty
          in
          if Pset.mem p targets then begin
            state := Decided (true, then_);
            loop then_ (Pset.add p acc)
          end else begin
            state := Decided (false, else_);
            loop else_ acc
          end
      end
    | Dyn_paths t -> loop t acc
    | Contents p -> Pset.add p acc
    | Lines_of p -> Pset.add p acc
    | Record_lib_deps _ -> acc
    | Fail _ -> acc
    | Memo m -> loop m.t acc
  in
  loop (Build.repr t) Pset.empty

let lib_deps =
  let rec loop : type a b. (a, b) t -> Build.lib_deps Pmap.t -> Build.lib_deps Pmap.t
    = fun t acc ->
      match t with
      | Arr _ -> acc
      | Targets _ -> acc
      | Compose (a, b) -> loop a (loop b acc)
      | First t -> loop t acc
      | Second t -> loop t acc
      | Split (a, b) -> loop a (loop b acc)
      | Fanout (a, b) -> loop a (loop b acc)
      | Paths _ -> acc
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
  let rec loop : type a b. (a, b) t -> Pset.t -> Pset.t = fun t acc ->
    match t with
    | Arr _ -> acc
    | Targets targets -> Pset.union acc targets
    | Compose (a, b) -> loop a (loop b acc)
    | First t -> loop t acc
    | Second t -> loop t acc
    | Split (a, b) -> loop a (loop b acc)
    | Fanout (a, b) -> loop a (loop b acc)
    | Paths _ -> acc
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
          if Pset.is_empty (loop a Pset.empty) && Pset.is_empty (loop b Pset.empty) then
            acc
          else
            code_errorf "Build_interpret.targets: cannot have targets \
                         under a [if_file_exists]"
      end
    | Memo m -> loop m.t acc
  in
  fun t -> loop (Build.repr t) Pset.empty

module Rule = struct
  type t =
    { build   : (unit, Action.t) Build.t
    ; targets : Path.Set.t
    ; sandbox : bool
    }

  let make ?(sandbox=false) build =
    { build
    ; targets = targets build
    ; sandbox
    }
end
