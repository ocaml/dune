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
      Pset.add acc (path t))
end

module Static_deps = struct
  type t =
    { rule_deps   : Path.Set.t
    ; action_deps : Path.Set.t
    }
end

type file_kind = Reg | Dir

let inspect_path file_tree path =
  match Path.drop_build_context path with
  | None ->
    if not (Path.exists path) then
      None
    else if Path.is_directory path then
      Some Dir
    else
      Some Reg
  | Some path ->
    match File_tree.find_dir file_tree path with
    | Some _ ->
      Some Dir
    | None ->
      if Path.is_root path then
        Some Dir
      else if File_tree.file_exists file_tree
                (Path.parent   path)
                (Path.basename path) then
        Some Reg
      else
        None

let static_deps t ~all_targets ~file_tree =
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
          let targets = all_targets ~dir in
          let result =
            Pset.filter targets ~f:(fun path ->
              Re.execp re (Path.basename path))
          in
          if Pset.is_empty result then begin
            match inspect_path file_tree dir with
            | None ->
              Loc.warn loc "Directory %s doesn't exist."
                (Path.to_string_maybe_quoted
                   (Path.drop_optional_build_context dir))
            | Some Reg ->
              Loc.warn loc "%s is not a directory."
                (Path.to_string_maybe_quoted
                   (Path.drop_optional_build_context dir))
            | Some Dir ->
              (* diml: we should probably warn in this case as well *)
              ()
          end;
          state := G_evaluated (Pset.to_list result);
          let action_deps = Pset.union result acc.action_deps in
          { acc with action_deps }
      end
    | If_file_exists (p, state) -> begin
        match !state with
        | Decided (_, t) -> loop t acc
        | Undecided (then_, else_) ->
          let dir = Path.parent p in
          let targets = all_targets ~dir in
          if Pset.mem targets p then begin
            state := Decided (true, then_);
            loop then_ acc
          end else begin
            state := Decided (false, else_);
            loop else_ acc
          end
      end
    | Dyn_paths t -> loop t acc
    | Vpath (Vspec.T (p, _)) -> { acc with rule_deps = Pset.add acc.rule_deps p }
    | Contents p -> { acc with rule_deps = Pset.add acc.rule_deps p }
    | Lines_of p -> { acc with rule_deps = Pset.add acc.rule_deps p }
    | Record_lib_deps _ -> acc
    | Fail _ -> acc
    | Memo m -> loop m.t acc
  in
  loop (Build.repr t) { rule_deps = Pset.empty; action_deps = Pset.empty }

let lib_deps =
  let rec loop : type a b. (a, b) t -> Build.lib_deps -> Build.lib_deps
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
      | Record_lib_deps deps -> Build.merge_lib_deps deps acc
      | Fail _ -> acc
      | If_file_exists (_, state) ->
        loop (get_if_file_exists_exn state) acc
      | Memo m -> loop m.t acc
  in
  fun t -> loop (Build.repr t) String_map.empty

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
    ; mode     : Jbuild.Rule.Mode.t
    ; locks    : Path.t list
    ; loc      : Loc.t option
    ; dir      : Path.t
    }

  let make ?(sandbox=false) ?(mode=Jbuild.Rule.Mode.Not_a_rule_stanza)
        ?context ?(locks=[]) ?loc build =
    let targets = targets build in
    let dir =
      match targets with
      | [] ->
        invalid_arg "Build_interpret.Rule.make: rule has no targets"
      | x :: l ->
        let dir = Path.parent (Target.path x) in
        List.iter l ~f:(fun target ->
          let path = Target.path target in
          if Path.parent path <> dir then
            match loc with
            | None ->
              Sexp.code_error "rule has targets in different directories"
                [ "targets", Sexp.To_sexp.list Path.sexp_of_t
                               (List.map targets ~f:Target.path)
                ]
            | Some loc ->
              Loc.fail loc
                "Rule has targets in different directories.\nTargets:\n%s"
                (String.concat ~sep:"\n"
                   (List.map targets ~f:(fun t ->
                      sprintf "- %s"
                        (Target.path t |> Path.to_string_maybe_quoted)))));
        dir
    in
    { context
    ; build
    ; targets
    ; sandbox
    ; mode
    ; locks
    ; loc
    ; dir
    }
end
