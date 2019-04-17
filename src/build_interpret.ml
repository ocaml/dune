open! Stdune
open Import
open Build.Repr

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
  loop (Build.repr t) Static_deps.empty true

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
  fun t -> loop (Build.repr t) Lib_name.Map.empty

let targets =
  let rec loop : type a b. (a, b) t -> Path.Set.t -> Path.Set.t = fun t acc ->
    match t with
    | Arr _ -> acc
    | Targets targets -> Path.Set.union targets acc
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
          let a = loop a Path.Set.empty in
          let b = loop b Path.Set.empty in
          if Path.Set.is_empty a && Path.Set.is_empty b then
            acc
          else begin
            Exn.code_error "Build_interpret.targets: cannot have targets \
                            under a [if_file_exists]"
              [ "targets-a", Path.Set.to_sexp a
              ; "targets-b", Path.Set.to_sexp b
              ]
          end
      end
    | Memo m -> loop m.t acc
    | Catch (t, _) -> loop t acc
    | Lazy_no_targets _ -> acc
  in
  fun t -> loop (Build.repr t) Path.Set.empty

module Rule = struct
  type t =
    { context  : Context.t option
    ; env      : Env.t option
    ; build    : (unit, Action.t) Build.t
    ; targets  : Path.Set.t
    ; sandbox  : bool
    ; mode     : Dune_file.Rule.Mode.t
    ; locks    : Path.t list
    ; loc      : Loc.t option
    ; dir      : Path.t
    }

  let make ?(sandbox=false) ?(mode=Dune_file.Rule.Mode.Not_a_rule_stanza)
        ~context ~env ?(locks=[]) ?loc build =
    let targets = targets build in
    let dir =
      match Path.Set.choose targets with
      | None -> begin
          match loc with
          | Some loc -> Errors.fail loc "Rule has no targets specified"
          | None -> Exn.code_error "Build_interpret.Rule.make: no targets" []
        end
      | Some x ->
        let dir = Path.parent_exn x in
        if Path.Set.exists targets ~f:(fun path -> Path.parent_exn path <> dir)
        then begin
          match loc with
          | None ->
            Exn.code_error "rule has targets in different directories"
              [ "targets", Path.Set.to_sexp targets
              ]
          | Some loc ->
            Errors.fail loc
              "Rule has targets in different directories.\nTargets:\n%s"
              (String.concat ~sep:"\n"
                 (Path.Set.to_list targets |> List.map ~f:(fun p ->
                    sprintf "- %s"
                      (Path.to_string_maybe_quoted p))))
        end;
        dir
    in
    { context
    ; env
    ; build
    ; targets
    ; sandbox
    ; mode
    ; locks
    ; loc
    ; dir
    }
end
