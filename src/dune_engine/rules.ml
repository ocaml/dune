open Import
module Id = Id.Make ()

module Dir_rules = struct
  module Alias_spec = struct
    type item =
      | Deps of unit Action_builder.t
      | Action of Rule.Anonymous_action.t Action_builder.t

    type t = { expansions : (Loc.t * item) Appendable_list.t } [@@unboxed]

    let union x y = { expansions = Appendable_list.( @ ) x.expansions y.expansions }
  end

  type alias =
    { name : Alias.Name.t
    ; spec : Alias_spec.t
    }

  type data =
    | Rule of Rule.t
    | Alias of alias

  type t = data Id.Map.t

  let dyn_of_data = function
    | Rule rule ->
      Dyn.Variant ("Rule", [ Record [ "targets", Targets.Validated.to_dyn rule.targets ] ])
    | Alias alias ->
      Dyn.Variant ("Alias", [ Record [ "name", Alias.Name.to_dyn alias.name ] ])
  ;;

  let to_dyn t = Dyn.(list dyn_of_data) (Id.Map.values t)

  type ready =
    { rules : Rule.t list
    ; aliases : Alias_spec.t Alias.Name.Map.t
    }

  let consume t =
    let rules, aliases =
      Id.Map.values t
      |> List.partition_map ~f:(function
        | Rule rule -> Left rule
        | Alias { name; spec } -> Right (name, spec))
    in
    let aliases =
      let add_item what = function
        | None -> Some what
        | Some base -> Some (Alias_spec.union what base)
      in
      (* This accumulates the aliases in reverse order, but there's another
         reversal whenever the expansion is inspected. The order doesn't really
         matter, but it does change the tests. So it's nice to maintain it if
         possible *)
      List.fold_left aliases ~init:Alias.Name.Map.empty ~f:(fun acc (name, item) ->
        Alias.Name.Map.update acc name ~f:(add_item item))
    in
    { rules; aliases }
  ;;

  let empty = Id.Map.empty
  let union_map a b ~f = Id.Map.union a b ~f:(fun _key a b -> Some (f a b))

  let union =
    union_map ~f:(fun a b ->
      assert (a == b);
      a)
  ;;

  let singleton (data : data) =
    let id = Id.gen () in
    Id.Map.singleton id data
  ;;

  let add t data =
    let id = Id.gen () in
    Id.Map.set t id data
  ;;

  let is_subset t ~of_ = Id.Map.is_subset t ~of_ ~f:(fun _ ~of_:_ -> true)
  let is_empty = Id.Map.is_empty

  module Nonempty : sig
    type maybe_empty = t
    type t = private maybe_empty

    val create : maybe_empty -> t option
    val to_dyn : t -> Dyn.t
    val union : t -> t -> t
    val singleton : data -> t
    val add : t -> data -> t
  end = struct
    type maybe_empty = t
    type nonrec t = t

    let to_dyn = to_dyn
    let create t = if is_empty t then None else Some t
    let union = union
    let singleton = singleton
    let add = add
  end
end

module T = struct
  type t = Dir_rules.Nonempty.t Path.Build.Map.t

  let empty = Path.Build.Map.empty
  let union_map a b ~f = Path.Build.Map.union a b ~f:(fun _key a b -> Some (f a b))
  let union = union_map ~f:Dir_rules.Nonempty.union
  let name = "Rules"
end

include T

let to_dyn = Path.Build.Map.to_dyn Dir_rules.Nonempty.to_dyn

let singleton_rule (rule : Rule.t) =
  let dir = rule.targets.root in
  Path.Build.Map.singleton dir (Dir_rules.Nonempty.singleton (Rule rule))
;;

let implicit_output = Memo.Implicit_output.add (module T)

let produce rules =
  if Path.Build.Map.is_empty rules
  then Memo.return ()
  else Memo.Implicit_output.produce implicit_output rules
;;

module Produce = struct
  let rule rule = produce (singleton_rule rule)

  module Alias = struct
    type t = Alias.t

    let alias t spec =
      produce
        (let dir = Alias.dir t in
         let name = Alias.name t in
         Path.Build.Map.singleton
           dir
           (Dir_rules.Nonempty.singleton (Alias { name; spec })))
    ;;

    let add_deps t ?(loc = Loc.none) expansion =
      alias
        t
        { expansions = Appendable_list.singleton (loc, Dir_rules.Alias_spec.Deps expansion)
        }
    ;;

    let add_action t ~loc action =
      let action =
        let open Action_builder.O in
        let+ action = action in
        { Rule.Anonymous_action.action
        ; loc
        ; dir = Alias.dir t
        ; alias = Some (Alias.name t)
        }
      in
      alias
        t
        { expansions = Appendable_list.singleton (loc, Dir_rules.Alias_spec.Action action)
        }
    ;;
  end
end

let of_dir_rules ~dir rules =
  match Dir_rules.Nonempty.create rules with
  | None -> Path.Build.Map.empty
  | Some rules -> Path.Build.Map.singleton dir rules
;;

let of_rules rules =
  List.fold_left rules ~init:Path.Build.Map.empty ~f:(fun acc rule ->
    Path.Build.Map.update acc rule.Rule.targets.root ~f:(function
      | None -> Some (Dir_rules.Nonempty.singleton (Rule rule))
      | Some acc -> Some (Dir_rules.Nonempty.add acc (Rule rule))))
;;

let directory_targets (rules : t) =
  Path.Build.Map.fold
    ~init:Path.Build.Map.empty
    rules
    ~f:(fun (dir_rules : Dir_rules.Nonempty.t) acc ->
      (dir_rules :> Dir_rules.t)
      |> Id.Map.fold ~init:acc ~f:(fun (data : Dir_rules.data) acc ->
        match data with
        | Alias _ -> acc
        | Rule rule ->
          Filename.Set.fold ~init:acc rule.targets.dirs ~f:(fun target acc ->
            let target = Path.Build.relative rule.targets.root target in
            Path.Build.Map.add_exn acc target rule.loc)))
;;

let collect f =
  let open Memo.O in
  let+ result, out = Memo.Implicit_output.collect implicit_output f in
  result, Option.value out ~default:T.empty
;;

let collect_unit f =
  let open Memo.O in
  let+ (), rules = collect f in
  rules
;;

let to_map x = (x : t :> Dir_rules.t Path.Build.Map.t)

let map t ~f =
  Path.Build.Map.map t ~f:(fun m ->
    Id.Map.to_list (m : Dir_rules.Nonempty.t :> Dir_rules.t)
    |> Id.Map.of_list_map_exn ~f:(fun (id, data) ->
      match f data with
      | `No_change -> id, data
      | `Changed data -> Id.gen (), data)
    |> Dir_rules.Nonempty.create
    |> Option.value_exn)
;;

let is_subset t ~of_ =
  Path.Build.Map.is_subset (to_map t) ~of_:(to_map of_) ~f:Dir_rules.is_subset
;;

let map_rules t ~f =
  map t ~f:(function
    | (Alias _ : Dir_rules.data) -> `No_change
    | Rule r -> `Changed (Rule (f r) : Dir_rules.data))
;;

let find t p =
  match Path.as_in_build_dir p with
  | None -> Dir_rules.empty
  | Some p ->
    (match Path.Build.Map.find t p with
     | Some dir_rules -> (dir_rules : Dir_rules.Nonempty.t :> Dir_rules.t)
     | None -> Dir_rules.empty)
;;

let prefix_rules prefix ~f =
  let open Memo.O in
  let* res, rules = collect f in
  let+ () =
    produce
      (map_rules rules ~f:(fun (rule : Rule.t) ->
         let t =
           let open Action_builder.O in
           prefix >>> rule.action
         in
         Rule.set_action rule t))
  in
  res
;;
