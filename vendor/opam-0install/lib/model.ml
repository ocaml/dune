(* Note: changes to this file may require similar changes to lib-cudf/model.ml *)

module Make (Monad : S.Monad) (Context : S.CONTEXT with type 'a monad = 'a Monad.t) = struct
  type 'a monad = 'a Monad.t
  (* Note: [OpamFormula.neg] doesn't work in the [Empty] case, so we just
     record whether to negate the result here. *)
  type restriction = {
    kind : [ `Ensure | `Prevent ];
    expr : OpamFormula.version_formula;
  }

  type real_role = {
    context : Context.t;
    name : OpamPackage.Name.t;
  }

  type role =
    | Real of real_role               (* A role is usually an opam package name *)
    | Virtual of int * impl list      (* (int just for sorting) *)
  and real_impl = {
    context : Context.t;
    pkg : OpamPackage.t;
    opam : OpamFile.OPAM.t;
    requires : dependency list;
  }
  and dependency = {
    drole : role;
    importance : [ `Essential | `Recommended | `Restricts ];
    restrictions : restriction list;
  }
  and impl =
    | RealImpl of real_impl                     (* An implementation is usually an opam package *)
    | VirtualImpl of int * dependency list      (* (int just for sorting) *)
    | Reject of OpamPackage.t
    | Dummy                                     (* Used for diagnostics *)

  let rec pp_version f = function
    | RealImpl impl -> Fmt.string f @@ OpamPackage.Version.to_string (OpamPackage.version impl.pkg)
    | Reject pkg -> Fmt.string f @@ OpamPackage.version_to_string pkg
    | VirtualImpl (_i, deps) -> Fmt.(list ~sep:(any "&") pp_role) f (List.map (fun d -> d.drole) deps)
    | Dummy -> Fmt.string f "(no version)"
  and pp_impl f = function
    | RealImpl impl -> Fmt.string f (OpamPackage.to_string impl.pkg)
    | Reject pkg -> Fmt.string f @@ OpamPackage.to_string pkg
    | VirtualImpl _ as x -> pp_version f x
    | Dummy -> Fmt.string f "(no solution found)"
  and pp_role f = function
    | Real t -> Fmt.string f (OpamPackage.Name.to_string t.name)
    | Virtual (_, impls) -> Fmt.pf f "%a" Fmt.(list ~sep:(any "|") pp_impl) impls

  let pp_impl_long = pp_impl

  module Role = struct
    type t = role

    let pp = pp_role

    let compare a b =
      match a, b with
      | Real a, Real b -> OpamPackage.Name.compare a.name b.name
      | Virtual (a, _), Virtual (b, _) -> compare (a : int) b
      | Real _, Virtual _ -> -1
      | Virtual _, Real _ -> 1
  end

  let role context name = Real { context; name }

  open Monad.O

  let fresh_id =
    let i = ref 0 in
    fun () ->
      incr i;
      !i

  let virtual_impl ~context ~depends () =
    let depends = depends |> List.map (fun name ->
        let drole = role context name in
        let importance = `Essential in
        { drole; importance; restrictions = []}
      ) in
    VirtualImpl (fresh_id (), depends)

  let virtual_role impls =
    Virtual (fresh_id (), impls)

  type command = |          (* We don't use 0install commands anywhere *)
  type command_name = private string
  let pp_command _ = function (_:command) -> .
  let command_requires _role = function (_:command) -> .
  let get_command _impl _command_name = None

  type dep_info = {
    dep_role : Role.t;
    dep_importance : [ `Essential | `Recommended | `Restricts ];
    dep_required_commands : command_name list;
  }

  type requirements = {
    role : Role.t;
    command : command_name option;
  }

  let dummy_impl = Dummy

  (* Turn an opam dependency formula into a 0install list of dependencies. *)
  let list_deps ~context ~importance deps =
    let open OpamTypes in
    let rec aux = function
      | Empty -> []
      | Atom (name, restrictions) ->
        let drole = role context name in
        [{ drole; restrictions; importance }]
      | Block x -> aux x
      | And (x, y) -> aux x @ aux y
      | Or _ as o ->
        let impls = group_ors o in
        let drole = virtual_role impls in
        (* Essential because we must apply a restriction, even if its
           components are only restrictions. *)
        [{ drole; restrictions = []; importance = `Essential }]
    and group_ors = function
      | Or (x, y) -> group_ors x @ group_ors y
      | expr -> [VirtualImpl (fresh_id (), aux expr)]
    in
    aux deps

  let requires _ = function
    | Dummy | Reject _ -> [], []
    | VirtualImpl (_, deps) -> deps, []
    | RealImpl impl -> impl.requires, []

  let dep_info { drole; importance; restrictions = _ } =
    { dep_role = drole; dep_importance = importance; dep_required_commands = [] }

  type role_information = {
    replacement : Role.t option;
    impls : impl list;
  }

  type machine_group = private string   (* We don't use machine groups because opam is source-only. *)
  let machine_group _impl = None

  type conflict_class = string

  let conflict_class = function
    | RealImpl impl ->
      OpamFile.OPAM.conflict_class impl.opam |> List.map OpamPackage.Name.to_string
    | VirtualImpl _ -> []
    | Dummy | Reject _ -> []

  (* Opam uses conflicts, e.g.
       conflicts if X {> 1} OR Y {< 1 OR > 2}
     whereas 0install uses restricts, e.g.
       restrict to X {<= 1} AND Y {>= 1 AND <= 2}

     Warning: [OpamFormula.neg _ Empty = Empty], so does NOT reverse the result in this case.
     For empty conflicts this is fine (don't conflict with anything, just like an empty depends
     list). But for the version expressions inside, it's wrong: a conflict with no expression
     conflicts with all versions and should restrict the choice to nothing, not to everything.
     So, we just tag the formula as [`Prevent] instead of negating it. *)
  let prevent f =
    OpamFormula.neg Fun.id f
    |> OpamFormula.map (fun (a, expr) -> OpamFormula.Atom (a, [{ kind = `Prevent; expr }]))

  let ensure =
    OpamFormula.map (fun (name, vexpr) ->
        let rlist =
          match vexpr with
          | OpamFormula.Empty -> []
          | r                 -> [{ kind = `Ensure; expr = r }]
        in
        OpamFormula.Atom (name, rlist)
      )

  (* Get all the candidates for a role. *)
  let implementations = function
    | Virtual (_, impls) -> Monad.return { impls; replacement = None }
    | Real role ->
      let context = role.context in
      let+ impls =
        Context.candidates context role.name
        >>| List.filter_map (function
            | _, Error _rejection -> None
            | version, Ok opam ->
              let pkg = OpamPackage.create role.name version in
              let requires =
                let make_deps importance xform get =
                  get opam
                  |> Context.filter_deps context pkg
                  |> xform
                  |> list_deps ~context ~importance
                in
                make_deps `Essential ensure OpamFile.OPAM.depends @
                make_deps `Restricts prevent OpamFile.OPAM.conflicts @
                make_deps `Recommended ensure OpamFile.OPAM.depopts
              in
              Some (RealImpl { context; pkg; opam; requires })
          )
      in
      { impls; replacement = None }

  let restrictions dependency = dependency.restrictions

  let meets_restriction impl { kind; expr } =
    match impl with
    | Dummy -> true
    | VirtualImpl _ -> assert false        (* Can't constrain version of a virtual impl! *)
    | Reject _ -> false
    | RealImpl impl ->
      let result = OpamFormula.check_version_formula expr (OpamPackage.version impl.pkg) in
      match kind with
      | `Ensure -> result
      | `Prevent -> not result

  type rejection = Context.rejection

  let rejects role =
    match role with
    | Virtual _ -> Monad.return ([], [])
    | Real role ->
      let context = role.context in
      let+ rejects =
        Context.candidates context role.name
        >>| List.filter_map (function
            | _, Ok _ -> None
            | version, Error reason ->
              let pkg = OpamPackage.create role.name version in
              Some (Reject pkg, reason)
          )
      in
      let notes = [] in
      rejects, notes

  let compare_version a b =
    match a, b with
    | RealImpl a, RealImpl b -> OpamPackage.compare a.pkg b.pkg
    | VirtualImpl (ia, _), VirtualImpl (ib, _) -> compare (ia : int) ib
    | Reject a, Reject b -> OpamPackage.compare a b
    | (RealImpl _ | Reject _ | VirtualImpl _ | Dummy),
      (RealImpl _ | Reject _ | VirtualImpl _ | Dummy)
      -> compare b a

  let user_restrictions = function
    | Virtual _ -> None
    | Real role ->
      match Context.user_restrictions role.context role.name with
      | None -> None
      | Some f -> Some { kind = `Ensure; expr = OpamFormula.Atom f }

  let format_machine _impl = "(src)"

  let string_of_op = function
    | `Eq -> "="
    | `Geq -> ">="
    | `Gt -> ">"
    | `Leq -> "<="
    | `Lt -> "<"
    | `Neq -> "<>"

  let string_of_version_formula = OpamFormula.string_of_formula (fun (rel, v) ->
      Printf.sprintf "%s %s" (string_of_op rel) (OpamPackage.Version.to_string v)
    )

  let string_of_restriction = function
    | { kind = `Prevent; expr = OpamFormula.Empty } -> "conflict with all versions"
    | { kind = `Prevent; expr } -> Fmt.str "not(%s)" (string_of_version_formula expr)
    | { kind = `Ensure; expr } -> string_of_version_formula expr

  let describe_problem _impl = Fmt.to_to_string Context.pp_rejection

  let version = function
    | RealImpl impl -> Some impl.pkg
    | Reject pkg -> Some pkg
    | VirtualImpl _ -> None
    | Dummy -> None

  let package_name = function
    | Real {name; _} -> Some name
    | Virtual _ -> None

  let formula { kind; expr } = (kind, expr)
end
