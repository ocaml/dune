(*
   This file includes code from the 0install library. It is distributed under
   the LGPL-2.1-or-later licence. See src/sdune_pkg/COPYING.md for the full
   license.

   Copyright (C) 2013, Thomas Leonard
   See the README file for details, or visit http://0install.net.

   Files:
   - Includes solver_core.ml and solver_core.mli files
   - Includes diagnostics.ml and diagnostics.mli
   - Remove usage from the S module from the s.ml file
*)

open Import
open Fiber.O

let with_test solver_env =
  match Solver_env.get solver_env Package_variable_name.with_test with
  | Some v -> Variable_value.equal v Variable_value.true_
  | None -> true
;;

module Priority = struct
  (* A priority defines a package's position in the list of candidates
     fed to the solver. Any change to package selection should be reflected in
     this priority rather than implemented in an ad-hoc manner *)
  type t =
    { (* We prefer packages with [avoid-version: false] *)
      avoid : bool
    ; version : OpamPackage.Version.t
    }

  let compare_version =
    let ord x y = OpamPackage.Version.compare x y |> Ordering.of_int in
    fun (pref : Version_preference.t) x y ->
      match pref with
      | Oldest -> ord x y
      | Newest -> ord y x
  ;;

  let compare pref t { avoid; version } =
    Tuple.T2.compare
      Bool.compare
      (compare_version pref)
      (t.avoid, t.version)
      (avoid, version)
  ;;

  let make (package : OpamFile.OPAM.t) =
    let avoid = List.mem package.flags Pkgflag_AvoidVersion ~equal:Poly.equal in
    let version = OpamFile.OPAM.package package |> OpamPackage.version in
    { version; avoid }
  ;;

  let allowed version = { avoid = false; version }
end

module Context = struct
  type rejection =
    | Unavailable
    | Refuted_by of Package_name.t

  let local_package_default_version =
    Package_version.to_opam_package_version Lock_dir.Pkg_info.default_version
  ;;

  type candidate_origin =
    | Local
    | Pinned
    | Repository

  type candidate =
    { priority : Priority.t
    ; opam : OpamFile.OPAM.t
    ; origin : candidate_origin
    }

  type candidates =
    { resolved : Resolved_package.t OpamPackage.Version.Map.t
    ; unfiltered : candidate list
    }

  type local_package =
    { opam_file : OpamFile.OPAM.t
    ; name : Package_name.t
    ; version : OpamPackage.Version.t
    ; depends : OpamTypes.filtered_formula
    ; conflicts : OpamTypes.filtered_formula
    ; filtered_formulas_by_platform :
        (Solver_env.t, OpamTypes.formula * OpamTypes.formula) Table.t
    }

  type t =
    { repos : Opam_repo.t list
    ; version_preference : Version_preference.t
    ; pinned_packages : Resolved_package.t Package_name.Map.t
    ; local_packages : local_package Package_name.Map.t Lazy.t
    ; local_constraints : (Package_name.t, local_package list) Table.t Lazy.t
    ; solver_env : Solver_env.t
      (* Base solver env. Full platform-specific envs are computed by extending
         this with the platform's own env. *)
    ; dune_version : OpamPackage.Version.t
    ; platforms : Solver_env.t list
    ; stats_updater : Solver_stats.Updater.t
    ; candidates_cache : (Package_name.t, candidates) Fiber.Cache.t
    ; platform_available_cache : (OpamPackage.t, (Solver_env.t, bool) Table.t) Table.t
    ; constraints : OpamTypes.filtered_formula Package_name.Map.t
    ; (* Number of versions of each package whose opam files were read from
         disk while solving. Used to report performance statistics. *)
      expanded_packages : (Package_name.t, int) Table.t
    }

  let create
        ~pinned_packages
        ~solver_env
        ~platforms
        ~repos
        ~local_packages
        ~version_preference
        ~stats_updater
        ~constraints
    =
    let candidates_cache = Fiber.Cache.create (module Package_name) in
    let constraints =
      List.map constraints ~f:(fun (constraint_ : Package_dependency.t) ->
        constraint_.name, constraint_)
      |> Package_name.Map.of_list_multi
      |> Package_name.Map.map ~f:(fun formulae ->
        List.map formulae ~f:Package_dependency.to_opam_filtered_formula
        |> OpamFormula.ands)
    in
    let module Opam_package_key = struct
      include OpamPackage

      let to_dyn = Opam_dyn.package
    end
    in
    let platform_available_cache = Table.create (module Opam_package_key) 1 in
    let expanded_packages = Table.create (module Package_name) 1 in
    let local_constraints =
      lazy
        (let acc = Table.create (module Package_name) 20 in
         let packages pkg (formula : OpamTypes.filtered_formula) =
           OpamFormula.iter
             (fun (name, _) ->
                let name = Package_name.of_opam_package_name name in
                Table.Multi.cons acc name pkg)
             formula
         in
         Lazy.force local_packages
         |> Package_name.Map.iter ~f:(fun pkg ->
           packages pkg pkg.depends;
           packages pkg pkg.conflicts);
         acc)
    in
    { repos
    ; version_preference
    ; local_packages
    ; pinned_packages
    ; solver_env =
        Solver_env.add_sentinel_values_for_unset_platform_vars solver_env
        (* The platform envs don't need sentinel values - they only contain
         platform-specific vars that will override the sentinels in solver_env
         when extended. *)
    ; dune_version = Dune_dep.version
    ; platforms
    ; stats_updater
    ; candidates_cache
    ; platform_available_cache
    ; constraints
    ; expanded_packages
    ; local_constraints
    }
  ;;

  (* Compute the full platform-specific env by extending the base env with the
     platform's own (platform-specific) env. *)
  let platform_env t platform = Solver_env.extend t.solver_env platform

  let pp_rejection = function
    | Unavailable -> Pp.paragraph "Availability condition not satisfied"
    | Refuted_by pkg ->
      Pp.paragraphf
        "Package does not satisfy constraints of local package %s"
        (Package_name.to_string pkg)
  ;;

  let eval_to_bool (filter : OpamTypes.filter) =
    try OpamFilter.eval_to_bool ~default:false (Fun.const None) filter with
    | Invalid_argument _ -> false
  ;;

  let is_opam_available_in_env t ~solver_env opam =
    let package = OpamFile.OPAM.package opam in
    OpamFile.OPAM.available opam
    |> OpamFilter.partial_eval
         (Solver_env.to_env solver_env
          |> Solver_stats.Updater.wrap_env t.stats_updater
          |> Lock_pkg.add_self_to_filter_env package)
    |> eval_to_bool
  ;;

  let is_available_for_platform t ~platform opam =
    let package = OpamFile.OPAM.package opam in
    let available_by_platform =
      Table.find_or_add t.platform_available_cache package ~f:(fun _ ->
        Table.create (module Solver_env) 1)
    in
    Table.find_or_add available_by_platform platform ~f:(fun platform ->
      is_opam_available_in_env t ~solver_env:(platform_env t platform) opam)
  ;;

  let pinned_candidate resolved_package =
    let version = Resolved_package.package resolved_package |> OpamPackage.version in
    let opam_file = Resolved_package.opam_file resolved_package in
    let resolved = OpamPackage.Version.Map.singleton version resolved_package in
    (* We don't respect avoid-version for pinned packages. This is intentional. *)
    { unfiltered =
        [ { priority = Priority.allowed version; opam = opam_file; origin = Pinned } ]
    ; resolved
    }
  ;;

  (* Filter deps using a specific solver_env *)
  let filter_deps_with_env t ~solver_env package filtered_formula =
    (* Add additional constraints to the formula. This works in two steps.
       First identify all the additional constraints applied to packages which
       appear in the current package's dependency formula. Then each additional
       constnraint is and-ed with the current package's dependency formula. *)
    let filtered_formula =
      OpamFormula.fold_left
        (fun additional_formulae (pkg, _) ->
           match
             Package_name.of_opam_package_name pkg |> Package_name.Map.find t.constraints
           with
           | None -> additional_formulae
           | Some additional -> additional :: additional_formulae)
        []
        filtered_formula
      |> List.fold_left ~init:filtered_formula ~f:(fun additional acc ->
        OpamFormula.And (acc, additional))
    in
    let package_is_local =
      OpamPackage.name package
      |> Package_name.of_opam_package_name
      |> Package_name.Map.mem (Lazy.force t.local_packages)
    in
    let with_test = package_is_local && with_test solver_env in
    Solver_env.to_env solver_env
    |> Solver_stats.Updater.wrap_env t.stats_updater
    |> Lock_pkg.add_self_to_filter_env package
    |> Resolve_opam_formula.apply_filter ~with_test ~formula:filtered_formula
  ;;

  (* Filter deps for a specific platform *)
  let filter_deps t ~platform package filtered_formula =
    let solver_env = platform_env t platform in
    filter_deps_with_env t ~solver_env package filtered_formula
  ;;

  let filtered_local_formulas t ~platform local_package =
    Table.find_or_add
      local_package.filtered_formulas_by_platform
      platform
      ~f:(fun platform ->
        let package =
          OpamPackage.create
            (OpamFile.OPAM.name local_package.opam_file)
            local_package.version
        in
        let solver_env = platform_env t platform in
        ( filter_deps_with_env t ~solver_env package local_package.depends
        , filter_deps_with_env t ~solver_env package local_package.conflicts ))
  ;;

  exception Found of Package_name.t

  let try_refute t ~platform package =
    let version = OpamPackage.version package in
    match
      let name = Package_name.of_opam_package_name (OpamPackage.name package) in
      Table.find (Lazy.force t.local_constraints) name
    with
    | None -> None
    | Some local_packages ->
      (try
         List.iter local_packages ~f:(fun pkg ->
           let depends, conflicts = filtered_local_formulas t ~platform pkg in
           match
             match
               depends
               |> OpamFormula.partial_eval (fun (name', f) ->
                 if OpamPackage.Name.equal name' (OpamPackage.name package)
                 then
                   if OpamFormula.check_version_formula f version then `True else `False
                 else `Formula (Atom (name', f)))
             with
             | `False -> `Reject
             | `Formula _ | `True ->
               (match
                  conflicts
                  |> OpamFormula.partial_eval (fun (name', f) ->
                    if OpamPackage.Name.equal name' (OpamPackage.name package)
                    then
                      if OpamFormula.check_version_formula f version
                      then `True
                      else `False
                    else `Formula (Atom (name', f)))
                with
                | `True -> `Reject
                | `Formula _ | `False -> `Continue)
           with
           | `Continue -> ()
           | `Reject -> raise_notrace (Found pkg.name));
         None
       with
       | Found p -> Some p)
  ;;

  let refuted_on_all_platforms t package =
    List.for_all t.platforms ~f:(fun platform ->
      Option.is_some (try_refute t ~platform package))
  ;;

  let pre_rejections t ~platform name =
    let package_name = Package_name.of_opam_package_name name in
    if
      Package_name.Map.mem (Lazy.force t.local_packages) package_name
      || Package_name.Map.mem t.pinned_packages package_name
    then []
    else
      Opam_repo.all_packages_versions_map t.repos name
      |> OpamPackage.Version.Map.keys
      |> List.filter_map ~f:(fun version ->
        let package = OpamPackage.create name version in
        if refuted_on_all_platforms t package
        then
          try_refute t ~platform package
          |> Option.map ~f:(fun name -> package, Refuted_by name)
        else None)
  ;;

  let rejection_for_platform t ~platform { opam; origin; _ } =
    match origin with
    | Local -> None
    | Pinned ->
      if is_available_for_platform t ~platform opam then None else Some Unavailable
    | Repository ->
      if not (is_available_for_platform t ~platform opam)
      then Some Unavailable
      else
        OpamFile.OPAM.package opam
        |> try_refute t ~platform
        |> Option.map ~f:(fun package -> Refuted_by package)
  ;;

  let repo_candidate t name =
    let versions =
      Opam_repo.all_packages_versions_map t.repos name
      |> OpamPackage.Version.Map.filter (fun version _ ->
        let package = OpamPackage.create name version in
        not (refuted_on_all_platforms t package))
    in
    let+ resolved = Opam_repo.load_all_versions_by_keys versions in
    Table.add_exn
      t.expanded_packages
      (Package_name.of_opam_package_name name)
      (OpamPackage.Version.Map.cardinal resolved);
    let unfiltered =
      OpamPackage.Version.Map.values resolved
      |> List.map ~f:(fun resolved_package ->
        let opam = Resolved_package.opam_file resolved_package in
        { priority = Priority.make opam; opam; origin = Repository })
      |> List.sort ~compare:(fun x y ->
        Priority.compare t.version_preference x.priority y.priority)
    in
    { unfiltered; resolved }
  ;;

  (* Get all candidates with their opam files, without pre-filtering by availability.
     Used for multi-platform solving where availability is checked per-platform. *)
  let candidates_unfiltered t name =
    let* () = Fiber.return () in
    let key = Package_name.of_opam_package_name name in
    match Package_name.Map.find (Lazy.force t.local_packages) key with
    | Some local_package ->
      let priority = Priority.allowed local_package.version in
      Fiber.return [ { priority; opam = local_package.opam_file; origin = Local } ]
    | None ->
      let+ res =
        Fiber.Cache.find_or_add t.candidates_cache key ~f:(fun () ->
          match Package_name.Map.find t.pinned_packages key with
          | Some resolved_package -> Fiber.return (pinned_candidate resolved_package)
          | None -> repo_candidate t name)
      in
      res.unfiltered
  ;;

  let user_restrictions : t -> OpamPackage.Name.t -> OpamFormula.version_constraint option
    =
    fun t pkg ->
    (* This isn't really needed because we already pin dune, but it seems to
       help the error messages *)
    if Package_name.equal Dune_dep.name (Package_name.of_opam_package_name pkg)
    then Some (`Eq, t.dune_version)
    else None
  ;;

  let count_expanded_packages t = Table.fold t.expanded_packages ~init:0 ~f:( + )
end

module Solver = struct
  open Pp.O

  module Dep_kind = struct
    type t =
      | Ensure
      | Prevent
  end

  (* Filter a list, keeping the first occurrence of each [Some name] key.
     Items whose [key] is [None] are kept unconditionally. *)
  let filter_dedup_by_name ~key items =
    let seen = ref OpamPackage.Name.Set.empty in
    List.filter items ~f:(fun item ->
      match key item with
      | None -> true
      | Some name ->
        if OpamPackage.Name.Set.mem name !seen
        then false
        else (
          seen := OpamPackage.Name.Set.add name !seen;
          true))
  ;;

  (* Copyright (c) 2020 Thomas Leonard <talex5@gmail.com>

     Permission to use, copy, modify, and distribute this software for any
     purpose with or without fee is hereby granted, provided that the above
     copyright notice and this permission notice appear in all copies.

     THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
     WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
     MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
     ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
     WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
     ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
     OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  *)
  module Input = struct
    (* Note: [OpamFormula.neg] doesn't work in the [Empty] case, so we just
       record whether to negate the result here. *)
    module Restriction = struct
      type t =
        { kind : Dep_kind.t
        ; expr : OpamFormula.version_formula
        }

      let string_of_version_formula =
        let string_of_op =
          let pos =
            { OpamParserTypes.FullPos.filename = ""; start = 0, 0; stop = 0, 0 }
          in
          fun pelem -> OpamPrinter.FullPos.relop { pelem; pos }
        in
        OpamFormula.string_of_formula (fun (rel, v) ->
          Printf.sprintf "%s %s" (string_of_op rel) (OpamPackage.Version.to_string v))
      ;;

      let to_string = function
        | { kind = Prevent; expr = OpamFormula.Empty } -> "conflict with all versions"
        | { kind = Prevent; expr } ->
          Format.sprintf "not(%s)" (string_of_version_formula expr)
        | { kind = Ensure; expr } -> string_of_version_formula expr
      ;;
    end

    module Virtual_id = Id.Make ()

    module Rank : sig
      type t
      type assign

      val compare : t -> t -> Ordering.t
      val bottom : t
      val of_int : int -> t
      val next : assign -> t
      val assign : unit -> assign
    end = struct
      type t = int

      let bottom = -1
      let of_int x = x
      let compare = Int.compare

      type assign = int ref

      let assign () = ref 0

      let next t =
        let res = !t in
        incr t;
        res
      ;;
    end

    type role =
      | Real of OpamPackage.Name.t * Solver_env.t
      | Virtual of Virtual_id.t * impl list

    and real_impl =
      { pkg : OpamPackage.t
      ; conflict_class : OpamPackage.Name.t list
      ; requires : dependency list Lazy.t
      ; avoid : bool
      }

    and dependency =
      { drole : role
      ; importance : Dep_kind.t
      ; restrictions : Restriction.t list
      }

    and impl =
      | RealImpl of real_impl (* An implementation is usually an opam package *)
      | VirtualImpl of Rank.t * dependency list (* (rank just for sorting) *)
      | Reject of OpamPackage.t
      | Dummy (* Used for diagnostics *)

    (* Deduplicate a list of dependencies by package name for display purposes.
       This avoids showing the same package multiple times for different platforms. *)
    let deduplicate_deps_by_name =
      filter_dedup_by_name ~key:(fun (dependency : dependency) ->
        match dependency.drole with
        | Virtual _ -> None
        | Real (name, _) -> Some name)
    ;;

    let rec pp_version = function
      | RealImpl impl ->
        Pp.text (OpamPackage.Version.to_string (OpamPackage.version impl.pkg))
      | Reject pkg -> Pp.text (OpamPackage.version_to_string pkg)
      | VirtualImpl (_i, deps) ->
        let deps = deduplicate_deps_by_name deps in
        Pp.concat_map ~sep:(Pp.char '&') deps ~f:(fun d -> pp_role d.drole)
      | Dummy -> Pp.text "(no version)"

    and pp_impl = function
      | RealImpl impl -> Pp.text (OpamPackage.to_string impl.pkg)
      | Reject pkg -> Pp.text (OpamPackage.to_string pkg)
      | VirtualImpl _ as x -> pp_version x
      | Dummy -> Pp.text "(no solution found)"

    and pp_role = function
      | Real (name, _platform) ->
        (* Don't show platform in user-facing output for now *)
        Pp.text (OpamPackage.Name.to_string name)
      | Virtual (_, impls) -> Pp.concat_map ~sep:(Pp.char '|') impls ~f:pp_impl
    ;;

    let pp_impl_long = pp_impl

    module Role = struct
      module T = struct
        type t = role

        let compare a b =
          match a, b with
          | Real (a_name, a_platform), Real (b_name, b_platform) ->
            (match Ordering.of_int (OpamPackage.Name.compare a_name b_name) with
             | Eq -> Solver_env.compare a_platform b_platform
             | x -> x)
          | Virtual (a, _), Virtual (b, _) -> Virtual_id.compare a b
          | Real _, Virtual _ -> Lt
          | Virtual _, Real _ -> Gt
        ;;

        let to_dyn = Dyn.opaque
      end

      include T

      let equal x y = Ordering.is_eq (compare x y)

      let hash = function
        | Real (name, platform) ->
          let hash = Hash.create () in
          let hash = Hash.feed hash 0 in
          let hash = Hash.feed hash (OpamPackage.Name.to_string name |> String.hash) in
          Hash.feed hash (Solver_env.hash platform) |> Hash.hash
        | Virtual (id, _) ->
          let hash = Hash.create () in
          let hash = Hash.feed hash 1 in
          Hash.feed hash (Virtual_id.hash id) |> Hash.hash
      ;;

      let platform = function
        | Real (_, platform) -> Some platform
        | Virtual _ -> None
      ;;

      let user_restrictions t context =
        match t with
        | Virtual _ -> None
        | Real (name, _platform) ->
          Context.user_restrictions context name
          |> Option.map ~f:(fun f ->
            { Restriction.kind = Ensure; expr = OpamFormula.Atom f })
      ;;

      let pp = pp_role

      let rejects role context =
        match role with
        | Virtual _ -> Fiber.return ([], [])
        | Real (name, platform) ->
          let pre_rejections =
            Context.pre_rejections context ~platform name
            |> List.map ~f:(fun (package, reason) -> Reject package, reason)
          in
          let+ rejects =
            Context.candidates_unfiltered context name
            >>| List.filter_map ~f:(fun ({ Context.priority; _ } as candidate) ->
              match Context.rejection_for_platform context ~platform candidate with
              | None -> None
              | Some reason ->
                let pkg = OpamPackage.create name priority.version in
                Some (Reject pkg, reason))
          in
          let rejects = pre_rejections @ rejects in
          let notes = [] in
          rejects, notes
      ;;

      module Map = Map.Make (T)
    end

    let%expect_test "equal roles have equal hashes" =
      let bindings =
        [ Package_variable_name.arch, "x86_64"
        ; Package_variable_name.os, "linux"
        ; Package_variable_name.os_version, "24.04"
        ; Package_variable_name.os_distribution, "ubuntu"
        ; Package_variable_name.os_family, "debian"
        ; Package_variable_name.sys_ocaml_version, "5.3.0"
        ]
      in
      let platform bindings =
        List.fold_left bindings ~init:Solver_env.empty ~f:(fun env (name, value) ->
          Solver_env.set env name (Variable_value.string value))
      in
      let name = OpamPackage.Name.of_string "foo" in
      let real_a = Real (name, platform bindings) in
      let real_b = Real (name, platform (List.rev bindings)) in
      let virtual_id = Virtual_id.gen () in
      let virtual_a = Virtual (virtual_id, []) in
      let virtual_b = Virtual (virtual_id, [ Dummy ]) in
      printfn
        "real_equal=%b real_hash_equal=%b virtual_equal=%b virtual_hash_equal=%b"
        (Role.equal real_a real_b)
        (Role.hash real_a = Role.hash real_b)
        (Role.equal virtual_a virtual_b)
        (Role.hash virtual_a = Role.hash virtual_b);
      [%expect
        {|
        real_equal=true real_hash_equal=true virtual_equal=true virtual_hash_equal=true
        |}]
    ;;

    module Impl = struct
      type t = impl

      let pp = pp_impl

      let requires _ = function
        | Dummy | Reject _ -> []
        | VirtualImpl (_, deps) -> deps
        | RealImpl impl -> Lazy.force impl.requires
      ;;

      let conflict_class = function
        | RealImpl impl -> impl.conflict_class
        | VirtualImpl _ -> []
        | Dummy | Reject _ -> []
      ;;

      let version = function
        | RealImpl impl -> Some impl.pkg
        | Reject pkg -> Some pkg
        | VirtualImpl _ -> None
        | Dummy -> None
      ;;

      let avoid = function
        | RealImpl { avoid; _ } -> avoid
        | Reject _ | VirtualImpl _ | Dummy -> false
      ;;

      let compare_version a b =
        match a, b with
        | RealImpl a, RealImpl b ->
          (* CR-someday rgrinberg: shouldn't we take our version preference into account here? *)
          Ordering.of_int (OpamPackage.compare a.pkg b.pkg)
        | RealImpl _, _ -> Gt
        | _, RealImpl _ -> Lt
        | VirtualImpl (ia, _), VirtualImpl (ib, _) -> Rank.compare ia ib
        | VirtualImpl _, _ -> Gt
        | _, VirtualImpl _ -> Lt
        | Reject a, Reject b -> Ordering.of_int (OpamPackage.compare a b)
        | Reject _, _ -> Gt
        | _, Reject _ -> Lt
        | Dummy, Dummy -> Eq
      ;;
    end

    let virtual_role impls =
      let impls =
        List.mapi impls ~f:(fun i -> function
          | VirtualImpl (_, x) -> VirtualImpl (Rank.of_int i, x)
          | x -> x)
      in
      Virtual (Virtual_id.gen (), impls)
    ;;

    (* Opam uses conflicts, e.g.
       conflicts if X {> 1} OR Y {< 1 OR > 2}
     whereas 0install uses restricts, e.g.
       restrict to X {<= 1} AND Y {>= 1 AND <= 2}

     Warning: [OpamFormula.neg _ Empty = Empty], so does NOT reverse the result in this case.
     For empty conflicts this is fine (don't conflict with anything, just like an empty depends
     list). But for the version expressions inside, it's wrong: a conflict with no expression
     conflicts with all versions and should restrict the choice to nothing, not to everything.
     So, we just tag the formula as [Prevent] instead of negating it. *)
    let prevent f =
      OpamFormula.neg Fun.id f
      |> OpamFormula.map (fun (a, expr) ->
        OpamFormula.Atom (a, [ { Restriction.kind = Prevent; expr } ]))
    ;;

    let ensure =
      OpamFormula.map (fun (name, vexpr) ->
        let rlist =
          match vexpr with
          | OpamFormula.Empty -> []
          | r -> [ { Restriction.kind = Ensure; expr = r } ]
        in
        OpamFormula.Atom (name, rlist))
    ;;

    (* Turn an opam dependency formula into a 0install list of dependencies. *)
    let list_deps ~importance ~rank ~platform deps =
      let rec aux (formula : _ OpamTypes.generic_formula) =
        match formula with
        | Empty -> []
        | Atom (name, restrictions) ->
          [ { drole = Real (name, platform); restrictions; importance } ]
        | Block x -> aux x
        | And (x, y) -> aux x @ aux y
        | Or _ as o ->
          let impls = group_ors o in
          let drole = virtual_role impls in
          (* Essential because we must apply a restriction, even if its
             components are only restrictions. *)
          [ { drole; restrictions = []; importance = Ensure } ]
      and group_ors = function
        | Or (x, y) -> group_ors x @ group_ors y
        | expr -> [ VirtualImpl (Rank.next rank, aux expr) ]
      in
      aux deps
    ;;

    (* Get all candidates for a role, applying availability and local-package
       constraints in that role's platform environment. *)
    let implementations role context =
      match role with
      | Virtual (_, impls) -> Fiber.return impls
      | Real (name, platform) ->
        Context.candidates_unfiltered context name
        >>| List.filter_map ~f:(fun ({ Context.priority; opam; _ } as candidate) ->
          match Context.rejection_for_platform context ~platform candidate with
          | Some _ -> None
          | None ->
            let { Priority.version; avoid } = priority in
            let pkg = OpamPackage.create name version in
            (* Note: we ignore depopts here: see opam/doc/design/depopts-and-features *)
            let requires =
              lazy
                (let rank = Rank.assign () in
                 let make_deps importance xform deps =
                   Context.filter_deps context ~platform pkg deps
                   |> xform
                   |> list_deps ~importance ~rank ~platform
                 in
                 (OpamFile.OPAM.depends opam |> make_deps Ensure ensure)
                 @ (OpamFile.OPAM.conflicts opam |> make_deps Prevent prevent))
            in
            let conflict_class = OpamFile.OPAM.conflict_class opam in
            Some (RealImpl { pkg; avoid; requires; conflict_class }))
    ;;

    let meets_restriction impl { Restriction.kind; expr } =
      match impl with
      | Dummy -> true
      | VirtualImpl _ -> assert false (* Can't constrain version of a virtual impl! *)
      | Reject _ -> false
      | RealImpl impl ->
        let result =
          OpamFormula.check_version_formula expr (OpamPackage.version impl.pkg)
        in
        (match kind with
         | Ensure -> result
         | Prevent -> not result)
    ;;
  end

  module Solver = struct
    (* Copyright (C) 2013, Thomas Leonard
     * See the README file for details, or visit http://0install.net.
     *)
    module Sat = Sat.Make (Input.Impl)

    type decision_state =
      (* The next candidate to try *)
      | Undecided of Sat.lit
      (* The dependencies to check next *)
      | Selected of Input.dependency list
      | Unselected

    type selection =
      { impl : Input.Impl.t (** The implementation chosen to fill the role *)
      ; var : Sat.lit
      }

    module Candidates = struct
      type t =
        { role : Input.Role.t
        ; clause : Sat.at_most_one_clause option
        ; vars : selection list
        }

      let selected t =
        let open Option.O in
        let+ lit = t.clause >>= Sat.get_selected in
        let impl = Sat.get_user_data_for_lit lit in
        { var = lit; impl }
      ;;

      let state t =
        match t.clause with
        | None -> Unselected (* There were never any candidates *)
        | Some clause ->
          (match Sat.get_selected clause with
           | Some lit ->
             (* We've already chosen which <implementation> to use. Follow dependencies. *)
             let impl = Sat.get_user_data_for_lit lit in
             Selected (Input.Impl.requires t.role impl)
           | None ->
             (match Sat.get_best_undecided clause with
              | Some lit -> Undecided lit
              | None -> Unselected (* No remaining candidates, and none was chosen. *)))
      ;;

      (* Add the implementations of an interface to the implementation cache
         (called the first time we visit it). *)
      let make_impl_clause sat context ~dummy_impl role =
        (* Insert dummy_impl (last) if we're trying to diagnose a problem. *)
        let+ impls =
          let+ impls = Input.implementations role context in
          (match dummy_impl with
           | None -> impls
           | Some dummy_impl -> impls @ [ dummy_impl ])
          |> List.map ~f:(fun impl ->
            let var = Sat.add_variable sat impl in
            { impl; var })
        in
        let clause =
          let impl_clause =
            match impls with
            | [] -> None
            | _ :: _ -> Some (Sat.at_most_one sat (List.map impls ~f:(fun s -> s.var)))
          in
          { role; clause = impl_clause; vars = impls }
        in
        clause, impls
      ;;
    end

    module At_most_group = struct
      (* A group of SAT literals, of which at most [nb] may be selected.
         The group may grow over time, so its constraint is reintroduced
         whenever it changes by [seal].  *)
      type t =
        { nb : int
        ; mutable vars : Sat.lit list
        ; mutable nb_vars : int
        ; mutable sealed : int
        }

      let create nb = { nb; vars = []; nb_vars = 0; sealed = 0 }

      let add t var =
        t.vars <- var :: t.vars;
        t.nb_vars <- t.nb_vars + 1
      ;;

      let seal sat t =
        if t.nb_vars <= t.nb || t.nb_vars = t.sealed
        then false
        else (
          t.sealed <- t.nb_vars;
          (* The new [at_most_clause] subsumes the previous one. *)
          let (_ : Sat.at_most_clause) = Sat.at_most sat t.nb t.vars in
          true)
      ;;
    end

    module Exclusive_group_map (Map : Map.S) = struct
      type t =
        { mutable groups : At_most_group.t Map.t
        ; mutable grown : Map.key list (* The groups that grew since the last [seal]. *)
        }

      let create () = { groups = Map.empty; grown = [] }

      let add t key var =
        let group =
          match Map.find t.groups key with
          | Some group -> group
          | None ->
            let group = At_most_group.create 1 in
            t.groups <- Map.set t.groups key group;
            group
        in
        At_most_group.add group var;
        t.grown <- key :: t.grown
      ;;

      let seal t sat =
        let grown = t.grown in
        t.grown <- [];
        List.fold_left grown ~init:false ~f:(fun added key ->
          match Map.find t.groups key with
          | None -> added
          | Some group -> At_most_group.seal sat group || added)
      ;;
    end

    module Conflict_classes = struct
      (* Key is (conflict_class_name, platform) to ensure conflict classes
         are enforced per-platform, not across platforms. In multi-platform
         solving, it's valid to select the same package with a conflict class
         on multiple platforms, but within each platform, at most one package
         with the conflict class can be selected. *)
      module Key = struct
        type t = OpamPackage.Name.t * Solver_env.t

        let compare (name1, plat1) (name2, plat2) =
          match Ordering.of_int (OpamPackage.Name.compare name1 name2) with
          | Eq -> Solver_env.compare plat1 plat2
          | x -> x
        ;;

        let to_dyn (name, plat) =
          Dyn.Tuple
            [ Dyn.string (OpamPackage.Name.to_string name); Solver_env.to_dyn plat ]
        ;;
      end

      include Exclusive_group_map (Map.Make (Key))

      (* Add [impl] to its conflict groups, if any.
         [role] is used to extract the platform for the group key.
         Virtual roles never carry conflict classes (see [Input.Impl.conflict_class]
         which returns [] for VirtualImpl), so there's nothing to do for them. *)
      let process t role impl_var impl =
        match role with
        | Input.Virtual _ -> ()
        | Input.Real (_, platform) ->
          Input.Impl.conflict_class impl
          |> List.iter ~f:(fun name -> add t (name, platform) impl_var)
      ;;
    end

    module Cross_platform_version = struct
      (* Implements @art-w's cross-platform version equality constraint from
         https://github.com/ocaml/dune/issues/13647. For each (package_name,
         version) appearing as an impl on any platform, introduce a "somewhere"
         SAT variable that is true iff some platform selected [package_name] at
         [version]. Two kinds of clauses are added:
         - For each per-platform impl: [impl_var implies somewhere].
         - At-most-one across the somewhere-vars of each [name].
         Together these force every platform that selects [name] to select the
         same version, while still permitting platforms to omit a package
         entirely (e.g. unix-only packages on Windows). *)

      module Groups = Exclusive_group_map (Package_name.Map)

      type t =
        { sat : Sat.t
        ; enabled : bool
        ; mutable somewhere_vars : Sat.lit OpamPackage.Map.t
        ; (* For each package name, the somewhere-vars across its versions, of
             which at most one can be selected. *)
          per_name : Groups.t
        }

      let create sat ~enabled =
        { sat
        ; enabled
        ; somewhere_vars = OpamPackage.Map.empty
        ; per_name = Groups.create ()
        }
      ;;

      let somewhere_var t package =
        let name = OpamPackage.name package in
        match OpamPackage.Map.find_opt package t.somewhere_vars with
        | Some v -> v
        | None ->
          (* The user data attached to this SAT variable is opaque to the SAT
             engine — we use Dummy since the variable doesn't represent a real
             package selection. *)
          let v = Sat.add_variable t.sat Input.Dummy in
          t.somewhere_vars <- OpamPackage.Map.add package v t.somewhere_vars;
          Groups.add t.per_name (Package_name.of_opam_package_name name) v;
          v
      ;;

      (* Return one canonical selection literal per package version. With
         multiple platforms this is the shared [somewhere] variable used for
         version equality. With one platform the implementation variable is
         already canonical, so no extra encoding is needed. *)
      let process t impl_var (impl : Input.impl) =
        match impl with
        | RealImpl real_impl ->
          let pkg = real_impl.pkg in
          let selection_var =
            if t.enabled
            then (
              let som = somewhere_var t pkg in
              Sat.implies t.sat impl_var [ som ] ~reason:"cross-platform version equality";
              som)
            else impl_var
          in
          Some (pkg, selection_var)
        | VirtualImpl _ | Reject _ | Dummy -> None
      ;;

      (* Add at-most-one over the somewhere-vars per package name. *)
      let seal t = Groups.seal t.per_name t.sat
    end

    (* Starting from [root_req], explore all the feeds and implementations we
       might need, adding all of them to [sat_problem]. *)
    let build_problem
          context
          root_req
          sat
          ~enforce_cross_platform_versions
          ~max_avoids
          ~dummy_impl
      =
      (* For each (iface, source) we have a list of implementations. *)
      let impl_cache = Fiber.Cache.create (module Input.Role) in
      let conflict_classes = Conflict_classes.create () in
      let cross_platform_versions =
        Cross_platform_version.create sat ~enabled:enforce_cross_platform_versions
      in
      let avoids = ref OpamPackage.Map.empty in
      let+ () =
        let rec lookup_impl expand_deps role =
          let impls = ref [] in
          let* candidates =
            Fiber.Cache.find_or_add impl_cache role ~f:(fun () ->
              let+ candidates, impls' =
                Candidates.make_impl_clause sat context ~dummy_impl role
              in
              impls := impls';
              candidates)
          in
          let+ () =
            Fiber.parallel_iter !impls ~f:(fun { var = impl_var; impl } ->
              Conflict_classes.process conflict_classes role impl_var impl;
              let version_selection =
                Cross_platform_version.process cross_platform_versions impl_var impl
              in
              (match version_selection with
               | Some (key, selection_var) when Input.Impl.avoid impl ->
                 avoids := OpamPackage.Map.add key selection_var !avoids
               | Some _ | None -> ());
              match expand_deps with
              | `No_expand -> Fiber.return ()
              | `Expand_and_collect_conflicts deferred ->
                Input.Impl.requires role impl
                |> Fiber.parallel_iter ~f:(fun (dep : Input.dependency) ->
                  match dep.importance with
                  | Ensure -> process_dep expand_deps impl_var dep
                  | Prevent ->
                    (* Defer processing restricting deps until all essential
                       deps have been processed for the entire problem.
                       Restricting deps will be processed later without
                       recurring into their dependencies. *)
                    deferred := (impl_var, dep) :: !deferred;
                    Fiber.return ()))
          in
          candidates
        and process_dep expand_deps user_var (dep : Input.dependency) : unit Fiber.t =
          (* Process a dependency of [user_var]:
             - find the candidate implementations to satisfy it
             - take just those that satisfy any restrictions in the dependency
             - ensure that we don't pick an incompatbile version if we select
               [user_var]
             - ensure that we do pick a compatible version if we select
               [user_var] (for "essential" dependencies only) *)
          let+ pass, fail =
            let+ candidates = lookup_impl expand_deps dep.drole in
            List.partition_map candidates.vars ~f:(fun { var; impl } ->
              if List.for_all ~f:(Input.meets_restriction impl) dep.restrictions
              then Left var
              else Right var)
          in
          match dep.importance with
          | Ensure ->
            Sat.implies
              sat
              ~reason:"essential dep"
              user_var
              pass (* Must choose a suitable candidate *)
          | Prevent ->
            (* If [user_var] is selected, don't select an incompatible version of
               the optional dependency. We don't need to do this explicitly in
               the [essential] case, because we must select a good version and we can't
               select two. *)
            (try
               let (_ : Sat.at_most_one_clause) =
                 Sat.at_most_one sat (user_var :: fail)
               in
               ()
             with
             | Invalid_argument reason ->
               (* Explicitly conflicts with itself! *)
               Sat.at_least_one sat [ Sat.neg user_var ] ~reason)
        in
        let conflicts = ref [] in
        let* () =
          (* This recursively builds the whole problem up. *)
          let+ candidates =
            lookup_impl (`Expand_and_collect_conflicts conflicts) root_req
          in
          List.map candidates.vars ~f:(fun x -> x.var)
          |> Sat.at_least_one sat ~reason:"need root" (* Must get what we came for! *)
        in
        (* Now process any restricting deps. Due to the cache, only restricting
           deps that aren't also an essential dep will be expanded. The solver will
           not process any transitive dependencies here since the dependencies of
           restricting dependencies are irrelevant to solving the dependency
           problem. *)
        List.rev !conflicts
        |> Fiber.parallel_iter ~f:(fun (impl_var, dep) ->
          process_dep `No_expand impl_var dep)
        (* All impl_candidates have now been added, so snapshot the cache. *)
      in
      let (_ : bool) = Conflict_classes.seal conflict_classes sat in
      let (_ : bool) = Cross_platform_version.seal cross_platform_versions in
      (match max_avoids, OpamPackage.Map.bindings !avoids |> List.map ~f:snd with
       | None, _ | _, [] -> ()
       | Some max_avoids, avoids ->
         let _ : Sat.at_most_clause = Sat.at_most sat max_avoids avoids in
         ());
      impl_cache
    ;;

    (** [do_solve model req] finds an implementation matching the given
        requirements, plus any other implementations needed
        to satisfy its dependencies.

        @param closest_match
          adds a lowest-ranked (but valid) implementation ([Input.dummy_impl]) to
          every interface, so we can always select something. Useful for diagnostics.
          Note: always try without [closest_match] first, or it may miss a valid solution.
        @param max_avoids
          if set, restricts the number of packages with the flag [avoid-version]. Used
          to minimize the number of those bad packages, but still find a solution when
          they are unavoidable.
        @return None if the solve fails (only happens if [closest_match] is false). *)
    let do_solve
          context
          ~closest_match
          ~enforce_cross_platform_versions
          ~max_avoids
          root_req
      =
      (* The basic plan is this:
         1. Scan the root interface and all dependencies recursively, building up a SAT problem.
         2. Solve the SAT problem. Whenever there are multiple options, try the most preferred one first.
         3. Create the selections XML from the results.

         All three involve recursively walking the tree in a similar way:
         1) we follow every dependency of every implementation (order not important)
         2) we follow every dependency of every selected implementation (better versions first)
         3) we follow every dependency of every selected implementation
      *)
      let opam_files_before = Context.count_expanded_packages context in
      let sat = Sat.create () in
      let* impl_clauses =
        let dummy_impl = if closest_match then Some Input.Dummy else None in
        build_problem
          context
          root_req
          sat
          ~enforce_cross_platform_versions
          ~max_avoids
          ~dummy_impl
      in
      let+ impl_clauses = Fiber.Cache.to_table impl_clauses in
      (* Run the solve *)
      let decider () =
        (* Walk the current solution, depth-first, looking for the first
           undecided interface. Then try the most preferred implementation of
           it that hasn't been ruled out. *)
        let seen = Table.create (module Input.Role) 100 in
        let rec find_undecided req =
          if Table.mem seen req
          then None (* Break cycles *)
          else (
            Table.set seen req true;
            match Table.find_exn impl_clauses req |> Candidates.state with
            | Unselected -> None
            | Undecided lit -> Some lit
            | Selected deps ->
              (* We've already selected a candidate for this component. Now
                 check its dependencies. *)
              List.find_map deps ~f:(fun (dep : Input.dependency) ->
                match dep.importance with
                | Ensure -> find_undecided dep.drole
                | Prevent ->
                  (* Restrictions don't express that we do or don't want the
                     dependency, so skip them here. If someone else needs this,
                     we'll handle it when we get to them.
                     If noone wants it, it will be set to unselected at the end. *)
                  None))
        in
        find_undecided root_req
      in
      let start = Time.now () in
      let result = Sat.run_solver sat decider in
      let stop = Time.now () in
      let num_opam_files = Context.count_expanded_packages context - opam_files_before in
      Dune_trace.emit Sat (fun () ->
        let stats = Sat.get_stats sat in
        Dune_trace.Event.sat_solve
          ~start
          ~dur:(Time.diff stop start)
          ~num_variables:stats.num_variables
          ~num_clauses:stats.num_clauses
          ~num_decisions:stats.num_decisions
          ~num_conflicts:stats.num_conflicts
          ~num_opam_files);
      match result with
      | false -> None
      | true ->
        (* Build the results object *)
        Some
          (Table.to_list impl_clauses
           |> List.filter_map ~f:(fun (key, v) ->
             Candidates.selected v |> Option.map ~f:(fun v -> key, v))
           |> Input.Role.Map.of_list_exn)
    ;;

    let do_solve context ~closest_match root_req =
      let enforce_cross_platform_versions =
        match context.Context.platforms with
        | [] | [ _ ] -> false
        | _ :: _ :: _ -> true
      in
      do_solve
        context
        ~closest_match
        ~enforce_cross_platform_versions
        ~max_avoids:(Some 0)
        root_req
      >>= function
      | Some sels ->
        (* Found a good solution, using no packages flagged as [avoid-version] *)
        Fiber.return (Some sels)
      | None ->
        do_solve
          context
          ~closest_match
          ~enforce_cross_platform_versions
          ~max_avoids:None
          root_req
        >>= (function
         | None ->
           (* No solution even when allowing [avoid-version] *)
           Fiber.return None
         | Some sels ->
           let nb_avoids sels =
             Input.Role.Map.fold sels ~init:[] ~f:(fun sel packages ->
               if Input.Impl.avoid sel.impl
               then (
                 match Input.Impl.version sel.impl with
                 | Some package -> package :: packages
                 | None ->
                   Code_error.raise
                     "avoid-version implementation has no package version"
                     [ ( "implementation"
                       , Dyn.string
                           (Format.asprintf "%a" Pp.to_fmt (Input.Impl.pp sel.impl)) )
                     ])
               else packages)
             |> List.sort_uniq ~compare:(fun a b ->
               Ordering.of_int (OpamPackage.compare a b))
             |> List.length
           in
           let upper = nb_avoids sels in
           (* There exists a solution, using at least 1 and at most [upper]
              packages with the flag [avoid-version]. Attempt to minimize
              their amount by dichotomy between the two bounds. *)
           let rec search lower upper best_sel =
             if lower = upper
             then Fiber.return (Some best_sel)
             else (
               let mid = (lower + upper) / 2 in
               do_solve
                 context
                 ~closest_match
                 ~enforce_cross_platform_versions
                 ~max_avoids:(Some mid)
                 root_req
               >>= function
               | None -> search (mid + 1) upper best_sel
               | Some sels ->
                 let upper = nb_avoids sels in
                 assert (upper <= mid);
                 search lower upper sels)
           in
           search 1 upper sels)
    ;;
  end

  module Diagnostics = struct
    let format_restrictions r =
      String.concat ~sep:", " (List.map ~f:Input.Restriction.to_string r)
    ;;

    module Note = struct
      (** An item of information to display for a component. *)
      type t =
        | UserRequested of Input.Restriction.t
        | Restricts of Input.Role.t * Input.Impl.t * Input.Restriction.t list
        | Feed_problem of string

      let pp = function
        | UserRequested r -> Pp.paragraphf "User requested %s" (format_restrictions [ r ])
        | Restricts (other_role, impl, r) ->
          Pp.hovbox
            ~indent:2
            (Input.Role.pp other_role
             ++ Pp.char ' '
             ++ Input.pp_version impl
             ++ Pp.text " requires "
             ++ Pp.paragraph (format_restrictions r))
        | Feed_problem msg -> Pp.text msg
      ;;
    end

    (** Represents a single interface in the example (failed) selections produced by the solver.
        It partitions the implementations into good and bad based (initially) on the split from the
        impl_provider. As we explore the example selections, we further filter the candidates. *)
    module Component = struct
      type rejection_reason =
        [ `Model_rejection of Context.rejection
        | `FailsRestriction of Input.Restriction.t
        | `DepFailsRestriction of Input.dependency * Input.Restriction.t
        | `ClassConflict of Input.Role.t * OpamPackage.Name.t
        | `ConflictsRole of Input.Role.t
        | `Cross_platform_version_conflict of Input.Role.t * Input.impl
        | `DiagnosticsFailure of User_message.Style.t Pp.t
        ]
      (* Why a particular implementation was rejected. This could be because the model rejected it,
         or because it conflicts with something else in the example (partial) solution. *)

      type reject = Input.impl * rejection_reason

      type t =
        { role : Input.Role.t
        ; diagnostics : User_message.Style.t Pp.t Lazy.t
        ; selected_impl : Input.impl option
        ; (* orig_good is all the implementations passed to the SAT solver (these are the
             ones with a compatible OS, CPU, etc). They are sorted most desirable first. *)
          orig_good : Input.impl list
        ; orig_bad : (Input.impl * Context.rejection) list
        ; mutable good : Input.impl list
        ; mutable bad : (Input.impl * rejection_reason) list
        ; mutable notes : Note.t list
        }

      (* Initialise a new component.
         @param candidates is the result from the impl_provider.
         @param selected_impl
           is the selected implementation, or [None] if we chose [dummy_impl].
         @param diagnostics can be used to produce diagnostics as a last resort. *)
      let create
            ~role
            (candidates, orig_bad, feed_problems)
            (diagnostics : _ Pp.t Lazy.t)
            (selected_impl : Input.impl option)
        =
        let notes = List.map ~f:(fun x -> Note.Feed_problem x) feed_problems in
        { role
        ; orig_good = candidates
        ; orig_bad
        ; good = candidates
        ; bad = List.map ~f:(fun (impl, reason) -> impl, `Model_rejection reason) orig_bad
        ; notes
        ; diagnostics
        ; selected_impl
        }
      ;;

      let note t note = t.notes <- note :: t.notes
      let notes t = List.rev t.notes

      (* Did rejecting [impl] make any difference?
         If [t] selected a better version anyway then we don't need to report this rejection. *)
      let affected_selection t impl =
        match t.selected_impl with
        (* CR-someday rgrinberg: take account version preference here? *)
        | Some selected when Input.Impl.compare_version selected impl = Gt -> false
        | _ -> true
      ;;

      (* Call [get_problem impl] on each good impl. If a problem is returned, move [impl] to [bad_impls].
         If anything changes and [!note] is not None, report it and clear the pending note. *)
      let filter_impls_ref ~note:n t get_problem =
        let old_good = List.rev t.good in
        t.good <- [];
        List.iter old_good ~f:(fun impl ->
          match get_problem impl with
          | None -> t.good <- impl :: t.good
          | Some problem ->
            Option.iter !n ~f:(fun info ->
              if affected_selection t impl
              then (
                note t info;
                n := None));
            t.bad <- (impl, problem) :: t.bad)
      ;;

      let filter_impls ?note t get_problem =
        let note = ref note in
        filter_impls_ref ~note t get_problem
      ;;

      (* Remove from [good_impls] anything that fails to meet these restrictions.
         Add removed items to [bad_impls], along with the cause. *)
      let apply_restrictions ~note t restrictions =
        let note = ref (Some note) in
        List.iter restrictions ~f:(fun r ->
          filter_impls_ref ~note t (fun impl ->
            if Input.meets_restriction impl r then None else Some (`FailsRestriction r)))
      ;;

      let apply_user_restriction t r =
        note t (UserRequested r);
        (* User restrictions should be applied before reaching the solver, but just in case: *)
        filter_impls t (fun impl ->
          if Input.meets_restriction impl r then None else Some (`FailsRestriction r));
        (* Completely remove non-matching impls.
           The user will only want to see the version they asked for. *)
        let new_bad =
          List.filter t.bad ~f:(fun (impl, _) ->
            if Input.meets_restriction impl r then true else false)
        in
        if new_bad <> [] || t.good <> [] then t.bad <- new_bad
      ;;

      let reject_all t reason =
        t.bad <- List.map ~f:(fun impl -> impl, reason) t.good @ t.bad;
        t.good <- []
      ;;

      let selected_impl t = t.selected_impl

      (* When something conflicts with itself then our usual trick of selecting
         the main implementation and failing the dependency doesn't work, so
         special-case that here. *)
      let reject_self_conflicts t =
        filter_impls t (fun impl ->
          let deps = Input.Impl.requires t.role impl in
          List.find_map deps ~f:(fun (dep : Input.dependency) ->
            match Input.Role.compare dep.drole t.role with
            | Lt | Gt -> None
            | Eq ->
              (* It depends on itself. *)
              List.find_map dep.restrictions ~f:(fun r ->
                if Input.meets_restriction impl r
                then None
                else Some (`DepFailsRestriction (dep, r)))))
      ;;

      let finalise t =
        if t.selected_impl = None
        then (
          reject_self_conflicts t;
          reject_all t (`DiagnosticsFailure (Lazy.force t.diagnostics)))
      ;;

      let pp_reject ((_impl, reason) : reject) =
        match reason with
        | `Model_rejection r -> Context.pp_rejection r
        | `FailsRestriction r ->
          Pp.paragraphf
            "Incompatible with restriction: %s"
            (Input.Restriction.to_string r)
        | `DepFailsRestriction (dep, restriction) ->
          Pp.hovbox
            (Pp.text "Requires "
             ++ Input.Role.pp dep.drole
             ++ Pp.textf " %s" (format_restrictions [ restriction ]))
        | `ClassConflict (other_role, cl) ->
          Pp.hovbox
            (Pp.textf "In same conflict class (%s) as " (OpamPackage.Name.to_string cl)
             ++ Input.Role.pp other_role)
        | `ConflictsRole other_role ->
          Pp.hovbox (Pp.text "Conflicts with " ++ Input.Role.pp other_role)
        | `Cross_platform_version_conflict (other_role, other_impl) ->
          let platform =
            match Input.Role.platform other_role with
            | None ->
              Code_error.raise "Virtual role has a cross-platform version conflict" []
            | Some platform -> platform
          in
          Pp.hovbox
            (Pp.text "Version differs from "
             ++ Input.pp_impl_long other_impl
             ++ Pp.text " selected on "
             ++ Solver_env.pp_oneline platform)
        | `DiagnosticsFailure msg ->
          Pp.hovbox (Pp.text "Reason for rejection unknown: " ++ msg)
      ;;

      let show_rejections ~verbose rejected =
        let by_version (a, _) (b, _) = Input.Impl.compare_version b a in
        let rejected = List.sort ~compare:by_version rejected in
        let rec aux i = function
          | [] -> Pp.nop
          | _ when i = 5 && not verbose -> Pp.cut ++ Pp.text "..."
          | (impl, problem) :: xs ->
            Pp.cut
            ++ Pp.hovbox
                 ~indent:2
                 (Input.pp_impl_long impl ++ Pp.text ": " ++ pp_reject (impl, problem))
            ++ aux (i + 1) xs
        in
        aux 0 rejected
      ;;

      let rejects t =
        let summary =
          if t.orig_good = []
          then if t.orig_bad = [] then `No_candidates else `All_unusable
          else `Conflicts
        in
        t.bad, summary
      ;;

      let pp_candidates ~verbose t =
        if t.selected_impl = None
        then
          Pp.cut
          ++
          match rejects t with
          | _, `No_candidates -> Pp.paragraph "No known implementations at all"
          | bad, `All_unusable ->
            Pp.vbox
              ~indent:2
              (Pp.paragraph "No usable implementations:" ++ show_rejections ~verbose bad)
          | bad, `Conflicts ->
            Pp.vbox
              ~indent:2
              (Pp.paragraph "Rejected candidates:" ++ show_rejections ~verbose bad)
        else Pp.nop
      ;;

      let pp_notes t =
        match notes t with
        | [] -> Pp.nop
        | notes -> Pp.cut ++ Pp.concat_map ~sep:Pp.cut notes ~f:Note.pp
      ;;

      let pp_outcome t =
        match t.selected_impl with
        | Some sel -> Input.pp_impl_long sel
        | None -> Pp.text "(problem)"
      ;;

      (* Format a textual description of this component's report. *)
      let pp ~verbose ~platform t =
        let platform_suffix =
          match platform with
          | None -> Pp.nop
          | Some platform -> Pp.text " on " ++ Solver_env.pp_oneline platform
        in
        Pp.vbox
          ~indent:2
          (Pp.hovbox
             (Input.pp_role t.role ++ Pp.text " -> " ++ pp_outcome t ++ platform_suffix)
           ++ pp_notes t
           ++ pp_candidates ~verbose t)
      ;;
    end

    (* Did any dependency of [impl] prevent it being selected?
       This can only happen if a component conflicts with something more
       important than itself (otherwise, we'd select something in [impl]'s
       interface and complain about the dependency instead).

       e.g. A depends on B and C. B and C both depend on D. C1 conflicts with
       D1. The depth-first priority order means we give priority to {A1, B1,
       D1}. Then we can't choose C1 because we prefer to keep D1. *)
    let get_dependency_problem role (report : Component.t Input.Role.Map.t) impl =
      Input.Impl.requires role impl
      |> List.find_map ~f:(fun (dep : Input.dependency) ->
        match Input.Role.Map.find report dep.drole with
        | None -> None (* Not in the selections => can't be part of a conflict *)
        | Some required_component ->
          (match Component.selected_impl required_component with
           | None -> None (* Dummy selection can't cause a conflict *)
           | Some dep_impl ->
             List.find_map dep.restrictions ~f:(fun r ->
               if Input.meets_restriction dep_impl r
               then None
               else Some (`DepFailsRestriction (dep, r)))))
    ;;

    (** A selected component has [dep] as a dependency. Use this to explain why some implementations
        of the required interface were rejected. *)
    let examine_dep
          requiring_role
          requiring_impl
          (report : Component.t Input.Role.Map.t)
          (dep : Input.dependency)
      =
      match Input.Role.Map.find report dep.drole with
      | None -> ()
      | Some required_component ->
        if dep.restrictions <> []
        then
          (* Remove implementations incompatible with the other selections *)
          Component.apply_restrictions
            required_component
            dep.restrictions
            ~note:(Restricts (requiring_role, requiring_impl, dep.restrictions))
    ;;

    (* Find all restrictions that are in play and affect this interface *)
    let examine_selection report role component =
      match Component.selected_impl component with
      | Some our_impl ->
        (* For each dependency of our selected impl, explain why it rejected
           impls in the dependency's interface. *)
        Input.Impl.requires role our_impl
        |> List.iter ~f:(examine_dep role our_impl report)
      | None ->
        (* For each of our remaining unrejected impls, check whether a
           dependency prevented its selection. *)
        get_dependency_problem role report |> Component.filter_impls component
    ;;

    (* Check for user-supplied restrictions *)
    let examine_extra_restrictions report context =
      Input.Role.Map.iteri report ~f:(fun role component ->
        Input.Role.user_restrictions role context
        |> Option.iter ~f:(Component.apply_user_restriction component))
    ;;

    (* Check if two roles refer to the same package (ignoring the platform).
       Used for conflict class checking - same package on different platforms
       should not be considered in conflict with itself. *)
    let same_package_name (role1 : Input.Role.t) (role2 : Input.Role.t) =
      match role1, role2 with
      | Real (name1, _), Real (name2, _) -> OpamPackage.Name.equal name1 name2
      | Virtual (id1, _), Virtual (id2, _) -> Input.Virtual_id.equal id1 id2
      | Real _, Virtual _ | Virtual _, Real _ -> false
    ;;

    (** For each selected implementation with a conflict class, reject all candidates
        with the same class on the same platform. *)
    let check_conflict_classes report =
      let classes =
        Input.Role.Map.foldi
          report
          ~init:OpamPackage.Name.Map.empty
          ~f:(fun role component acc ->
            match Component.selected_impl component, Input.Role.platform role with
            | None, _ | _, None -> acc
            | Some impl, Some platform ->
              Input.Impl.conflict_class impl
              |> List.fold_left ~init:acc ~f:(fun acc conflict_class ->
                let platforms =
                  OpamPackage.Name.Map.find_opt conflict_class acc
                  |> Option.value ~default:Solver_env.Map.empty
                in
                OpamPackage.Name.Map.add
                  conflict_class
                  (Solver_env.Map.set platforms platform role)
                  acc))
      in
      Input.Role.Map.iteri report ~f:(fun role component ->
        match Input.Role.platform role with
        | None -> ()
        | Some platform ->
          Component.filter_impls component (fun impl ->
            Input.Impl.conflict_class impl
            |> List.find_map ~f:(fun conflict_class ->
              OpamPackage.Name.Map.find_opt conflict_class classes
              |> Option.bind ~f:(fun platforms ->
                match Solver_env.Map.find platforms platform with
                | Some other_role when not (same_package_name role other_role) ->
                  Some (`ClassConflict (other_role, conflict_class))
                | _ -> None))))
    ;;

    (** Reject candidates whose version differs from the version selected for
        the same package on another platform. *)
    let check_cross_platform_versions report =
      let selected_by_package =
        Input.Role.Map.foldi
          report
          ~init:OpamPackage.Name.Map.empty
          ~f:(fun role component acc ->
            match role, Component.selected_impl component with
            | Input.Real (name, _), Some impl ->
              let selected =
                OpamPackage.Name.Map.find_opt name acc |> Option.value ~default:[]
              in
              OpamPackage.Name.Map.add name ((role, impl) :: selected) acc
            | (Input.Real _ | Input.Virtual _), None | Input.Virtual _, Some _ -> acc)
      in
      Input.Role.Map.iteri report ~f:(fun role component ->
        match role with
        | Input.Virtual _ -> ()
        | Input.Real (name, _) ->
          let selected =
            OpamPackage.Name.Map.find_opt name selected_by_package
            |> Option.value ~default:[]
          in
          Component.filter_impls component (fun impl ->
            List.find_map selected ~f:(fun (other_role, other_impl) ->
              if
                Input.Role.compare role other_role <> Eq
                && Input.Impl.compare_version impl other_impl <> Eq
              then Some (`Cross_platform_version_conflict (other_role, other_impl))
              else None)))
    ;;

    let of_result context impls =
      let explain role =
        match Input.Role.Map.find impls role with
        | Some (sel : Solver.selection) -> Solver.Sat.explain_reason sel.var
        | None -> Pp.text "Role not used!"
      in
      let+ report =
        let get_selected role (sel : Solver.selection) =
          let diagnostics = lazy (explain role) in
          let impl = if sel.impl = Input.Dummy then None else Some sel.impl in
          (* CR-someday rgrinberg: Are we recomputing things here? *)
          let* impl_candidates = Input.implementations role context in
          let+ rejects, feed_problems = Input.Role.rejects role context in
          Component.create
            ~role
            (impl_candidates, rejects, feed_problems)
            diagnostics
            impl
        in
        Input.Role.Map.to_list impls
        |> Fiber.parallel_map ~f:(fun (k, v) ->
          let+ v = get_selected k v in
          k, v)
        |> Fiber.map ~f:Input.Role.Map.of_list_exn
      in
      examine_extra_restrictions report context;
      check_conflict_classes report;
      check_cross_platform_versions report;
      Input.Role.Map.iteri ~f:(examine_selection report) report;
      Input.Role.Map.iteri ~f:(fun _ c -> Component.finalise c) report;
      report
    ;;
  end

  let solve context pkgs =
    let platforms = context.Context.platforms in
    let req =
      match pkgs, platforms with
      | [ pkg ], [ platform ] ->
        (* Single package, single platform - use Real directly *)
        Input.Real (pkg, platform)
      | _ ->
        (* Multiple packages or platforms - create virtual root *)
        let depends =
          List.concat_map platforms ~f:(fun platform ->
            List.map pkgs ~f:(fun name ->
              { Input.drole = Real (name, platform)
              ; importance = Ensure
              ; restrictions = []
              }))
        in
        let impl : Input.Impl.t = VirtualImpl (Input.Rank.bottom, depends) in
        Input.virtual_role [ impl ]
    in
    Solver.do_solve context ~closest_match:false req
    >>| function
    | Some sels -> Ok sels
    | None -> Error req
  ;;

  let role_name = function
    | Input.Virtual _ -> None
    | Input.Real (name, _) -> Some name
  ;;

  let deduplicate_roles_by_name = filter_dedup_by_name ~key:role_name

  (* Deduplicate impls by package name, keeping one representative per package.
     For VirtualImpls, skip them if all their deps point to packages we've already seen. *)
  let deduplicate_impls_by_name impls =
    let seen = ref OpamPackage.Name.Set.empty in
    List.filter impls ~f:(fun impl ->
      match impl with
      | Input.VirtualImpl (_, deps) ->
        (* Check if this VirtualImpl has any deps we haven't seen yet *)
        let has_new_deps =
          List.exists deps ~f:(fun (d : Input.dependency) ->
            match d.drole with
            | Input.Virtual _ -> true
            | Input.Real (name, _) -> not (OpamPackage.Name.Set.mem name !seen))
        in
        if has_new_deps
        then (
          (* Add all dep names to seen *)
          List.iter deps ~f:(fun (d : Input.dependency) ->
            match d.drole with
            | Input.Virtual _ -> ()
            | Input.Real (name, _) -> seen := OpamPackage.Name.Set.add name !seen);
          true)
        else false
      | _ ->
        (match Input.Impl.version impl with
         | None -> true
         | Some pkg ->
           let name = OpamPackage.name pkg in
           if OpamPackage.Name.Set.mem name !seen
           then false
           else (
             seen := OpamPackage.Name.Set.add name !seen;
             true)))
  ;;

  let pp_rolemap ~verbose ~total_platforms reasons =
    let good, bad, unknown =
      Input.Role.Map.to_list reasons
      |> List.partition_three ~f:(fun (role, component) ->
        match Diagnostics.Component.selected_impl component with
        | Some impl when Diagnostics.Component.notes component = [] -> `Left impl
        | _ ->
          (match Diagnostics.Component.rejects component with
           | _, `No_candidates -> `Right role
           | _, _ -> `Middle component))
    in
    let good = deduplicate_impls_by_name good in
    let good_names =
      List.fold_left good ~init:OpamPackage.Name.Set.empty ~f:(fun names impl ->
        match Input.Impl.version impl with
        | None -> names
        | Some pkg -> OpamPackage.Name.Set.add (OpamPackage.name pkg) names)
    in
    let component_key component =
      Format.asprintf
        "%a"
        Pp.to_fmt
        (Diagnostics.Component.pp ~verbose ~platform:None component)
    in
    let bad_with_keys =
      List.map bad ~f:(fun component -> component, component_key component)
    in
    let bad_summaries =
      List.fold_left
        bad_with_keys
        ~init:OpamPackage.Name.Map.empty
        ~f:(fun summaries (component, key) ->
          match (component : Diagnostics.Component.t).role with
          | Input.Virtual _ -> summaries
          | Input.Real (name, _) ->
            let summary =
              match OpamPackage.Name.Map.find_opt name summaries with
              | None -> 1, key, false
              | Some (count, first_key, varied) ->
                count + 1, first_key, varied || not (String.equal first_key key)
            in
            OpamPackage.Name.Map.add name summary summaries)
    in
    let show_platform_for name =
      let count, _, varied =
        OpamPackage.Name.Map.find_opt name bad_summaries |> Option.value_exn
      in
      count < total_platforms || varied || OpamPackage.Name.Set.mem name good_names
    in
    let seen_names = ref OpamPackage.Name.Set.empty in
    let seen_virtual_keys = ref String.Set.empty in
    let bad =
      List.filter_map bad_with_keys ~f:(fun (component, key) ->
        match (component : Diagnostics.Component.t).role with
        | Input.Virtual _ ->
          if String.Set.mem !seen_virtual_keys key
          then None
          else (
            seen_virtual_keys := String.Set.add !seen_virtual_keys key;
            Some component)
        | Input.Real (name, _) ->
          if show_platform_for name
          then Some component
          else if OpamPackage.Name.Set.mem name !seen_names
          then None
          else (
            seen_names := OpamPackage.Name.Set.add name !seen_names;
            Some component))
    in
    let unknown = deduplicate_roles_by_name unknown in
    let pp_bad (component : Diagnostics.Component.t) =
      let platform =
        match component.role with
        | Input.Virtual _ -> None
        | Input.Real (name, platform) -> Option.some_if (show_platform_for name) platform
      in
      Diagnostics.Component.pp ~verbose ~platform component
    in
    let known =
      Pp.paragraph "Selected candidates: "
      ++ Pp.hovbox (Pp.concat_map ~sep:Pp.space good ~f:Input.pp_impl)
      ++ Pp.cut
      ++ Pp.enumerate bad ~f:pp_bad
    in
    match unknown with
    | [] -> known
    | _ ->
      Pp.hovbox
        (Pp.text "The following packages couldn't be found: "
         ++ Pp.concat_map ~sep:Pp.space unknown ~f:(fun role ->
           Pp.box (Input.Role.pp role)))
  ;;

  let diagnostics_rolemap context req =
    Solver.do_solve context req ~closest_match:true
    >>| Option.value_exn
    >>= Diagnostics.of_result context
  ;;

  let diagnostics ?(verbose = false) context solve_error =
    let+ diag = diagnostics_rolemap context solve_error in
    let total_platforms = List.length context.Context.platforms in
    Pp.paragraph "Couldn't solve the package dependency formula."
    ++ Pp.cut
    ++ Pp.vbox (pp_rolemap ~verbose ~total_platforms diag)
  ;;

  (* Extract one package per name and the selected package versions for each
     platform. Checking version equality here keeps the SAT invariant close to
     the solver result that establishes it. *)
  let packages_by_platform ~platforms sels =
    let packages_by_platform =
      List.fold_left platforms ~init:Solver_env.Map.empty ~f:(fun acc platform ->
        Solver_env.Map.set acc platform Package_name.Map.empty)
    in
    Input.Role.Map.foldi
      sels
      ~init:(OpamPackage.Name.Map.empty, packages_by_platform)
      ~f:(fun role (sel : Solver.selection) (packages, packages_by_platform) ->
        match role with
        | Input.Virtual _ -> packages, packages_by_platform
        | Input.Real (opam_name, platform) ->
          (match Input.Impl.version sel.impl with
           | None -> packages, packages_by_platform
           | Some package ->
             let name = Package_name.of_opam_package_name opam_name in
             let version =
               OpamPackage.version package |> Package_version.of_opam_package_version
             in
             let packages =
               match OpamPackage.Name.Map.find_opt opam_name packages with
               | None -> OpamPackage.Name.Map.add opam_name package packages
               | Some existing ->
                 let existing_version =
                   OpamPackage.version existing |> Package_version.of_opam_package_version
                 in
                 if not (Package_version.equal version existing_version)
                 then
                   Code_error.raise
                     "Cross-platform version equality SAT constraint failed: solver \
                      selected multiple versions of the same package"
                     [ "name", Package_name.to_dyn name
                     ; "version_a", Package_version.to_dyn existing_version
                     ; "version_b", Package_version.to_dyn version
                     ];
                 packages
             in
             let platform_packages =
               Solver_env.Map.find packages_by_platform platform |> Option.value_exn
             in
             ( packages
             , Solver_env.Map.set
                 packages_by_platform
                 platform
                 (Package_name.Map.set platform_packages name version) )))
    |> fun (packages, packages_by_platform) ->
    OpamPackage.Name.Map.bindings packages |> List.map ~f:snd, packages_by_platform
  ;;
end

let solve_package_list packages ~context =
  Fiber.collect_errors (fun () ->
    (* [Solver.solve] returns [Error] when it's unable to find a solution to
       the dependencies, but can also raise exceptions, for example if opam
       is unable to parse an opam file in the package repository. To prevent
       an unexpected opam exception from crashing dune, we catch all
       exceptions raised by the solver and report them as [User_error]s
       instead. *)
    Solver.solve context packages)
  >>| (function
   | Ok (Ok res) -> Ok res
   | Ok (Error e) -> Error (`Diagnostics e)
   | Error [] -> assert false
   | Error (exn :: _) ->
     (* CR-rgrinberg: this needs to be handled right *)
     Error (`Exn exn))
  >>= function
  | Ok sels ->
    let packages, packages_by_platform =
      Solver.packages_by_platform ~platforms:context.Context.platforms sels
    in
    Fiber.return @@ Ok (packages, packages_by_platform)
  | Error (`Diagnostics e) ->
    let+ diagnostics = Solver.diagnostics context e in
    Error (`Solve_error diagnostics)
  | Error (`Exn exn) ->
    (match exn.exn with
     | OpamPp.(Bad_format _ | Bad_format_list _ | Bad_version _) as bad_format ->
       (* CR-rgrinberg: needs to include locations *)
       Fiber.return
       @@ Error
            (`Manifest_error
                (User_error.make [ Pp.text (OpamPp.string_of_bad_format bad_format) ]))
     | User_error.E message -> Fiber.return @@ Error (`Manifest_error message)
     | _ ->
       Code_error.raise
         "Unexpected exception raised while solving dependencies"
         [ "exception", Exn_with_backtrace.to_dyn exn ])
;;

module Solver_result = struct
  type t =
    { lock_dir : Lock_dir.t
    ; files : File_entry.t Package_version.Map.Multi.t Package_name.Map.t
    ; pinned_packages : Package_name.Set.t
    ; num_expanded_packages : int
    }
end

let reject_unreachable_packages =
  let reachable deps_of_package ~roots =
    let seen = ref Package_name.Set.empty in
    let rec loop = function
      | [] -> ()
      | pkg :: rest ->
        if Package_name.Set.mem !seen pkg
        then loop rest
        else (
          seen := Package_name.Set.add !seen pkg;
          let deps =
            match Package_name.Map.find deps_of_package pkg with
            | None -> []
            | Some deps -> deps
          in
          loop (List.rev_append deps rest))
    in
    loop roots;
    !seen
  in
  let merge_packages pkgs_by_name local_packages ~lock_dir ~local =
    Package_name.Map.merge pkgs_by_name local_packages ~f:(fun name lhs rhs ->
      match lhs, rhs with
      | None, None -> assert false
      | Some _, Some _ ->
        Code_error.raise
          "package is both local and returned by solver"
          [ "name", Package_name.to_dyn name ]
      | Some pkg, None -> Some (lock_dir pkg)
      | None, Some pkg -> Some (local pkg))
  in
  fun solver_env ~dune_version ~local_packages ~pkgs_by_name ->
    let roots = Package_name.Map.keys local_packages in
    let pkgs_by_version =
      merge_packages
        pkgs_by_name
        local_packages
        ~lock_dir:(fun (pkg : Lock_dir.Pkg.t) -> pkg.info.version)
        ~local:(fun (pkg : Local_package.For_solver.t) -> pkg.version)
    in
    let pkgs_by_name =
      merge_packages
        pkgs_by_name
        local_packages
        ~lock_dir:(fun (pkg : Lock_dir.Pkg.t) ->
          Lock_dir.Conditional_choice.choose_for_platform pkg.depends ~platform:solver_env
          |> Option.value ~default:[]
          |> List.map ~f:(fun (depend : Lock_dir.Dependency.t) -> depend.name))
        ~local:(fun (pkg : Local_package.For_solver.t) ->
          let deps =
            match
              Lock_pkg.local_package_dependencies
                pkg
                ~env:(Solver_env.to_env solver_env)
                ~with_test:(with_test solver_env)
                ~packages:pkgs_by_version
                ~dune_version
            with
            | Ok regular -> regular
            | Error (`Formula_could_not_be_satisfied hints) ->
              (match hints with
               | [ (Unsatisfied_version_constraint { package_name; _ } as hint) ]
                 when Package_name.equal package_name Dune_dep.name ->
                 (* The current version of dune was injected into the set of
                    available packages right before the dependencies were
                    computed, so there's a possibility that it will violate
                    the project's dependency constraints. A user can trigger
                    this error case by placing version constraints on their
                    project's dependency on the "dune" package which are not
                    satisfied by the current version of Dune. *)
                 User_error.raise
                   [ Pp.text
                       "The current version of Dune does not satisfy the version \
                        constraints for Dune in this project's dependencies."
                   ; Pp.text "Details:"
                   ; Resolve_opam_formula.Unsatisfied_formula_hint.pp hint
                   ]
               | _ ->
                 (* This case is a code error because the set of packages
                    being searched has already been produced by the solver.
                    It should be guaranteed that all packages in the set are
                    mutually compatible and that the set is closed under
                    dependency relationships (with the possible exception of
                    the Dune package itself, which is handled by a separate
                    case). *)
                 Code_error.raise
                   "can't find a valid solution for the dependencies"
                   [ "name", Package_name.to_dyn pkg.name
                   ; ( "hints"
                     , Dyn.list Resolve_opam_formula.Unsatisfied_formula_hint.to_dyn hints
                     )
                   ])
          in
          let depopts =
            List.filter_map pkg.depopts ~f:(fun (d : Package_dependency.t) ->
              Option.some_if (Package_name.Map.mem pkgs_by_version d.name) d.name)
          in
          deps @ depopts)
    in
    reachable pkgs_by_name ~roots
;;

let files resolved_packages =
  let+ files = Resolved_package.get_opam_package_files resolved_packages in
  let open Result.O in
  let+ files = files in
  List.map2 resolved_packages files ~f:(fun resolved_package entries ->
    let package = Resolved_package.package resolved_package in
    package, entries)
  |> List.filter_map ~f:(fun (package, entries) ->
    if List.is_empty entries
    then None
    else (
      let package_name = OpamPackage.name package |> Package_name.of_opam_package_name in
      let package_version =
        OpamPackage.version package |> Package_version.of_opam_package_version
      in
      Some (package_name, Package_version.Map.singleton package_version entries)))
  |> Package_name.Map.of_list_exn
;;

let package_kind =
  (* Heuristic to determine whether a package is an ocaml compiler *)
  let opam_file_is_compiler (opam_package : OpamFile.OPAM.t) =
    (* Identify compiler packages by using the fact that all compiler
      Packages declare conflicts with other compiler packages. note
      that relying on the "compiler" flag to identify compiler packages
      will not work, as compiler options packages (such as
      ocaml-option-flambda) also have this flag. *)
    let ocaml_core_compiler = OpamPackage.Name.of_string "ocaml-core-compiler" in
    List.mem opam_package.conflict_class ocaml_core_compiler ~equal:OpamPackage.Name.equal
  in
  fun package ->
    if opam_file_is_compiler (Resolved_package.opam_file package)
    then `Compiler
    else `Non_compiler
;;

let resolve_url (file_url : OpamFile.URL.t) =
  let url = OpamFile.URL.url file_url in
  let+ url : OpamUrl.t =
    match url.backend with
    | `git ->
      let loc = Loc.none in
      let+ mount = Mount.of_opam_url loc url in
      (match Mount.backend mount with
       | Path path ->
         Code_error.raise
           "Attempted to resolve git URL but loading it resolved to a local path"
           [ "url", OpamUrl.to_dyn url; "path", Path.to_dyn path ]
       | Git at_rev ->
         let resolved_hash = at_rev |> Rev_store.At_rev.rev |> Rev_store.Object.to_hex in
         (match !Clflags.display, url.hash with
          | Short, Some old_hash ->
            (match String.equal resolved_hash old_hash with
             | true -> ()
             | false ->
               User_warning.emit
                 [ Pp.textf
                     "Locking repository URL %s to commit %s"
                     (OpamUrl.to_string url)
                     resolved_hash
                 ])
          | _ -> ());
         { url with hash = Some resolved_hash })
    | _ -> Fiber.return url
  in
  OpamFile.URL.with_url url file_url
;;

let resolve_opam_packages opam_packages_to_lock candidates_cache =
  let resolve_package name version =
    let entry : Context.candidates = Table.find_exn candidates_cache name in
    OpamPackage.Version.Map.find version entry.resolved
  in
  Fiber.parallel_map opam_packages_to_lock ~f:(fun opam_package ->
    let name = OpamPackage.name opam_package |> Package_name.of_opam_package_name in
    let version = OpamPackage.version opam_package in
    let resolved_package = resolve_package name version in
    (* resolve URLs *)
    let+ resolved_package =
      let opam_file = Resolved_package.opam_file resolved_package in
      let+ opam_file =
        match OpamFile.OPAM.url opam_file with
        | None -> Fiber.return opam_file
        | Some url ->
          let+ url = resolve_url url in
          OpamFile.OPAM.with_url url opam_file
      in
      Resolved_package.with_opam_file opam_file resolved_package
    in
    name, opam_package, resolved_package)
;;

let deduplicate_platform_overlays platform_overlays =
  List.rev
    (List.fold_left platform_overlays ~init:[] ~f:(fun acc overlay ->
       if List.exists acc ~f:(Solver_env.equal overlay) then acc else overlay :: acc))
;;

(* Split [solver_env] into a portable base env (with platform-specific
   variables unset) and the distinct platform overlays to solve for. *)
let base_solver_env_and_platforms solver_env ~solve_for_platforms ~portable_lock_dir =
  if portable_lock_dir
  then
    ( Solver_env.unset_multi solver_env Dune_lang.Package_variable_name.platform_specific
    , deduplicate_platform_overlays solve_for_platforms )
  else solver_env, [ Solver_env.empty ]
;;

let solve_lock_dir
      solver_env
      ~platform_overlays
      version_preference
      repos
      ~local_packages
      ~pins:pinned_packages
      ~constraints
      ~selected_depopts
      ~portable_lock_dir
  =
  match platform_overlays with
  | [] -> Code_error.raise "solve_lock_dir called with empty platform_overlays" []
  | _ ->
    (match Package_name.Map.add pinned_packages Dune_dep.name Resolved_package.dune with
     | Error p ->
       let loc = Resolved_package.loc p in
       let message =
         User_error.make
           ~loc
           [ Pp.text
               "Dune cannot be pinned. The currently running version is the only one \
                that may be used"
           ]
       in
       Fiber.return (Error (`Manifest_error message))
     | Ok pinned_packages ->
       let pinned_package_names = Package_name.Set.of_keys pinned_packages in
       let stats_updater = Solver_stats.Updater.init () in
       (* The platform envs themselves identify the platforms: every role and
          every per-platform selection is keyed by the platform's own
          (platform-specific) env. *)
       let platforms = platform_overlays in
       let platform_envs =
         List.map platform_overlays ~f:(fun platform ->
           platform, Solver_env.extend solver_env platform)
       in
       let full_solver_envs = List.map platform_envs ~f:snd in
       let context =
         let local_packages' =
           lazy
             (Package_name.Map.map local_packages ~f:(fun local ->
                let opam_file = Local_package.For_solver.to_opam_file local in
                let version =
                  Option.value
                    opam_file.version
                    ~default:Context.local_package_default_version
                in
                let depends = OpamFile.OPAM.depends opam_file in
                let conflicts = OpamFile.OPAM.conflicts opam_file in
                { Context.opam_file
                ; version
                ; depends
                ; conflicts
                ; name = local.name
                ; filtered_formulas_by_platform = Table.create (module Solver_env) 1
                }))
         in
         Context.create
           ~pinned_packages
           ~solver_env
           ~platforms
           ~repos
           ~version_preference
           ~local_packages:local_packages'
           ~stats_updater
           ~constraints
       in
       Package_name.Map.keys local_packages @ selected_depopts
       |> List.map ~f:Package_name.to_opam_package_name
       |> solve_package_list ~context
       >>= (function
        | Error _ as e -> Fiber.return e
        | Ok (solution, packages_by_platform) ->
          (* The full solver envs and selected package versions for the
             platforms on which a package is selected. *)
          let solver_envs_for_package name =
            List.filter_map platform_envs ~f:(fun (platform, full_solver_env) ->
              let packages =
                Solver_env.Map.find packages_by_platform platform |> Option.value_exn
              in
              Option.some_if
                (Package_name.Map.mem packages name)
                (full_solver_env, packages))
          in
          let is_dune name = Package_name.equal Dune_dep.name name in
          (* Don't include local packages or dune in the lock dir. *)
          let opam_packages_to_lock =
            let is_local_package = Package_name.Map.mem local_packages in
            List.filter solution ~f:(fun package ->
              let name = OpamPackage.name package |> Package_name.of_opam_package_name in
              (not (is_local_package name)) && not (is_dune name))
          in
          let* candidates_cache = Fiber.Cache.to_table context.candidates_cache in
          let resolve_package name version =
            (Table.find_exn candidates_cache name).resolved
            |> OpamPackage.Version.Map.find version
          in
          let* pkgs_by_name =
            let+ package_branches =
              let+ resolved_pkgs =
                resolve_opam_packages opam_packages_to_lock candidates_cache
              in
              (* Evaluate each package separately on every platform where the
                 solver selected it. Reachability is computed before these
                 branches are merged. *)
              List.map resolved_pkgs ~f:(fun (name, opam_package, resolved_package) ->
                let package_solver_envs = solver_envs_for_package name in
                Lock_pkg.opam_package_to_lock_file_pkg_branches
                  package_solver_envs
                  stats_updater
                  opam_package
                  ~pinned:(Package_name.Set.mem pinned_package_names name)
                  resolved_package
                  ~portable_lock_dir
                |> Result.map ~f:(fun branches -> name, branches))
              |> Result.List.all
            in
            Result.map package_branches ~f:(fun package_branches ->
              let branches_by_platform =
                List.fold_left
                  platform_envs
                  ~init:Solver_env.Map.empty
                  ~f:(fun acc (_, solver_env) ->
                    Solver_env.Map.set acc solver_env Package_name.Map.empty)
              in
              let branches_by_platform =
                List.fold_left
                  package_branches
                  ~init:branches_by_platform
                  ~f:(fun branches_by_platform (name, branches) ->
                    List.fold_left
                      branches
                      ~init:branches_by_platform
                      ~f:(fun branches_by_platform (solver_env, package) ->
                        let packages =
                          Solver_env.Map.find branches_by_platform solver_env
                          |> Option.value_exn
                        in
                        if Package_name.Map.mem packages name
                        then
                          Code_error.raise
                            "Solver selected multiple versions for the same package"
                            [ "name", Package_name.to_dyn name ];
                        Solver_env.Map.set
                          branches_by_platform
                          solver_env
                          (Package_name.Map.set packages name package)))
              in
              (* Drop unreachable platform branches before merging package
                 metadata. In particular, this prevents a dependency pruned on
                 one platform from surviving because the package is reachable
                 on another platform. *)
              List.fold_left
                platform_envs
                ~init:Package_name.Map.empty
                ~f:(fun merged_packages (_, solver_env) ->
                  let packages =
                    Solver_env.Map.find branches_by_platform solver_env
                    |> Option.value_exn
                  in
                  let reachable =
                    reject_unreachable_packages
                      solver_env
                      ~dune_version:
                        (Package_version.of_opam_package_version context.dune_version)
                      ~local_packages
                      ~pkgs_by_name:packages
                  in
                  Package_name.Map.foldi
                    packages
                    ~init:merged_packages
                    ~f:(fun name package merged_packages ->
                      if not (Package_name.Set.mem reachable name)
                      then merged_packages
                      else
                        Package_name.Map.update merged_packages name ~f:(function
                          | None -> Some package
                          | Some previous ->
                            Some (Lock_dir.Pkg.merge_conditionals previous package)))))
          in
          let ocaml =
            let open Result.O in
            let* pkgs_by_name = pkgs_by_name in
            (* This doesn't allow the compiler to live in the source tree. Oh
         well, it's not possible now anyway. *)
            match
              Package_name.Map.filter_map pkgs_by_name ~f:(fun (pkg : Lock_dir.Pkg.t) ->
                match
                  let version =
                    Package_version.to_opam_package_version pkg.info.version
                  in
                  resolve_package pkg.info.name version |> package_kind
                with
                | `Compiler -> Some pkg.info.name
                | `Non_compiler -> None)
              |> Package_name.Map.values
            with
            | [] -> Ok None
            | [ x ] -> Ok (Some (Loc.none, x))
            | _ ->
              Error
                (User_error.make
                   (* CR-someday rgrinberg: needs to include locations *)
                   [ Pp.text "multiple compilers selected" ]
                   ~hints:[ Pp.text "add a conflict" ])
          in
          let lock_dir =
            let open Result.O in
            let* pkgs_by_name = pkgs_by_name
            and* ocaml = ocaml in
            let+ () =
              Package_name.Map.values pkgs_by_name
              |> Result.List.map
                   ~f:(fun { Lock_dir.Pkg.depends; info = { name; _ }; _ } ->
                     (* A repository package must not evade validation by
                        depending on a workspace package only on a
                        non-primary platform, so validate the dependency
                        choices for every platform where the package is
                        selected. *)
                     let platform_envs = solver_envs_for_package name in
                     Result.List.map platform_envs ~f:(fun (platform_env, _) ->
                       match
                         Lock_dir.Conditional_choice.choose_for_platform
                           depends
                           ~platform:platform_env
                       with
                       | None -> Ok ()
                       | Some depends ->
                         Result.List.map
                           depends
                           ~f:(fun { Lock_dir.Dependency.name = dep_name; loc } ->
                             match
                               (not (is_dune dep_name))
                               && Package_name.Map.mem local_packages dep_name
                             with
                             | false -> Ok ()
                             | true ->
                               Error
                                 (User_error.make
                                    ~loc
                                    [ Pp.textf
                                        "Dune does not support packages outside the \
                                         workspace depending on packages in the \
                                         workspace. The package %S is not in the \
                                         workspace but it depends on the package %S \
                                         which is in the workspace."
                                        (Package_name.to_string name)
                                        (Package_name.to_string dep_name)
                                    ]))
                         |> Result.map ~f:(fun (_ : unit list) -> ()))
                     |> Result.map ~f:(fun (_ : unit list) -> ()))
              |> Result.map ~f:(fun (_ : unit list) -> ())
            in
            let expanded_solver_variable_bindings =
              let stats = Solver_stats.Updater.snapshot stats_updater in
              Solver_stats.Expanded_variable_bindings.of_variable_set
                stats.expanded_variables
                solver_env
            in
            Lock_dir.create_latest_version
              pkgs_by_name
              ~local_packages:(Package_name.Map.values local_packages)
              ~ocaml
              ~repos:(Some repos)
              ~expanded_solver_variable_bindings
              ~solved_for_platforms:full_solver_envs
              ~portable_lock_dir
          in
          let+ files =
            match pkgs_by_name with
            | Error e -> Fiber.return (Error e)
            | Ok pkgs_by_name ->
              let+ files =
                Package_name.Map.to_list_map
                  pkgs_by_name
                  ~f:(fun name (package : Lock_dir.Pkg.t) ->
                    Package_version.to_opam_package_version package.info.version
                    |> resolve_package name)
                |> files
              in
              files
          in
          (match Result.both lock_dir files with
           | Error e -> Error (`Manifest_error e)
           | Ok (lock_dir, files) ->
             Ok
               { Solver_result.lock_dir
               ; files
               ; pinned_packages = pinned_package_names
               ; num_expanded_packages = Context.count_expanded_packages context
               })))
;;
