open Import

type t =
  { roots : Package_name.t list
  ; forward : Package_name.t list Package_name.Map.t
  ; reverse : Package_name.t list Package_name.Map.t
  ; opam_packages : OpamPackage.t Package_name.Map.t
  }

let roots t = t.roots

let dependencies t package_name =
  Package_name.Map.find t.forward package_name |> Option.value ~default:[]
;;

let dependents t package_name =
  Package_name.Map.find t.reverse package_name |> Option.value ~default:[]
;;

let opam_package t package_name = Package_name.Map.find_exn t.opam_packages package_name
let name_repr = Repr.abstract Package_name.to_dyn

let name_map_repr value_repr =
  Repr.view (Repr.list (Repr.pair name_repr value_repr)) ~to_:Package_name.Map.to_list
;;

let repr =
  Repr.record
    "dependency-graph"
    [ Repr.field "roots" (Repr.list name_repr) ~get:(fun t -> t.roots)
    ; Repr.field "forward" (name_map_repr (Repr.list name_repr)) ~get:(fun t -> t.forward)
    ; Repr.field "reverse" (name_map_repr (Repr.list name_repr)) ~get:(fun t -> t.reverse)
    ; Repr.field
        "packages"
        (name_map_repr (Repr.abstract Opam_dyn.package))
        ~get:(fun t -> t.opam_packages)
    ]
;;

let to_dyn = Repr.to_dyn repr

(* Local packages are not present in the lockdir, so their dependencies must be
   resolved through the package universe. Locked package dependencies can be read
   directly from the lockdir because they have already been solved for this
   platform. *)
let immediate_dependencies
      package_universe
      ~local_packages
      ~platform_pkgs
      ~platform
      package_name
  =
  let deps =
    if Package_name.Map.mem local_packages package_name
    then
      Package_universe.opam_package_dependencies_of_package
        package_universe
        package_name
        ~which:`All
        ~traverse:`Immediate
      |> List.map ~f:(fun opam_package ->
        Package_name.of_opam_package_name (OpamPackage.name opam_package))
    else (
      match Package_name.Map.find platform_pkgs package_name with
      | None -> []
      | Some (pkg : Lock_dir.Pkg.t) ->
        Lock_dir.Conditional_choice.choose_for_platform pkg.depends ~platform
        |> Option.value ~default:[]
        |> List.filter_map ~f:(fun (dep : Lock_dir.Dependency.t) ->
          Option.some_if
            (Package_name.Map.mem platform_pkgs dep.name
             || Package_name.Map.mem local_packages dep.name)
            dep.name))
  in
  List.sort deps ~compare:Package_name.compare
;;

let create package_universe =
  let local_packages = Package_universe.local_packages package_universe in
  let platform = Package_universe.platform package_universe in
  let lock_dir = Package_universe.lock_dir package_universe in
  let platform_pkgs =
    Lock_dir.Packages.pkgs_on_platform_by_name lock_dir.packages ~platform
  in
  let roots = Package_name.Map.keys local_packages in
  (* Expand each reachable package once so graph construction shares work across
     repeated dependencies and terminates on local dependency cycles. *)
  let rec collect (forward, reverse, opam_packages, seen) package_name =
    if Package_name.Set.mem seen package_name
    then forward, reverse, opam_packages, seen
    else (
      let seen = Package_name.Set.add seen package_name in
      let deps =
        immediate_dependencies
          package_universe
          ~local_packages
          ~platform_pkgs
          ~platform
          package_name
      in
      let forward = Package_name.Map.set forward package_name deps in
      let opam_packages =
        Package_name.Map.set
          opam_packages
          package_name
          (Package_universe.opam_package_of_package package_universe package_name)
      in
      let reverse =
        List.fold_left deps ~init:reverse ~f:(fun reverse dep ->
          Package_name.Map.add_multi reverse dep package_name)
      in
      List.fold_left deps ~init:(forward, reverse, opam_packages, seen) ~f:collect)
  in
  let forward, reverse, opam_packages, _seen =
    List.fold_left
      roots
      ~init:
        ( Package_name.Map.empty
        , Package_name.Map.empty
        , Package_name.Map.empty
        , Package_name.Set.empty )
      ~f:collect
  in
  { roots; forward; reverse; opam_packages }
;;

(* A cycle makes the fully-expanded dependency tree infinite, so occurrence
   counts are intentionally omitted for packages reached through a cycle. *)
let occurrences t =
  let root_set = Package_name.Set.of_list t.roots in
  let cache = ref Package_name.Map.empty in
  let rec occurrences ~visiting package_name =
    match Package_name.Map.find !cache package_name with
    | Some n -> Some n
    | None ->
      if Package_name.Set.mem visiting package_name
      then None
      else (
        let visiting = Package_name.Set.add visiting package_name in
        let from_roots = if Package_name.Set.mem root_set package_name then 1 else 0 in
        let n =
          dependents t package_name
          |> List.fold_left ~init:(Some from_roots) ~f:(fun acc parent ->
            let open Option.O in
            let* acc = acc in
            let+ n = occurrences ~visiting parent in
            acc + n)
        in
        Option.iter n ~f:(fun n -> cache := Package_name.Map.set !cache package_name n);
        n)
  in
  fun package_name -> occurrences ~visiting:Package_name.Set.empty package_name
;;

(* [visited] is shared across roots so repeated packages collapse to a leaf.
   [visiting] is path-local so only edges closing a real cycle are marked. *)
let pp_tree t ~adjacency start =
  let occurrences = occurrences t in
  let label ~cyclic package_name =
    let name = OpamPackage.to_string (opam_package t package_name) in
    if cyclic
    then sprintf "%s (cycle)" name
    else (
      match occurrences package_name with
      | Some n when n > 1 -> sprintf "%s (*%d)" name n
      | Some _ | None -> name)
  in
  let rec node ~visiting visited package_name =
    if Package_name.Set.mem visiting package_name
    then visited, Pp.text (label ~cyclic:true package_name)
    else if Package_name.Set.mem visited package_name
    then visited, Pp.text (label ~cyclic:false package_name)
    else (
      let visited = Package_name.Set.add visited package_name in
      let visiting = Package_name.Set.add visiting package_name in
      let children =
        Package_name.Map.find adjacency package_name |> Option.value ~default:[]
      in
      match children with
      | [] -> visited, Pp.text (label ~cyclic:false package_name)
      | _ :: _ ->
        let visited, children =
          List.fold_map children ~init:visited ~f:(node ~visiting)
        in
        ( visited
        , Pp.vbox
            (Pp.concat
               ~sep:Pp.cut
               [ Pp.hbox (Pp.text (label ~cyclic:false package_name))
               ; Pp.enumerate children ~f:Fun.id |> Pp.box
               ]) ))
  in
  List.fold_map
    start
    ~init:Package_name.Set.empty
    ~f:(node ~visiting:Package_name.Set.empty)
  |> snd
  |> Pp.enumerate ~f:Fun.id
  |> Pp.box
;;

let pp_deps_tree t = pp_tree t ~adjacency:t.forward t.roots
let pp_why t package_name = pp_tree t ~adjacency:t.reverse [ package_name ]
