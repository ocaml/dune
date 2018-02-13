open Import

module Preds : sig
  type t

  val make : string list -> t
  val count : t -> int
  val is_subset : t -> subset:t -> bool
  val intersects : t -> t -> bool
end = struct
  type t = string list

  let make l = List.sort l ~cmp:String.compare

  let count = List.length

  let rec is_subset t ~subset =
    match t, subset with
    | _, [] -> true
    | [], _ :: _ -> false
    | x1 :: l1, x2 :: l2 ->
      let d = String.compare x1 x2 in
      if d = 0 then
        is_subset l1 ~subset:l2
      else if d < 0 then
        is_subset l1 ~subset
      else
        false

  let rec intersects a b =
    match a, b with
    | [], _ | _, [] -> false
    | x1 :: l1, x2 :: l2 ->
      let d = String.compare x1 x2 in
      if d = 0 then
        true
      else if d < 0 then
        intersects l1 b
      else
        intersects a l2
end

(* An assignment or addition *)
module Rule = struct
  type t =
    { preds_required  : Preds.t
    ; preds_forbidden : Preds.t
    ; value           : string
    }

  let formal_predicates_count t =
    Preds.count t.preds_required + Preds.count t.preds_forbidden

  let matches t ~preds =
    Preds.is_subset preds ~subset:t.preds_required &&
    not (Preds.intersects preds t.preds_forbidden)


  let make (rule : Meta.rule) =
    let preds_required, preds_forbidden =
      List.partition_map rule.predicates ~f:(function
        | Pos x -> Inl x
        | Neg x -> Inr x)
    in
    { preds_required  = Preds.make preds_required
    ; preds_forbidden = Preds.make preds_forbidden
    ; value           = rule.value
    }
end

(* Set of rules for a given variable of a package *)
module Rules = struct
  (* To implement the algorithm described in [1], [set_rules] is sorted by decreasing
     number of formal predicates, then according to the order of the META
     file. [add_rules] are in the same order as in the META file.

     [1] http://projects.camlcity.org/projects/dl/findlib-1.6.3/doc/ref-html/r729.html *)
  type t =
    { set_rules : Rule.t list
    ; add_rules : Rule.t list
    }

  let interpret t ~preds =
    let rec find_set_rule = function
      | [] -> ""
      | rule :: rules ->
        if Rule.matches rule ~preds then
          rule.value
        else
          find_set_rule rules
    in
    let v = find_set_rule t.set_rules in
    List.fold_left t.add_rules ~init:v ~f:(fun v rule ->
      if Rule.matches rule ~preds then
        v ^ " " ^ rule.value
      else
        v)

  let of_meta_rules (rules : Meta.Simplified.Rules.t) =
    let add_rules = List.map rules.add_rules ~f:Rule.make in
    let set_rules =
      List.map rules.set_rules ~f:Rule.make
      |> List.stable_sort ~cmp:(fun a b ->
        compare (Rule.formal_predicates_count b) (Rule.formal_predicates_count a))
    in
    { add_rules; set_rules }
end

module Vars = struct
  type t = Rules.t String_map.t

  let get (t : t) var preds =
    let preds = Preds.make preds in
    match String_map.find var t with
    | None -> ""
    | Some rules -> Rules.interpret rules ~preds

  let get_words t var preds = String.extract_comma_space_separated_words (get t var preds)
end

module Config = struct
  type t =
    { vars  : Vars.t
    ; preds : string list
    }

  let load path ~toolchain ~context =
    let path = Path.extend_basename path ~suffix:".d" in
    let conf_file = Path.relative path (toolchain ^ ".conf") in
    if not (Path.exists conf_file) then
      die "@{<error>Error@}: ocamlfind toolchain %s isn't defined in %a \
           (context: %s)" toolchain Path.pp path context;
    let vars =
      (Meta.simplify { name = ""
                     ; entries = Meta.load (Path.to_string conf_file)
                     }).vars
    in
    { vars = String_map.map vars ~f:Rules.of_meta_rules; preds = [toolchain] }

  let get { vars; preds } var =
    Vars.get vars var preds
end

module Package_not_available = struct
  type t =
    { package     : string
    ; required_by : With_required_by.Entry.t list
    ; reason      : reason
    }

  and reason =
    | Not_found
    | Hidden

  let explain ppf reason =
    match reason with
    | Not_found ->
      Format.fprintf ppf "not found"
    | Hidden ->
      Format.fprintf ppf "hidden (unsatisfied 'exist_if')"
end

module External_dep_conflicts_with_local_lib = struct
  type t =
    { package             : string
    ; required_by         : With_required_by.Entry.t
    ; required_locally_in : With_required_by.Entry.t list
    ; defined_locally_in  : Path.t
    }
end

module Dependency_cycle = struct
  type t =
    { cycle       : string list
    ; required_by : With_required_by.Entry.t list
    }
end

type error =
  | Package_not_available
    of Package_not_available.t
  | External_dep_conflicts_with_local_lib
    of External_dep_conflicts_with_local_lib.t
  | Dependency_cycle
    of Dependency_cycle.t

exception Findlib of error

type t =
  { stdlib_dir    : Path.t
  ; path          : Path.t list
  ; builtins      : Meta.t String_map.t
  ; packages      : (string, package or_not_available) Hashtbl.t
  (* Cache the result of [closure]. A key is the list of package
     unique identifiers. *)
  ; closure_cache : (int list, (package list, error) result) Hashtbl.t
  }

and package =
  { name             : string
  ; unique_id        : int
  ; dir              : Path.t
  ; version          : string
  ; description      : string
  ; archives         : Path.t list Mode.Dict.t
  ; plugins          : Path.t list Mode.Dict.t
  ; jsoo_runtime     : string list
  ; requires         : package list or_not_available Lazy.t
  ; ppx_runtime_deps : package list or_not_available Lazy.t
  ; db               : t
  }

and 'a or_not_available = ('a, Package_not_available.t) result

let path t = t.path

let create ~stdlib_dir ~path =
  { stdlib_dir
  ; path
  ; builtins      = Meta.builtins ~stdlib_dir
  ; packages      = Hashtbl.create 1024
  ; closure_cache = Hashtbl.create 1024
  }

let root_package_name s =
  match String.index s '.' with
  | None -> s
  | Some i -> String.sub s ~pos:0 ~len:i

let gen_package_unique_id =
  let next = ref 0 in
  fun () ->
    let n = !next in
    next := n + 1;
    n

(* Parse a single package from a META file *)
let rec parse_package t ~name ~parent_dir ~vars =
  let pkg_dir = Vars.get vars "directory" [] in
  let dir =
    if pkg_dir = "" then
      parent_dir
    else if pkg_dir.[0] = '+' || pkg_dir.[0] = '^' then
      Path.relative t.stdlib_dir
        (String.sub pkg_dir ~pos:1 ~len:(String.length pkg_dir - 1))
    else if Filename.is_relative pkg_dir then
      Path.relative parent_dir pkg_dir
    else
      Path.absolute pkg_dir
  in
  let archives var preds =
    Mode.Dict.of_func (fun ~mode ->
      List.map (Vars.get_words vars var (Mode.findlib_predicate mode :: preds))
        ~f:(Path.relative dir))
  in
  let exists_if = Vars.get_words vars "exists_if" [] in
  let exists =
    List.for_all exists_if ~f:(fun fn ->
      Path.exists (Path.relative dir fn))
  in
  (dir,
   if exists then
     let jsoo_runtime = Vars.get_words vars "jsoo_runtime" [] in
     let preds = ["ppx_driver"; "mt"; "mt_posix"] in
     let requires         = Vars.get_words vars "requires"         preds in
     let ppx_runtime_deps = Vars.get_words vars "ppx_runtime_deps" preds in
     Ok
      { name
      ; dir
      ; unique_id   = gen_package_unique_id ()
      ; version     = Vars.get vars "version" []
      ; description = Vars.get vars "description" []
      ; archives    = archives "archive" preds
      ; jsoo_runtime
      ; plugins     = Mode.Dict.map2 ~f:(@)
                        (archives "archive" ("plugin" :: preds))
                        (archives "plugin" preds)
      ; requires         = lazy (resolve_deps t requires)
      ; ppx_runtime_deps = lazy (resolve_deps t ppx_runtime_deps)
      ; db = t
      }
  else
    Error
      { Package_not_available.
        package     = name
      ; reason      = Hidden
      ; required_by = []
      })

(* Parse all the packages defined in a META file and add them to
   [t.packages] *)
and parse_and_acknowledge_meta t ~dir (meta : Meta.t) =
  let rec loop ~dir ~full_name (meta : Meta.Simplified.t) =
    let vars = String_map.map meta.vars ~f:Rules.of_meta_rules in
    let dir, pkg = parse_package t ~name:full_name ~parent_dir:dir ~vars in
    Hashtbl.add t.packages ~key:full_name ~data:pkg;
    List.iter meta.subs ~f:(fun (meta : Meta.Simplified.t) ->
      loop ~dir ~full_name:(sprintf "%s.%s" full_name meta.name) meta)
  in
  loop ~dir ~full_name:meta.name (Meta.simplify meta)

(* Search for a <package>/META file in the findlib search path, parse
   it and add its contents to [t.packages] *)
and find_and_acknowledge_meta t ~fq_name =
  let root_name = root_package_name fq_name in
  let rec loop dirs : (Path.t * Meta.t) option =
    match dirs with
    | dir :: dirs ->
      let sub_dir = Path.relative dir root_name in
      let fn = Path.relative sub_dir "META" in
      if Path.exists fn then
        Some (sub_dir,
              { name    = root_name
              ; entries = Meta.load (Path.to_string fn)
              })
      else
        (* Alternative layout *)
        let fn = Path.relative dir ("META." ^ root_name) in
        if Path.exists fn then
          Some (dir,
                { name    = root_name
                ; entries = Meta.load (Path.to_string fn)
                })
        else
          loop dirs
    | [] ->
      match String_map.find root_name t.builtins with
      | Some meta -> Some (t.stdlib_dir, meta)
      | None -> None
  in
  match loop t.path with
  | None ->
    Hashtbl.add t.packages ~key:root_name
      ~data:(Error { package     = root_name
                   ; reason      = Not_found
                   ; required_by = []
                   })
  | Some (dir, meta) -> parse_and_acknowledge_meta t meta ~dir

and find_internal t name =
  match Hashtbl.find t.packages name with
  | Some x -> x
  | None ->
    find_and_acknowledge_meta t ~fq_name:name;
    match Hashtbl.find t.packages name with
    | Some x -> x
    | None ->
      let res : _ or_not_available =
        Error
          { package     = name
          ; required_by = []
          ; reason      = Not_found
          }
      in
      Hashtbl.add t.packages ~key:name ~data:res;
      res

and resolve_deps t names =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | name :: names ->
      match find_internal t name with
      | Ok x -> loop (x :: acc) names
      | Error _ as e -> e
  in
  loop [] names

let find t ~required_by name =
  match find_internal t name with
  | Ok _ as res -> res
  | Error (na : Package_not_available.t) ->
    Error { na with required_by }

let find_exn t ~required_by name =
  match find t ~required_by name with
  | Ok    x -> x
  | Error e -> raise (Findlib (Package_not_available e))

let available t name =
  match find_internal t name with
  | Ok    _ -> true
  | Error _ -> false

module Package = struct
  type t = package

  let name        t = t.name
  let dir         t = t.dir
  let version     t = t.version
  let description t = t.description

  let archives t mode = Mode.Dict.get t.archives mode
  let plugins  t mode = Mode.Dict.get t.plugins  mode

  let jsoo_runtime t = t.jsoo_runtime

  let deps_exn (deps : _ or_not_available Lazy.t) ~required_by =
    match Lazy.force deps with
    | Ok x -> x
    | Error na ->
      raise (Findlib (Package_not_available { na with required_by }))

  let requires         t ~required_by = deps_exn t.requires         ~required_by
  let ppx_runtime_deps t ~required_by = deps_exn t.ppx_runtime_deps ~required_by
end

module Closure =
  Top_closure.Make
    (String)
    (struct
      type graph = unit
      type t = Package.t * With_required_by.Entry.t list
      let key (pkg, _) = pkg.name
      let deps (pkg, required_by) () =
        let required_by =
          With_required_by.Entry.Library pkg.name :: required_by
        in
        List.map (Package.requires pkg ~required_by)
          ~f:(fun x -> (x, required_by))
    end)

let check_deps_consistency ~required_by ~local_public_libs deps =
  List.iter deps ~f:(fun pkg' ->
    match String_map.find pkg'.name local_public_libs with
    | None -> ()
    | Some path ->
      raise (Findlib (External_dep_conflicts_with_local_lib
                        { package             = pkg'.name
                        ; required_by         = Library "TODO" (*pkg.name*)
                        ; required_locally_in = required_by
                        ; defined_locally_in  = path
                        })))

let extend_error_stack e ~required_by =
  match e with
  | Package_not_available x ->
    Package_not_available
      { x with required_by = x.required_by @ required_by }
  | External_dep_conflicts_with_local_lib x ->
    External_dep_conflicts_with_local_lib
      { x with required_locally_in = x.required_locally_in @ required_by }
  | Dependency_cycle x ->
    Dependency_cycle
      { x with required_by = x.required_by @ required_by }

let closure pkgs ~required_by ~local_public_libs =
  match pkgs with
  | [] -> []
  | first :: others ->
    let t = first.db in
    let key =
      first.unique_id :: List.map others ~f:(fun p ->
        assert (p.db == t);
        p.unique_id)
    in
    match
      Hashtbl.find_or_add t.closure_cache key ~f:(fun _ ->
        let pkgs = List.map pkgs ~f:(fun p -> (p, [])) in
        match Closure.top_closure () pkgs with
        | Ok pkgs -> Ok (List.map pkgs ~f:fst)
        | Error cycle ->
          Error
            (Dependency_cycle
               { cycle       = List.map cycle ~f:(fun (p, _) -> p.name)
               ; required_by = []
               })
        | exception (Findlib e) -> Error e)
    with
    | Ok pkgs ->
      check_deps_consistency pkgs ~required_by ~local_public_libs;
      pkgs
    | Error e -> raise (Findlib (extend_error_stack e ~required_by))

let closed_ppx_runtime_deps_of pkgs ~required_by ~local_public_libs =
  closure pkgs ~required_by ~local_public_libs
  |> List.concat_map ~f:(Package.ppx_runtime_deps ~required_by)
  |> closure ~required_by ~local_public_libs

let root_packages t =
  let pkgs =
    List.concat_map t.path ~f:(fun dir ->
      Sys.readdir (Path.to_string dir)
      |> Array.to_list
      |> List.filter ~f:(fun name ->
        Path.exists (Path.relative dir (name ^ "/META"))))
    |> String_set.of_list
  in
  let pkgs =
    String_set.union pkgs
      (String_set.of_list (String_map.keys t.builtins))
  in
  String_set.elements pkgs

let load_all_packages t =
  List.iter (root_packages t) ~f:(fun pkg ->
    find_and_acknowledge_meta t ~fq_name:pkg)

let all_packages t =
  load_all_packages t;
  Hashtbl.fold t.packages ~init:[] ~f:(fun ~key:_ ~data acc ->
    match data with
    | Ok    p -> p :: acc
    | Error _ -> acc)
  |> List.sort ~cmp:(fun a b -> String.compare a.name b.name)

let all_unavailable_packages t =
  load_all_packages t;
  Hashtbl.fold t.packages ~init:[] ~f:(fun ~key:_ ~data acc ->
    match data with
    | Ok    _ -> acc
    | Error n -> n :: acc)
  |> List.sort ~cmp:(fun a b ->
    String.compare a.Package_not_available.package b.package)

let stdlib_with_archives t =
  let x = find_exn t ~required_by:[] "stdlib" in
  let archives =
    { Mode.Dict.byte   = Path.relative x.dir "stdlib.cma"  :: x.archives.byte
    ; Mode.Dict.native = Path.relative x.dir "stdlib.cmxa" :: x.archives.native
    }
  in
  { x with archives }
