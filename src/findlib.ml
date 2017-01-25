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
  (* To implement the algorithm described in [1], [set_rules] is sorted by number of format
     predicates, then according to the order of the META file. [add_rules] are in the same
     order as in the META file.

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
        compare (Rule.formal_predicates_count a) (Rule.formal_predicates_count b))
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

  let get_words t var preds = String.split_words (get t var preds)
end

type package =
  { name             : string
  ; dir              : Path.t
  ; version          : string
  ; description      : string
  ; archives         : string list Mode.Dict.t
  ; plugins          : string list Mode.Dict.t
  ; requires         : package list
  ; ppx_runtime_deps : package list
  ; has_headers      : bool
  }

type present_or_absent =
  | Present of package
  | Absent

type t =
  { context     : Context.t
  ; packages    : (string, present_or_absent) Hashtbl.t
  ; has_headers : (Path.t, bool             ) Hashtbl.t
  }

let has_headers t ~dir =
  match Hashtbl.find t.has_headers dir with
  | Some x -> x
  | None ->
    let x = List.exists (Path.readdir dir) ~f:(fun fn -> Filename.check_suffix fn ".h") in
    Hashtbl.add t.has_headers ~key:dir ~data:x;
    x

let context t = t.context

let create context =
  { context
  ; packages    = Hashtbl.create 1024
  ; has_headers = Hashtbl.create 1024
  }

module Pkg_step1 = struct
  type t =
    { package          : package
    ; requires         : string list
    ; ppx_runtime_deps : string list
    ; exists           : bool
    }
end

let parse_package t ~name ~parent_dir ~vars =
  let pkg_dir = Vars.get vars "directory" [] in
  let dir =
    if pkg_dir = "" then
      parent_dir
    else if pkg_dir.[0] = '+' || pkg_dir.[0] = '^' then
      Path.relative t.context.stdlib_dir
        (String.sub pkg_dir ~pos:1 ~len:(String.length pkg_dir - 1))
    else if Filename.is_relative pkg_dir then
      Path.relative parent_dir pkg_dir
    else
      Path.absolute pkg_dir
  in
  let archives var preds =
    Mode.Dict.of_func (fun ~mode ->
      Vars.get_words vars var (Mode.findlib_predicate mode :: preds))
  in
  let preds = ["ppx_driver"; "mt"; "mt_posix"] in
  let pkg =
    { name
    ; dir
    ; has_headers = has_headers t ~dir
    ; version     = Vars.get vars "version" []
    ; description = Vars.get vars "description" []
    ; archives    = archives "archive" preds
    ; plugins     = Mode.Dict.map2 ~f:(@)
                      (archives "archive" ("plugin" :: preds))
                      (archives "plugin" preds)
    ; requires    = []
    ; ppx_runtime_deps = []
    }
  in
  let exists_if = Vars.get_words vars "exists_if" [] in
  let exists =
    List.for_all exists_if ~f:(fun fn ->
      Path.exists (Path.relative dir fn))
  in
  { Pkg_step1.
    package          = pkg
  ; requires         = Vars.get_words vars "requires"         preds
  ; ppx_runtime_deps = Vars.get_words vars "ppx_runtime_deps" preds
  ; exists           = exists
  }

let parse_meta t ~dir (meta : Meta.t) =
  let rec loop ~dir ~full_name ~acc (meta : Meta.Simplified.t) =
    let vars = String_map.map meta.vars ~f:Rules.of_meta_rules in
    let pkg = parse_package t ~name:full_name ~parent_dir:dir ~vars in
    let dir = pkg.package.dir in
    List.fold_left meta.subs ~init:(pkg :: acc) ~f:(fun acc (meta : Meta.Simplified.t) ->
      loop ~dir ~full_name:(sprintf "%s.%s" full_name meta.name) ~acc meta)
  in
  loop ~dir ~full_name:meta.name (Meta.simplify meta) ~acc:[]

exception Package_not_found of string

let root_package_name s =
  match String.index s '.' with
  | None -> s
  | Some i -> String.sub s ~pos:0 ~len:i

let rec load_meta_rec t root_name ~packages =
  if String_map.mem root_name packages then
    packages
  else
    let rec loop dirs : Path.t * Meta.t =
      match dirs with
      | dir :: dirs ->
        let dir = Path.relative dir root_name in
        let fn = Path.relative dir "META" in
        if Path.exists fn then
          (dir,
           { name    = root_name
           ; entries = Meta.load (Path.to_string fn)
           })
        else
          loop dirs
      | [] ->
        match String_map.find root_name Meta.builtins with
        | Some meta -> (t.context.stdlib_dir, meta)
        | None -> raise (Package_not_found root_name)
    in
    let dir, meta = loop t.context.findlib_path in
    let new_packages = parse_meta t ~dir meta in
    let packages =
      List.fold_left new_packages ~init:packages ~f:(fun acc (pkg : Pkg_step1.t) ->
        String_map.add acc ~key:pkg.package.name ~data:pkg)
    in
    let deps =
      List.fold_left new_packages ~init:String_set.empty
        ~f:(fun acc (pkg : Pkg_step1.t) ->
          if pkg.exists then
            let add_roots acc deps =
              List.fold_left deps ~init:acc ~f:(fun acc dep ->
                String_set.add (root_package_name dep) acc)
            in
            add_roots (add_roots acc pkg.requires) pkg.ppx_runtime_deps
          else
            acc)
    in
    String_set.fold deps ~init:packages ~f:(fun name packages ->
      load_meta_rec t name ~packages)

module Local_closure =
  Top_closure.Make
    (String)
    (struct
      type graph = Pkg_step1.t String_map.t
      type t = Pkg_step1.t
      let key (t : t) = t.package.name
      let deps (t : t) packages =
        List.filter_map t.requires ~f:(fun name ->
          String_map.find name packages) @
        List.filter_map t.ppx_runtime_deps ~f:(fun name ->
          String_map.find name packages)
    end)

let remove_dups_preserve_order pkgs =
  let rec loop seen pkgs acc =
    match pkgs with
    | [] -> List.rev acc
    | pkg :: pkgs ->
      if String_set.mem pkg.name seen then
        loop seen pkgs acc
      else
        loop (String_set.add pkg.name seen) pkgs (pkg :: acc)
  in
  loop String_set.empty pkgs []
;;

let load_meta t root_name =
  let packages = load_meta_rec t root_name ~packages:String_map.empty in
  match Local_closure.top_closure packages (String_map.values packages) with
  | Error cycle ->
    die "dependency cycle detected between external findlib packages:\n   %s"
      (List.map cycle ~f:(fun (pkg : Pkg_step1.t) -> pkg.package.name)
       |> String.concat ~sep:"\n-> ")
  | Ok ordering ->
    List.iter ordering ~f:(fun (pkg : Pkg_step1.t) ->
      if not pkg.exists then begin
        if !Clflags.debug_findlib then
          Printf.eprintf "findlib: package %S is hidden\n"
            pkg.package.name
      end else begin
        let resolve_deps deps missing_deps_acc =
          let deps, missing_deps =
            List.partition_map deps ~f:(fun name ->
              match Hashtbl.find t.packages name with
              | Some (Present pkg) -> Inl pkg
              | None | Some Absent ->
                match String_map.find name packages with
                | None -> Inr (name, None)
                | Some pkg ->
                  Inr (name, Some pkg))
          in
          (deps, missing_deps @ missing_deps_acc)
        in
        let requires, missing_deps = resolve_deps pkg.requires [] in
        let ppx_runtime_deps, missing_deps =
          resolve_deps pkg.ppx_runtime_deps missing_deps
        in
        match missing_deps with
        | [] ->
          let requires =
            remove_dups_preserve_order
              (List.concat_map requires ~f:(fun pkg -> pkg.requires) @ requires)
          in
          let ppx_runtime_deps =
            remove_dups_preserve_order
              (List.concat
                 [ List.concat_map ppx_runtime_deps ~f:(fun pkg -> pkg.requires)
                 ; ppx_runtime_deps
                 ; List.concat_map requires ~f:(fun pkg -> pkg.ppx_runtime_deps)
                 ])
          in
          let pkg =
            { pkg.package with
              requires
            ; ppx_runtime_deps
            }
          in
          Hashtbl.add t.packages ~key:pkg.name ~data:(Present pkg)
        | _ ->
          let unknown_deps, hidden_deps =
            List.partition_map missing_deps ~f:(fun (name, pkg) ->
              match pkg with
              | None -> Inl name
              | Some pkg -> Inr pkg)
          in
          match unknown_deps with
          | name :: _ -> raise (Package_not_found name)
          | [] ->
            (* We can be in this case for ctypes.foreign for instance *)
            if !Clflags.debug_findlib then
              Printf.eprintf "findlib: skipping %S has it has hidden dependencies: %s\n"
                pkg.package.name
                (String.concat ~sep:", "
                   (List.map hidden_deps
                      ~f:(fun (pkg : Pkg_step1.t) -> pkg.package.name)));
            assert (List.for_all hidden_deps
                      ~f:(fun (pkg : Pkg_step1.t) -> not pkg.exists))
      end
    )

let find_exn t name =
  match Hashtbl.find t.packages name with
  | Some (Present x) -> x
  | Some Absent -> raise (Package_not_found name)
  | None ->
    match load_meta t (root_package_name name) with
    | exception (Package_not_found _ as e) ->
      Hashtbl.add t.packages ~key:name ~data:Absent;
      raise e
    | () ->
      match Hashtbl.find t.packages name with
      | Some (Present x) -> x
      | Some Absent      -> raise (Package_not_found name)
      | None             -> assert false

let available t name =
  match find_exn t name with
  | _ -> true
  | exception (Package_not_found _) -> false

let closure pkgs =
  remove_dups_preserve_order
    (List.concat_map pkgs ~f:(fun pkg -> pkg.requires)
     @ pkgs)

let closed_ppx_runtime_deps_of pkgs =
  remove_dups_preserve_order
    (List.concat_map pkgs ~f:(fun pkg -> pkg.ppx_runtime_deps))

let root_packages t =
  let pkgs =
    List.concat_map t.context.findlib_path ~f:(fun dir ->
      Sys.readdir (Path.to_string dir)
      |> Array.to_list
      |> List.filter ~f:(fun name ->
        Path.exists (Path.relative dir (name ^ "/META"))))
    |> String_set.of_list
  in
  let pkgs =
    String_set.union pkgs
      (String_set.of_list (String_map.keys Meta.builtins))
  in
  String_set.elements pkgs

let all_packages t =
  List.iter (root_packages t) ~f:(fun pkg ->
    ignore (find_exn t pkg : package));
  Hashtbl.fold t.packages ~init:[] ~f:(fun ~key:pkg ~data acc ->
    match data with
    | Present _ -> pkg :: acc
    | Absent    -> acc)
  |> List.sort ~cmp:String.compare
