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

type package =
  { name             : string
  ; dir              : Path.t
  ; version          : string
  ; description      : string
  ; archives         : Path.t list Mode.Dict.t
  ; plugins          : Path.t list Mode.Dict.t
  ; jsoo_runtime     : string list
  ; requires         : package list
  ; ppx_runtime_deps : package list
  }

module Package_not_available = struct
  type t =
    { package     : string
    ; required_by : With_required_by.Entry.t list
    ; reason      : reason
    }

  and reason =
    | Not_found
    | Hidden
    | Dependencies_unavailable of t list

  module Closure =
    Top_closure.Make
      (String)
      (struct
        type graph = unit
        type nonrec t = t
        let key t = t.package
        let deps t () =
          match t.reason with
          | Not_found | Hidden -> []
          | Dependencies_unavailable l -> l
      end)

  let all_names ts =
    let rec loop acc ts =
      List.fold_left ts ~init:acc ~f:(fun acc t ->
        if String_set.mem t.package acc then
          acc
        else
          let acc = String_set.add t.package acc in
          match t.reason with
          | Not_found | Hidden -> acc
          | Dependencies_unavailable ts -> loop acc ts)
    in
    loop String_set.empty ts |> String_set.elements

  let top_closure ts =
    match Closure.top_closure () ts with
    | Ok ts   -> ts
    | Error _ ->
      code_errorf "Findlib.Package_not_available.top_sort got a cycle:\n%s"
        (all_names ts
         |> List.map ~f:(sprintf "- %s")
         |> String.concat ~sep:"\n")

  let explain ppf reason =
    match reason with
    | Not_found ->
      Format.fprintf ppf "not found"
    | Hidden ->
      Format.fprintf ppf "hidden (unsatisfied 'exist_if')"
    | Dependencies_unavailable deps ->
      Format.fprintf ppf
        "@[<2>unavailable dependencies:@ %t@]"
        (fun ppf ->
           match deps with
           | [] -> ()
           | t :: rest ->
             Format.fprintf ppf "%s" t.package;
             List.iter rest ~f:(fun t ->
               Format.fprintf ppf ",@ %s" t.package))
end

type present_or_not_available =
  | Present       of package
  | Not_available of Package_not_available.t

type t =
  { stdlib_dir    : Path.t
  ; path          : Path.t list
  ; builtins      : Meta.t String_map.t
  ; packages      : (string, present_or_not_available) Hashtbl.t
  }

let path t = t.path

let create ~stdlib_dir ~path =
  { stdlib_dir
  ; path
  ; builtins      = Meta.builtins ~stdlib_dir
  ; packages      = Hashtbl.create 1024
  }

module Pkg_step1 = struct
  type t =
    { package          : package
    ; requires         : string list
    ; ppx_runtime_deps : string list
    ; exists           : bool
    ; required_by      : With_required_by.Entry.t list
    }
end

module External_dep_conflicts_with_local_lib = struct
  type t =
    { package             : string
    ; required_by         : With_required_by.Entry.t
    ; required_locally_in : With_required_by.Entry.t list
    ; defined_locally_in  : Path.t
    }
end

type error =
  | Package_not_available of Package_not_available.t
  | External_dep_conflicts_with_local_lib of External_dep_conflicts_with_local_lib.t

exception Findlib of error

let parse_package t ~name ~parent_dir ~vars ~required_by =
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
  let jsoo_runtime = Vars.get_words vars "jsoo_runtime" [] in
  let preds = ["ppx_driver"; "mt"; "mt_posix"] in
  let pkg =
    { name
    ; dir
    ; version     = Vars.get vars "version" []
    ; description = Vars.get vars "description" []
    ; archives    = archives "archive" preds
    ; jsoo_runtime
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
  ; required_by
  }

let parse_meta t ~dir ~required_by (meta : Meta.t) =
  let rec loop ~dir ~full_name ~acc (meta : Meta.Simplified.t) =
    let vars = String_map.map meta.vars ~f:Rules.of_meta_rules in
    let pkg = parse_package t ~name:full_name ~parent_dir:dir ~vars ~required_by in
    let dir = pkg.package.dir in
    List.fold_left meta.subs ~init:(pkg :: acc) ~f:(fun acc (meta : Meta.Simplified.t) ->
      loop ~dir ~full_name:(sprintf "%s.%s" full_name meta.name) ~acc meta)
  in
  loop ~dir ~full_name:meta.name (Meta.simplify meta) ~acc:[]

let root_package_name s =
  match String.index s '.' with
  | None -> s
  | Some i -> String.sub s ~pos:0 ~len:i

let rec load_meta_rec t ~fq_name ~packages ~required_by =
  let root_name = root_package_name fq_name in
  if String_map.mem root_name packages ||
     Hashtbl.mem t.packages root_name then
    packages
  else
    (* Search for a <package>/META file in the findlib search path *)
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
        | None ->
          let required_by =
            if root_name = fq_name then
              required_by
            else
              With_required_by.Entry.Library fq_name :: required_by
          in
          Hashtbl.add t.packages ~key:root_name
            ~data:(Not_available { package = root_name
                                 ; required_by
                                 ; reason = Not_found
                                 });
          None
    in
    match loop t.path with
    | None -> packages
    | Some (dir, meta) ->
      let new_packages = parse_meta t ~dir ~required_by meta in
      let packages =
        List.fold_left new_packages ~init:packages ~f:(fun acc (pkg : Pkg_step1.t) ->
          String_map.add acc ~key:pkg.package.name ~data:pkg)
      in
      let deps =
        List.fold_left new_packages ~init:String_map.empty
          ~f:(fun acc (pkg : Pkg_step1.t) ->
            if pkg.exists then
              let add_deps acc deps =
                List.fold_left deps ~init:acc ~f:(fun acc dep ->
                  String_map.add acc ~key:dep ~data:pkg.package.name)
              in
              add_deps (add_deps acc pkg.requires) pkg.ppx_runtime_deps
            else
              acc)
      in
      String_map.fold deps ~init:packages ~f:(fun ~key:dep ~data:package packages ->
        load_meta_rec t ~fq_name:dep ~packages
          ~required_by:(With_required_by.Entry.Library package :: required_by))

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

let load_meta t ~fq_name ~required_by =
  let packages = load_meta_rec t ~fq_name ~packages:String_map.empty ~required_by in
  match Local_closure.top_closure packages (String_map.values packages) with
  | Error cycle ->
    die "dependency cycle detected between external findlib packages:\n   %s"
      (List.map cycle ~f:(fun (pkg : Pkg_step1.t) -> pkg.package.name)
       |> String.concat ~sep:"\n-> ")
  | Ok ordering ->
    List.iter ordering ~f:(fun (pkg : Pkg_step1.t) ->
      let status =
        if not pkg.exists then begin
          if !Clflags.debug_findlib then
            Printf.eprintf "findlib: package %S is hidden\n"
              pkg.package.name;
          Not_available
            { package     = pkg.package.name
            ; required_by = pkg.required_by
            ; reason      = Hidden
            }
        end else begin
          let resolve_deps deps missing_deps_acc =
            let deps, missing_deps =
              List.partition_map deps ~f:(fun name ->
                match Hashtbl.find t.packages name with
                | Some (Present pkg) -> Inl pkg
                | Some (Not_available na) -> Inr na
                | None ->
                  let na : Package_not_available.t =
                    { package     = name
                    ; required_by = Library pkg.package.name :: pkg.required_by
                    ; reason      = Not_found
                    }
                  in
                  Hashtbl.add t.packages ~key:name ~data:(Not_available na);
                  Inr na)
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
            Present pkg
          | _ ->
            Not_available
              { package     = pkg.package.name
              ; required_by = pkg.required_by
              ; reason      = Dependencies_unavailable missing_deps
              }
        end
      in
      Hashtbl.add t.packages ~key:pkg.package.name ~data:status
    )

let find_exn t ~required_by name =
  match Hashtbl.find t.packages name with
  | Some (Present x) -> x
  | Some (Not_available na) -> raise (Findlib (Package_not_available na))
  | None ->
    load_meta t ~fq_name:name ~required_by;
    match Hashtbl.find t.packages name with
    | Some (Present x) -> x
    | Some (Not_available pnf) ->
      raise (Findlib (Package_not_available pnf))
    | None ->
      let na : Package_not_available.t =
        { package = name
        ; required_by
        ; reason = Not_found
        }
      in
      Hashtbl.add t.packages ~key:name ~data:(Not_available na);
      raise (Findlib (Package_not_available na))

let find t ~required_by name =
  match find_exn t ~required_by name with
  | exception (Findlib (Package_not_available _)) -> None
  | x -> Some x

let available t ~required_by name =
  match find_exn t name ~required_by with
  | (_ : package) -> true
  | exception (Findlib (Package_not_available _)) -> false

let check_deps_consistency ~required_by ~local_public_libs pkg requires =
  List.iter requires ~f:(fun pkg' ->
    match String_map.find pkg'.name local_public_libs with
    | None -> ()
    | Some path ->
      raise (Findlib (External_dep_conflicts_with_local_lib
                        { package             = pkg'.name
                        ; required_by         = Library pkg.name
                        ; required_locally_in = required_by
                        ; defined_locally_in  = path
                        })))

let closure ~required_by ~local_public_libs pkgs =
  remove_dups_preserve_order
    (List.concat_map pkgs ~f:(fun pkg ->
       check_deps_consistency ~required_by ~local_public_libs pkg pkg.requires;
       pkg.requires)
     @ pkgs)

let closed_ppx_runtime_deps_of ~required_by ~local_public_libs pkgs =
  remove_dups_preserve_order
    (List.concat_map pkgs ~f:(fun pkg ->
       check_deps_consistency ~required_by ~local_public_libs pkg pkg.ppx_runtime_deps;
       pkg.ppx_runtime_deps))

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

let all_packages t =
  List.iter (root_packages t) ~f:(fun pkg ->
    ignore (find t pkg ~required_by:[] : package option));
  Hashtbl.fold t.packages ~init:[] ~f:(fun ~key:_ ~data acc ->
    match data with
    | Present       p -> p :: acc
    | Not_available _ -> acc)
  |> List.sort ~cmp:(fun a b -> String.compare a.name b.name)

let all_unavailable_packages t =
  List.iter (root_packages t) ~f:(fun pkg ->
    ignore (find t pkg ~required_by:[] : package option));
  Hashtbl.fold t.packages ~init:[] ~f:(fun ~key:_ ~data acc ->
    match data with
    | Present       _ -> acc
    | Not_available n -> n :: acc)
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
