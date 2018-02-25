open Import

module P  = Variant
module Ps = Variant.Set

(* An assignment or addition *)
module Rule = struct
  type t =
    { preds_required  : Ps.t
    ; preds_forbidden : Ps.t
    ; value           : string
    }

  let formal_predicates_count t =
    Ps.cardinal t.preds_required + Ps.cardinal t.preds_forbidden

  let matches t ~preds =
    Ps.is_subset t.preds_required ~of_:preds &&
    Ps.is_empty (Ps.inter preds t.preds_forbidden)

  let make (rule : Meta.rule) =
    let preds_required, preds_forbidden =
      List.partition_map rule.predicates ~f:(function
        | Pos x -> Left  x
        | Neg x -> Right x)
    in
    { preds_required  = Ps.make preds_required
    ; preds_forbidden = Ps.make preds_forbidden
    ; value           = rule.value
    }
end

(* Set of rules for a given variable of a package. Implements the
   algorithm described here:

   http://projects.camlcity.org/projects/dl/findlib-1.6.3/doc/ref-html/r729.html
*)
module Rules = struct
  (* To implement the algorithm, [set_rules] is sorted by decreasing
     number of formal predicates, then according to the order of the
     META file. [add_rules] are in the same order as in the META
     file. *)
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
      |> List.stable_sort ~compare:(fun a b ->
        compare
          (Rule.formal_predicates_count b)
          (Rule.formal_predicates_count a))
    in
    { add_rules; set_rules }
end

module Vars = struct
  type t = Rules.t String_map.t

  let get (t : t) var preds =
    match String_map.find t var with
    | None -> None
    | Some rules -> Some (Rules.interpret rules ~preds)

  let get_words t var preds =
    match get t var preds with
    | None -> []
    | Some s -> String.extract_comma_space_separated_words s
end

module Config = struct
  type t =
    { vars  : Vars.t
    ; preds : Ps.t
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
    { vars = String_map.map vars ~f:Rules.of_meta_rules
    ; preds = Ps.make [toolchain]
    }

  let get { vars; preds } var =
    Vars.get vars var preds
end

module Package = struct
  type t =
    { meta_file : Path.t
    ; name      : string
    ; dir       : Path.t
    ; vars      : Vars.t
    }

  let meta_file t = t.meta_file
  let name      t = t.name
  let dir       t = t.dir

  let preds = Ps.of_list [P.ppx_driver; P.mt; P.mt_posix]

  let get_paths t var preds =
    List.map (Vars.get_words t.vars var preds) ~f:(Path.relative t.dir)

  let make_archives t var preds =
    Mode.Dict.of_func (fun ~mode ->
      get_paths t var (Ps.add preds (Mode.variant mode)))

  let version          t = Vars.get       t.vars "version"          Ps.empty
  let description      t = Vars.get       t.vars "description"      Ps.empty
  let jsoo_runtime     t = get_paths      t      "jsoo_runtime"     Ps.empty
  let requires         t = Vars.get_words t.vars "requires"         preds
  let ppx_runtime_deps t = Vars.get_words t.vars "ppx_runtime_deps" preds

  let archives t = make_archives t "archive" preds
  let plugins t =
    Mode.Dict.map2 ~f:(@)
      (make_archives t "archive" (Ps.add preds Variant.plugin))
      (make_archives t "plugin" preds)
end

module Unavailable_reason = struct
  type t =
    | Not_found
    | Hidden of Package.t

  let to_string = function
    | Not_found  -> "not found"
    | Hidden pkg ->
      sprintf "in %s is hidden (unsatisfied 'exist_if')"
        (Path.to_string_maybe_quoted (Package.dir pkg))

  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

type t =
  { stdlib_dir : Path.t
  ; path       : Path.t list
  ; builtins   : Meta.t String_map.t
  ; packages   : (string, (Package.t, Unavailable_reason.t) result) Hashtbl.t
  }

let path t = t.path

let root_package_name s =
  match String.index s '.' with
  | None -> s
  | Some i -> String.sub s ~pos:0 ~len:i

(* Parse a single package from a META file *)
let parse_package t ~meta_file ~name ~parent_dir ~vars =
  let pkg_dir = Vars.get vars "directory" Ps.empty in
  let dir =
    match pkg_dir with
    | None | Some "" -> parent_dir
    | Some pkg_dir ->
      if pkg_dir.[0] = '+' || pkg_dir.[0] = '^' then
        Path.relative t.stdlib_dir
          (String.sub pkg_dir ~pos:1 ~len:(String.length pkg_dir - 1))
      else if Filename.is_relative pkg_dir then
      Path.relative parent_dir pkg_dir
      else
        Path.absolute pkg_dir
  in
  let exists_if = Vars.get_words vars "exists_if" Ps.empty in
  let exists =
    List.for_all exists_if ~f:(fun fn ->
      Path.exists (Path.relative dir fn))
  in
  let pkg =
    { Package.
      meta_file
    ; name
    ; dir
    ; vars
    }
  in
  let res =
    if exists then
      Ok pkg
    else
      Error (Unavailable_reason.Hidden pkg)
  in
  (dir, res)

(* Parse all the packages defined in a META file and add them to
   [t.packages] *)
let parse_and_acknowledge_meta t ~dir ~meta_file (meta : Meta.t) =
  let rec loop ~dir ~full_name (meta : Meta.Simplified.t) =
    let vars = String_map.map meta.vars ~f:Rules.of_meta_rules in
    let dir, res =
      parse_package t ~meta_file ~name:full_name ~parent_dir:dir ~vars
    in
    Hashtbl.add t.packages full_name res;
    List.iter meta.subs ~f:(fun (meta : Meta.Simplified.t) ->
      loop ~dir ~full_name:(sprintf "%s.%s" full_name meta.name) meta)
  in
  loop ~dir ~full_name:meta.name (Meta.simplify meta)

(* Search for a <package>/META file in the findlib search path, parse
   it and add its contents to [t.packages] *)
let find_and_acknowledge_meta t ~fq_name =
  let root_name = root_package_name fq_name in
  let rec loop dirs : (Path.t * Path.t * Meta.t) option =
    match dirs with
    | dir :: dirs ->
      let sub_dir = Path.relative dir root_name in
      let fn = Path.relative sub_dir "META" in
      if Path.exists fn then
        Some (sub_dir,
              fn,
              { name    = root_name
              ; entries = Meta.load (Path.to_string fn)
              })
      else
        (* Alternative layout *)
        let fn = Path.relative dir ("META." ^ root_name) in
        if Path.exists fn then
          Some (dir,
                fn,
                { name    = root_name
                ; entries = Meta.load (Path.to_string fn)
                })
        else
          loop dirs
    | [] ->
      match String_map.find t.builtins root_name with
      | Some meta -> Some (t.stdlib_dir, Path.of_string "<internal>", meta)
      | None -> None
  in
  match loop t.path with
  | None ->
    Hashtbl.add t.packages root_name (Error Not_found)
  | Some (dir, meta_file, meta) ->
    parse_and_acknowledge_meta t meta ~meta_file ~dir

let find t name =
  match Hashtbl.find t.packages name with
  | Some x -> x
  | None ->
    find_and_acknowledge_meta t ~fq_name:name;
    match Hashtbl.find t.packages name with
    | Some x -> x
    | None ->
      let res = Error Unavailable_reason.Not_found in
      Hashtbl.add t.packages name res;
      res

let available t name =
  match find t name with
  | Ok    _ -> true
  | Error _ -> false

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
  String_set.to_list pkgs

let load_all_packages t =
  List.iter (root_packages t) ~f:(fun pkg ->
    find_and_acknowledge_meta t ~fq_name:pkg)

let all_packages t =
  load_all_packages t;
  Hashtbl.fold t.packages ~init:[] ~f:(fun x acc ->
    match x with
    | Ok    p -> p :: acc
    | Error _ -> acc)
  |> List.sort ~compare:(fun (a : Package.t) b -> String.compare a.name b.name)

let create ~stdlib_dir ~path =
  { stdlib_dir
  ; path
  ; builtins = Meta.builtins ~stdlib_dir
  ; packages = Hashtbl.create 1024
  }

let all_unavailable_packages t =
  load_all_packages t;
  Hashtbl.foldi t.packages ~init:[] ~f:(fun name x acc ->
    match x with
    | Ok    _ -> acc
    | Error e -> ((name, e) :: acc))
  |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
