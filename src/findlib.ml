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
  ; requires         : string list
  ; ppx_runtime_deps : string list
  }

type t =
  { context  : Context.t
  ; packages : (string, package) Hashtbl.t
  }

let context t = t.context

let create context =
  { context
  ; packages = Hashtbl.create 1024
  }

let add_package t ~name ~parent_dir ~vars =
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
    ; version     = Vars.get vars "version" []
    ; description = Vars.get vars "description" []
    ; archives    = archives "archive" preds
    ; plugins     = Mode.Dict.map2 ~f:(@)
                      (archives "archive" ("plugin" :: preds))
                      (archives "plugin" preds)
    ; requires    = Vars.get_words vars "requires" preds
    ; ppx_runtime_deps = Vars.get_words vars "ppx_runtime_deps" preds
    }
  in
  Hashtbl.add t.packages ~key:name ~data:pkg;
  dir

let acknowledge_meta t ~dir (meta : Meta.t) =
  let rec loop ~dir ~full_name (meta : Meta.Simplified.t) =
    let vars = String_map.map meta.vars ~f:Rules.of_meta_rules in
    let dir = add_package t ~name:full_name ~parent_dir:dir ~vars in
    List.iter meta.subs ~f:(fun (meta : Meta.Simplified.t) ->
      loop ~dir ~full_name:(sprintf "%s.%s" full_name meta.name) meta)
  in
  loop ~dir ~full_name:meta.name (Meta.simplify meta)

exception Package_not_found of string

let root_package_name s =
  match String.index s '.' with
  | None -> s
  | Some i -> String.sub s ~pos:0 ~len:i

let load_meta t root_name =
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
      match Meta.builtin root_name with
      | Some meta -> (t.context.stdlib_dir, meta)
      | None -> raise (Package_not_found root_name)
  in
  let dir, meta = loop t.context.findlib_path in
  acknowledge_meta t ~dir meta

let find t name =
  match Hashtbl.find t.packages name with
  | Some x -> x
  | None ->
    load_meta t (root_package_name name);
    match Hashtbl.find t.packages name with
    | Some x -> x
    | None -> assert false

let root_packages t =
  let pkgs =
    List.concat_map t.context.findlib_path ~f:(fun dir ->
      Sys.readdir (Path.to_string dir)
      |> Array.to_list
      |> List.filter ~f:(fun name ->
        Path.exists (Path.relative dir (name ^ "/META"))))
  in
  let pkgs =
    if List.mem "compiler-libs" ~set:pkgs then
      pkgs
    else
      "compiler-libs" :: pkgs
  in
  List.sort pkgs ~cmp:String.compare

let all_packages t =
  List.iter (root_packages t) ~f:(fun pkg ->
    ignore (find t pkg : package));
  Hashtbl.fold t.packages ~init:[] ~f:(fun ~key:pkg ~data:_ acc -> pkg :: acc)
  |> List.sort ~cmp:String.compare
