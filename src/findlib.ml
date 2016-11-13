open Import

module Preds : sig
  type t

  val make : string list -> t
  
  val is_subset : t -> subset:t -> bool
  val intersects : t -> t -> bool
end = struct
  type t = string list

  let make l = List.sort l ~cmp:String.compare

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
        is_subset t ~subset:l2

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

type rule =
  { preds_required  : Preds.t
  ; preds_forbidden : Preds.t
  ; action          : Meta.action
  ; value           : string
  }

type package =
  { name : string
  ; vars : rule list (* In reverse order of the META file *) String_map.t
  }

let db = Hashtbl.create 1024

let make_rule ((_, preds, action, value) : META.var) =
  let preds_required, preds_forbidden =
    List.partition_map preds ~f:(function
        | P x -> Inl x
        | A x -> Inr x)
  in
  { preds_required  = Preds.make preds_required
  ; preds_forbidden = Preds.make preds_forbidden
  ; action
  ; value
  }

let acknowledge_meta (meta : META.t) =
  let pkgs = META.flatten meta in
  List.iter pkgs ~f:(fun (name, vars) ->
      let vars =
        List.fold_left vars ~init:String_map.empty ~f:(fun acc ((vname, _, _, _) as var) ->
            let rule = make_rule var in
            let rules =
              match String_map.find acc vname with
              | exception Not_found -> []
              | rules -> rules
            in
            String_map.add acc ~key:vname ~data:(rule :: rules))
      in
      Hashtbl.add db name { name; vars })

let findlib_dir = ref ""

exception Package_not_found of string

let root_pkg s =
  match String.index s '.' with
  | exception Not_found -> s
  | i -> String.sub s ~pos:0 ~len:i

let rec get_pkg name =
  match Hashtbl.find db pkg with
  | exception Not_found ->
    let root = root_pkg name in
    let fn = !findlib_dir ^/ root ^/ "META" in
    if Sys.file_exists fn then begin
      acknowledge_meta { name = root; entries = META.load fn };
      get_pkg name
    end else
      raise (Package_not_found name)
  | pkg -> pkg

let rec interpret_rules rules ~preds =
  match rules with
  | [] -> None
  | rule :: rules ->
    if Preds.is_subset preds ~subset:rule.preds_required &&
       not (Preds.intersects preds rule.preds_forbidden) then
      match rule.action with
      | Set -> Some rule.value
      | Add ->
        match interpret_rules rules ~preds with
        | None -> Some rule.value
        | Some v -> Some (v ^ " " rule.value)
    else
      interpret_rules rules ~preds

let get_var pkg ~preds var =
  match String_map.find pkg.vars var with
  | exception Not_found -> None
  | rules -> interpret_rules rules ~preds

let query ~pkg ~preds ~var =
  get_var (get_pkg pkg) ~preds:(Preds.make preds) var
