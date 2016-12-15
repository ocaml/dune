open Import
open Jbuild_types

type t =
  { findlib              : Findlib.t
  ; libs                 : (string, Lib.t) Hashtbl.t
  ; internals_top_sorted : Lib.Internal.t list
  }

let find t name =
  match Hashtbl.find t.libs name with
  | Some x -> x
  | None ->
    let pkg = Findlib.find_exn t.findlib name in
    Hashtbl.add t.libs ~key:name ~data:(External pkg);
    External pkg

let find_internal t name =
  match Hashtbl.find t.libs name with
  | Some (Internal (dir, lib)) -> Some (dir, lib)
  | _ -> None

let split t names =
  List.partition_map names ~f:(fun name ->
    match find_internal t name with
    | Some x -> Inl x
    | None   -> Inr name)

module Local_closure = Top_closure.Make(String)(struct
    type graph = t
    type t = Lib.Internal.t
    let key ((_, lib) : t) = lib.name
    let deps ((_, lib) : Lib.Internal.t) graph =
      List.filter_map lib.libraries ~f:(fun dep ->
        find_internal graph dep)
  end)

let top_sort_internals t =
  let internals =
    Hashtbl.fold t.libs ~init:[] ~f:(fun ~key:_ ~data acc ->
      match data with
      | Lib.Internal lib -> lib :: acc
      | Lib.External _   -> acc)
  in
  match Local_closure.top_closure t internals with
  | Ok l -> l
  | Error cycle ->
    die "dependency cycle between libraries:\n   %s"
      (List.map cycle ~f:(fun lib -> Lib.describe (Internal lib))
       |> String.concat ~sep:"\n-> ")

let create findlib stanzas =
  let libs : (string, Lib.t) Hashtbl.t = Hashtbl.create 1024 in
  List.iter stanzas ~f:(fun (dir, stanzas) ->
    List.iter stanzas ~f:(fun stanza ->
      match (stanza : Stanza.t) with
      | Library lib ->
        let data = Lib.Internal (dir, lib) in
        Hashtbl.add libs ~key:lib.name ~data;
        Option.iter lib.public_name ~f:(fun name ->
          Hashtbl.add libs ~key:name ~data)
      | _ -> ()));
  let t = { findlib; libs; internals_top_sorted = [] } in
  let internals_top_sorted = top_sort_internals t in
  { t with internals_top_sorted }

let internal_libs_without_non_installable_optional_ones t =
  List.fold_left t.internals_top_sorted ~init:String_map.empty
    ~f:(fun acc (dir, lib) ->
      if not lib.Library.optional || (
        let int_deps, ext_deps = split t (lib.virtual_deps @ lib.libraries) in
        List.for_all int_deps ~f:(fun (_, dep) -> String_map.mem dep.Library.name acc) &&
        List.for_all ext_deps ~f:(Findlib.available t.findlib)) then
        String_map.add acc ~key:lib.name ~data:(dir, lib)
      else
        acc)
  |> String_map.values
