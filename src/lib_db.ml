open Import
open Jbuild_types

type t =
  { findlib : Findlib.t
  ; libs    : (string, Lib.t) Hashtbl.t
  }

let find t name =
  match Hashtbl.find t.libs name with
  | Some x -> x
  | None ->
    let pkg = Findlib.find t.findlib name in
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

let check_internal_cycles t =
  let internals =
    Hashtbl.fold t.libs ~init:[] ~f:(fun ~key:_ ~data acc ->
      match data with
      | Lib.Internal lib -> lib :: acc
      | Lib.External _   -> acc)
  in
  match Local_closure.top_closure t internals with
  | Ok _ -> ()
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
  let t = { findlib; libs } in
  check_internal_cycles t;
  t
