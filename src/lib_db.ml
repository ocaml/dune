open Import
open Jbuild_types

type t =
  { findlib : Findlib.t
  ; libs    : (string, Lib.t) Hashtbl.t
  }

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
  { findlib; libs }

let find t name =
  match Hashtbl.find t.libs name with
  | Some x -> x
  | None ->
    let pkg = Findlib.find t.findlib name in
    Hashtbl.add t.libs ~key:name ~data:(External pkg);
    External pkg

module Top_closure = Top_closure.Make(String)(struct
    type graph = t
    type t = Lib.t
    let key = Lib.best_name
    let deps t graph =
      let lib =
        Hashtbl.find_exn graph.libs (key t) ~string_of_key:(sprintf "%S")
          ~table_desc:(fun _ ->
            sprintf "<libraries for context %s>"
              (Path.to_string (Findlib.context graph.findlib).build_dir))
      in
      List.map (Lib.deps lib) ~f:(find graph)
  end)

let top_closure t names =
  match Top_closure.top_closure t (List.map names ~f:(find t)) with
  | Ok order -> order
  | Error cycle ->
    die "dependency cycle between libraries:\n   %s"
      (List.map cycle ~f:Lib.describe
       |> String.concat ~sep:"\n-> ")
