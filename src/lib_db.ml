open Import
open Jbuild_types

type t =
  { findlib                  : Findlib.t
  ; libs                     : (string, Lib.t) Hashtbl.t
  ; instalable_internal_libs : Lib.Internal.t String_map.t
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

module Local_closure = Top_closure.Make(String)(struct
    type graph = t
    type t = Lib.Internal.t
    let key ((_, lib) : t) = lib.name
    let deps ((_, lib) : Lib.Internal.t) graph =
      List.concat_map lib.libraries ~f:(fun dep ->
        List.filter_map (Lib_dep.to_lib_names dep) ~f:(find_internal graph))
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

module Compute_instalable_internal_libs = struct
  let lib_is_installable t ~internal_instalable_libs name =
    match find_internal t name with
    | Some (_, lib) -> String_map.mem lib.name internal_instalable_libs
    | None -> Findlib.available t.findlib name

  let dep_is_installable t ~internal_instalable_libs dep =
    match (dep : Lib_dep.t) with
    | Direct s -> lib_is_installable t s ~internal_instalable_libs
    | Select { choices; _ } ->
      List.exists choices ~f:(fun c ->
        lib_is_installable t ~internal_instalable_libs c.Lib_dep.dep)

  let compute t =
    List.fold_left (top_sort_internals t) ~init:String_map.empty
      ~f:(fun acc (dir, lib) ->
        if not lib.Library.optional ||
           List.for_all (Library.all_lib_deps lib)
             ~f:(dep_is_installable t ~internal_instalable_libs:acc) then
          String_map.add acc ~key:lib.name ~data:(dir, lib)
        else
          acc)
end

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
  let t = { findlib; libs; instalable_internal_libs = String_map.empty } in
  { t with instalable_internal_libs =
             Compute_instalable_internal_libs.compute t }

let internal_libs_without_non_installable_optional_ones t =
  String_map.values t.instalable_internal_libs

let find_either t name =
  match find t name with
  | x -> Inl x
  | exception e ->
    (* Call [find] again to get a proper backtrace *)
    Inr { fail = fun () -> ignore (find t name : Lib.t); raise e }

let interpret_lib_deps t ~dir lib_deps =
  let libs, failures =
    List.partition_map lib_deps ~f:(function
      | Lib_dep.Direct s -> find_either t s
      | Select { result_fn; choices } ->
        match
          List.find_map choices ~f:(fun s ->
            match find_either t s.dep with
            | Inl _ as x -> Some x
            | Inr _ -> None)
        with
        | Some res -> res
        | None ->
          Inr { fail = fun () ->
            die "\
No solution found for the following form in %s:
  (select %s from
    %s)"
              (Path.to_string dir)
              result_fn
              (String.concat ~sep:"\n    "
                 (List.map choices ~f:(fun c -> c.Lib_dep.dep)))
          })
  in
  let internals, externals =
    List.partition_map libs ~f:(function
      | Internal x -> Inl x
      | External x -> Inr x)
  in
  (internals, externals,
   match failures with
   | [] -> None
   | f :: _ -> Some f)

let resolve_selects t lib_deps =
  List.filter_map lib_deps ~f:(function
    | Lib_dep.Direct _ -> None
    | Select { result_fn; choices } ->
      let contents =
        match List.find_map choices ~f:(fun s ->
          match find_either t s.dep with
          | Inl _ -> Some s.code
          | Inr _ -> None)
        with
        | Some code -> code
        | None -> "no solution found"
      in
      Some (result_fn, contents))
