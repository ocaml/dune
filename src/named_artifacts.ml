open Import
open Jbuild_types

type t =
  { path      : Path.t list
  ; findlib   : Findlib.t
  ; artifacts : (string, Path.t) Hashtbl.t
  }

let create ~path findlib stanzas =
  let artifacts : (string, Path.t) Hashtbl.t = Hashtbl.create 1024 in
  List.iter stanzas ~f:(fun (dir, stanzas) ->
    List.iter stanzas ~f:(fun stanza ->
      match (stanza : Stanza.t) with
      | Provides { name; file } ->
        Hashtbl.add artifacts ~key:name ~data:(Path.relative dir file)
      | _ -> ()));
  { path; findlib; artifacts }

let binary t name =
  match Hashtbl.find t.artifacts name with
  | Some p -> p
  | None ->
    match Bin.which ~path:t.path name with
    | Some p ->
      Hashtbl.add t.artifacts ~key:name ~data:p;
      p
    | None ->
      die "Program %s not found in the tree or in the PATH" name

let in_findlib t name =
  match Hashtbl.find t.artifacts name with
  | Some p -> p
  | None ->
    match String.lsplit2 name ~on:':' with
    | None -> invalid_arg "Named_artifacts.in_findlib"
    | Some (pkg, file) ->
      let p = Path.relative (Findlib.find_exn t.findlib pkg).dir file in
      Hashtbl.add t.artifacts ~key:name ~data:p;
      p
