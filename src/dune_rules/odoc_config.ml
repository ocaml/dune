(* Configuration for odoc documentation generation per package.

   Packages can provide a doc/{package}/odoc-config.sexp file that specifies
   additional dependencies for documentation generation.

   Format:
   (libraries lib1 lib2 ...)  ; Additional libraries to link against
   (packages pkg1 pkg2 ...)   ; Additional packages to link against
*)

open Import

type deps =
  { packages : Package.Name.t list
  ; libraries : Lib_name.t list
  }

type t = { deps : deps }

let empty = { deps = { libraries = []; packages = [] } }

(* Parse S-expression config file *)
let atom_to_string (sexp : Dune_sexp.Ast.t) =
  match sexp with
  | Atom (_loc, atom) -> Some (Dune_sexp.Atom.to_string atom)
  | _ -> None
;;

let parse_string_list sexps = List.filter_map sexps ~f:atom_to_string

let parse_entry (sexp : Dune_sexp.Ast.t) =
  match sexp with
  | Atom _ -> None
  | List (_loc, first :: rest) ->
    (match atom_to_string first with
     | Some "libraries" ->
       let libs = parse_string_list rest |> List.map ~f:Lib_name.of_string in
       Some (`Libraries libs)
     | Some "packages" ->
       let pkgs = parse_string_list rest |> List.map ~f:Package.Name.of_string in
       Some (`Packages pkgs)
     | _ -> None)
  | _ -> None
;;

let parse_exn s =
  let sexps = Dune_sexp.Parser.parse_string ~fname:"odoc-config.sexp" ~mode:Many s in
  let entries = List.filter_map sexps ~f:parse_entry in
  let libraries, packages =
    List.fold_left entries ~init:([], []) ~f:(fun (libs, pkgs) entry ->
      match entry with
      | `Libraries l -> l :: libs, pkgs
      | `Packages p -> libs, p :: pkgs)
  in
  let libraries = List.concat libraries |> List.sort_uniq ~compare:Lib_name.compare in
  let packages = List.concat packages |> List.sort_uniq ~compare:Package.Name.compare in
  { deps = { libraries; packages } }
;;

let load config_path =
  match Io.read_file config_path with
  | exception _ ->
    (* File doesn't exist or can't be read, return empty config *)
    empty
  | s ->
    (try parse_exn s with
     | exn ->
       User_warning.emit
         [ Pp.textf
             "Failed to parse odoc-config file %s: %s"
             (Path.to_string config_path)
             (Printexc.to_string exn)
         ];
       empty)
;;
