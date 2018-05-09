open Import
open Sexp.Of_sexp

module Lang = struct
  type t =
    | Jbuilder
    | Dune of Syntax.Version.t
end

type t =
  { lang     : Lang.t
  ; name     : string
  ; root     : Path.t
  ; version  : string option
  ; packages : Package.t Package.Name.Map.t
  }

let filename = "dune-project"

type lang =
  | Dune_0_1

let lang =
  let name =
    enum
      [ ("dune", ()) ]
  in
  let version ver =
    match string ver with
    | "0.1" -> Dune_0_1
    | _ ->
      of_sexp_error ver "unsupported version of the dune language"
  in
  field_multi "lang" (name @> version @> nil) (fun () v -> v)

let default_name ~dir ~packages =
  match Package.Name.Map.choose packages with
  | None ->
    "_" ^ String.concat ~sep:"_" (Path.explode_exn dir)
  | Some (name, _) ->
    Package.Name.to_string
      (Package.Name.Map.fold packages ~init:name ~f:(fun pkg acc ->
         min acc pkg.Package.name))

let name ~dir ~packages =
  field_o "name" string >>= function
  | Some s -> return s
  | None -> return (default_name ~dir ~packages)

let parse ~dir packages =
  record
    (lang >>= fun Dune_0_1 ->
     name ~dir ~packages >>= fun name ->
     field_o "version" string >>= fun version ->
     return { lang = Dune (0, 1)
            ; name
            ; root = dir
            ; version
            ; packages
            })

let load_dune_project ~dir packages =
  let fname = Path.relative dir filename in
  let sexp = Io.Sexp.load_many_as_one fname in
  parse ~dir packages sexp

let make_jbuilder_project ~dir packages =
  { lang = Jbuilder
  ; name = default_name ~dir ~packages
  ; root = dir
  ; version = None
  ; packages
  }

let load ~dir ~files =
  let packages =
    String.Set.fold files ~init:[] ~f:(fun fn acc ->
      match Filename.split_extension fn with
      | (pkg, ".opam") when pkg <> "" ->
        let version_from_opam_file =
          let opam = Opam_file.load (Path.relative dir fn) in
          match Opam_file.get_field opam "version" with
          | Some (String (_, s)) -> Some s
          | _ -> None
        in
        let name = Package.Name.of_string pkg in
        (name,
         { Package. name
         ; path = dir
         ; version_from_opam_file
         }) :: acc
      | _ -> acc)
    |> Package.Name.Map.of_list_exn
  in
  if String.Set.mem files filename then
    Some (load_dune_project ~dir packages)
  else if not (Package.Name.Map.is_empty packages) then
    Some (make_jbuilder_project ~dir packages)
  else
    None
