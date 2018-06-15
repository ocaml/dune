open Import
open Sexp.Of_sexp

module Kind = struct
  type t =
    | Dune
    | Jbuilder
end

module Name : sig
  type t = private
    | Named     of string
    | Anonymous of Path.t

  val compare : t -> t -> Ordering.t

  val to_string_hum : t -> string

  val named_of_sexp : t Sexp.Of_sexp.t
  val sexp_of_t : t Sexp.To_sexp.t

  val encode : t -> string
  val decode : string -> t

  val anonymous : Path.t -> t option
  val named : string -> t option

  val anonymous_root : t
end = struct
  type t =
    | Named     of string
    | Anonymous of Path.t

  let anonymous_root = Anonymous Path.root

  let compare a b =
    match a, b with
    | Named     x, Named     y -> String.compare x y
    | Anonymous x, Anonymous y -> Path.compare   x y
    | Named     _, Anonymous _ -> Lt
    | Anonymous _, Named     _ -> Gt

  let to_string_hum = function
    | Named     s -> s
    | Anonymous p -> sprintf "<anonymous %s>" (Path.to_string_maybe_quoted p)

  let sexp_of_t = function
    | Named s -> Sexp.To_sexp.string s
    | Anonymous p ->
      List [ Sexp.unsafe_atom_of_string "anonymous"
           ; Path.sexp_of_t p
           ]

  let validate name =
    let len = String.length name in
    len > 0 &&
    String.for_all name ~f:(function
      | '.' | '/' -> false
      | _         -> true)

  let named name =
    if validate name then
      Some (Named name)
    else
      None

  let anonymous path =
    if Path.is_managed path then
      Some (Anonymous path)
    else
      None

  let named_of_sexp =
    Sexp.Of_sexp.Parser.map_validate string ~f:(fun s ->
      if validate s then
        Ok (Named s)
      else
        Sexp.Of_sexp.Parser.error "invalid project name")

  let encode = function
    | Named     s -> s
    | Anonymous p ->
      if Path.is_root p then
        "."
      else
        "." ^ String.map (Path.to_string p)
                ~f:(function
                  | '/' -> '.'
                  | c   -> c)

  let decode =
    let invalid s =
      (* Users would see this error if they did "dune build
         _build/default/.ppx/..." *)
      die "Invalid encoded project name: %S" s
    in
    fun s ->
      match s with
      | "" -> invalid s
      | "." -> anonymous_root
      | _ when s.[0] = '.' ->
        let p =
          Path.of_string
            (String.split s ~on:'.'
             |> List.tl
             |> String.concat ~sep:"/")
        in
        if not (Path.is_managed p) then invalid s;
        Anonymous p
      | _ when validate s -> Named s
      | _ -> invalid s
end

type t =
  { kind                  : Kind.t
  ; name                  : Name.t
  ; root                  : Path.Local.t
  ; version               : string option
  ; packages              : Package.t Package.Name.Map.t
  ; mutable stanza_parser : Stanza.t list Sexp.Of_sexp.t
  ; mutable project_file  : Path.t option
  }

type project = t

module Lang = struct
  type t = Syntax.Version.t * (project -> Stanza.Parser.t list)

  let make ver f = (ver, f)

  let langs = Hashtbl.create 32

  let register name versions =
    if Hashtbl.mem langs name then
      Exn.code_error "Dune_project.Lang.register: already registered"
        [ "name", Sexp.To_sexp.string name ];
    Hashtbl.add langs name (Syntax.Versioned_parser.make versions)

  let parse first_line =
    let { Dune_lexer.
          lang = (name_loc, name)
        ; version = (ver_loc, ver)
        } = first_line
    in
    let ver =
      Sexp.Of_sexp.parse Syntax.Version.t
        (Atom (ver_loc, Sexp.Atom.of_string ver)) in
    match Hashtbl.find langs name with
    | None ->
      Loc.fail name_loc "Unknown language %S.%s" name
        (hint name (Hashtbl.keys langs))
    | Some versions ->
      Syntax.Versioned_parser.find_exn versions
        ~loc:ver_loc ~data_version:ver

  let latest name =
    let versions = Option.value_exn (Hashtbl.find langs name) in
    Syntax.Versioned_parser.last versions

  let version = fst
end

module Extension = struct
  type maker = project -> Stanza.Parser.t list Sexp.Of_sexp.cstr_parser

  type t = Syntax.Version.t * maker

  let make ver f = (ver, f)

  let extensions = Hashtbl.create 32

  let register name versions =
    if Hashtbl.mem extensions name then
      Exn.code_error "Dune_project.Extension.register: already registered"
        [ "name", Sexp.To_sexp.string name ];
    Hashtbl.add extensions name (Syntax.Versioned_parser.make versions)

  let lookup (name_loc, name) (ver_loc, ver) =
    match Hashtbl.find extensions name with
    | None ->
      Loc.fail name_loc "Unknown extension %S.%s" name
        (hint name (Hashtbl.keys extensions))
    | Some versions ->
      Syntax.Versioned_parser.find_exn versions ~loc:ver_loc ~data_version:ver
end

let filename = "dune-project"

let get_local_path p =
  match Path.kind p with
  | External _ -> assert false
  | Local    p -> p

let anonymous = lazy(
  let t =
    { kind          = Dune
    ; name          = Name.anonymous_root
    ; packages      = Package.Name.Map.empty
    ; root          = get_local_path Path.root
    ; version       = None
    ; stanza_parser = Sexp.Of_sexp.make (fun _ -> assert false)
    ; project_file  = None
    }
  in
  t.stanza_parser <- Sexp.Of_sexp.sum (snd (Lang.latest "dune") t);
  t)

let default_name ~dir ~packages =
  match Package.Name.Map.choose packages with
  | None -> Option.value_exn (Name.anonymous dir)
  | Some (_, pkg) ->
    let pkg =
      Package.Name.Map.fold packages ~init:pkg ~f:(fun pkg acc ->
        if acc.Package.name <= pkg.Package.name then
          acc
        else
          pkg)
    in
    let name = Package.Name.to_string pkg.name in
    match Name.named name with
    | Some x -> x
    | None ->
      Loc.fail (Loc.in_file (Path.to_string (Package.opam_file pkg)))
        "%S is not a valid opam package name."
        name

let name ~dir ~packages =
  field_o "name" Name.named_of_sexp >>= function
  | Some x -> return x
  | None   -> return (default_name ~dir ~packages)

let parse ~dir ~lang_stanzas ~packages ~file =
  record
    (name ~dir ~packages >>= fun name ->
     field_o "version" string >>= fun version ->
     let t =
       { kind = Dune
       ; name
       ; root = get_local_path dir
       ; version
       ; packages
       ; stanza_parser = Sexp.Of_sexp.make (fun _ -> assert false)
       ; project_file  = Some file
       }
     in
     dup_field_multi "using"
       (list_loc >>= fun loc ->
        next (located string) >>= fun name ->
        next (located Syntax.Version.t) >>= fun ver ->
        Extension.lookup name ver t >>= fun stanzas ->
        return (snd name, (loc, stanzas)))
     >>= fun extensions ->
     let extensions_stanzas =
       match String.Map.of_list extensions with
       | Error (name, _, (loc, _)) ->
         Loc.fail loc "Extension %S specified for the second time." name
       | Ok _ ->
         List.concat_map extensions ~f:(fun (_, (_, x)) -> x)
     in
     t.stanza_parser <- Sexp.Of_sexp.sum (lang_stanzas t @ extensions_stanzas);
     return t)

let load_dune_project ~dir packages =
  let fname = Path.relative dir filename in
  Io.with_lexbuf_from_file fname ~f:(fun lb ->
    let lang_stanzas = Lang.parse (Dune_lexer.first_line lb) in
    let sexp = Sexp.Parser.parse lb ~mode:Many_as_one in
    Sexp.Of_sexp.parse (parse ~dir ~lang_stanzas ~packages ~file:fname) sexp)

let make_jbuilder_project ~dir packages =
  let t =
    { kind = Jbuilder
    ; name = default_name ~dir ~packages
    ; root = get_local_path dir
    ; version = None
    ; packages
    ; stanza_parser = Sexp.Of_sexp.make (fun _ -> assert false)
    ; project_file = None
    }
  in
  t.stanza_parser <- Sexp.Of_sexp.sum (snd (Lang.latest "dune") t);
  t

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

let notify_user s =
  kerrf ~f:print_to_console "@{<warning>Info@}: %s\n" s

let project_file t =
  match t.project_file with
  | Some file -> file
  | None ->
    let file = Path.relative (Path.of_local t.root) filename in
    let maj, min = fst (Lang.latest "dune") in
    let s = sprintf "(lang dune %d.%d)" maj min in
    notify_user
      (sprintf "creating file %s with this contents: %s"
         (Path.to_string_maybe_quoted file) s);
    Io.write_file file (s ^ "\n") ~binary:false;
    t.project_file <- Some file;
    file

let ensure_project_file_exists t =
  ignore (project_file t : Path.t)

let append_to_project_file t str =
  let file = project_file t in
  let prev = Io.read_file file ~binary:false in
  notify_user
    (sprintf "appending this line to %s: %s"
       (Path.to_string_maybe_quoted file) str);
  Io.with_file_out file ~binary:false ~f:(fun oc ->
    List.iter [prev; str] ~f:(fun s ->
      output_string oc s;
      let len = String.length s in
      if len > 0 && s.[len - 1] <> '\n' then output_char oc '\n'))

