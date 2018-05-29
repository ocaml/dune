open Import
open Sexp.Of_sexp

module Lang = struct
  type t =
    | Jbuilder
    | Dune of Syntax.Version.t

  let latest = Dune (0, 1)
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
    if Path.is_local path then
      Some (Anonymous path)
    else
      None

  let named_of_sexp sexp =
    let s = string sexp in
    if validate s then
      Named s
    else
      of_sexp_error sexp "invalid project name"

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
        if not (Path.is_local p) then invalid s;
        Anonymous p
      | _ when validate s -> Named s
      | _ -> invalid s
end

type t =
  { lang     : Lang.t
  ; name     : Name.t
  ; root     : Path.t
  ; version  : string option
  ; packages : Package.t Package.Name.Map.t
  }

let anonymous =
  { lang     = Lang.latest
  ; name     = Name.anonymous_root
  ; packages = Package.Name.Map.empty
  ; root     = Path.root
  ; version  = None
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
