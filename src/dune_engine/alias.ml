open! Stdune
open Import

module Name : sig
  type t

  include Dune_lang.Conv.S with type t := t

  val equal : t -> t -> bool

  val hash : t -> int

  val compare : t -> t -> Ordering.t

  val of_string : string -> t

  val parse_string_exn : Loc.t * string -> t

  val to_string : t -> string

  val to_dyn : t -> Dyn.t

  val default : t

  val runtest : t

  val install : t

  val all : t

  val parse_local_path : Loc.t * Path.Local.t -> Path.Local.t * t

  module Map : Map.S with type key = t

  module Set : Set.S with type elt = t
end = struct
  include String

  (* DUNE3 once we get rid the "loose" validation, implement this module using
     [Stringlike] *)
  let of_string_opt_loose s = Option.some_if (Filename.basename s = s) s

  let of_string_opt = function
    (* The [""] case is caught by of_string_opt_loose. But there's no harm in
       being more explicit about it *)
    | ""
    | "."
    | "/"
    | ".." ->
      None
    | s -> of_string_opt_loose s

  let invalid_alias = Pp.textf "%S is not a valid alias name"

  let parse_string_exn ~syntax (loc, s) =
    let of_string_opt =
      if syntax >= (2, 0) then
        of_string_opt
      else
        of_string_opt_loose
    in
    match of_string_opt s with
    | None -> User_error.raise ~loc [ invalid_alias s ]
    | Some s -> s

  let encode = Dune_lang.Encoder.string

  let decode =
    let open Dune_lang.Decoder in
    let* syntax = Dune_lang.Syntax.get_exn Stanza.syntax in
    plain_string (fun ~loc s -> parse_string_exn ~syntax (loc, s))

  let parse_string_exn = parse_string_exn ~syntax:Stanza.latest_version

  let of_string s =
    match of_string_opt s with
    | Some s -> s
    | None ->
      Code_error.raise "invalid alias name" [ ("s", Dyn.Encoder.string s) ]

  let to_string s = s

  let default = "default"

  let runtest = "runtest"

  let install = "install"

  let all = "all"

  let to_dyn = String.to_dyn

  let parse_local_path (loc, p) =
    match Path.Local.parent p with
    | Some dir -> (dir, Path.Local.basename p)
    | None ->
      User_error.raise ~loc
        [ Pp.textf "Invalid alias path: %S"
            (Path.Local.to_string_maybe_quoted p)
        ]
end

module T : sig
  type t = private
    { dir : Path.Build.t
    ; name : Name.t
    }

  val make : Name.t -> dir:Path.Build.t -> t

  val of_user_written_path : loc:Loc.t -> Path.t -> t
end = struct
  type t =
    { dir : Path.Build.t
    ; name : Name.t
    }

  let make name ~dir = { dir; name }

  let of_user_written_path ~loc path =
    match Path.as_in_build_dir path with
    | Some path ->
      let name = Name.of_string (Path.Build.basename path) in
      { dir = Path.Build.parent_exn path; name }
    | None ->
      User_error.raise ~loc
        [ Pp.text "Invalid alias!"
        ; Pp.textf "Tried to reference path outside build dir: %S"
            (Path.to_string_maybe_quoted path)
        ]
end

include T

let compare x y =
  match Name.compare x.name y.name with
  | (Lt | Gt) as x -> x
  | Eq -> Path.Build.compare x.dir y.dir

let equal x y = compare x y = Eq

let hash { dir; name } = Tuple.T2.hash Path.Build.hash Name.hash (dir, name)

let to_dyn { dir; name } =
  let open Dyn in
  Record [ ("dir", Path.Build.to_dyn dir); ("name", Name.to_dyn name) ]

let suffix = "-" ^ String.make 32 '0'

let name t = t.name

let dir t = t.dir

let stamp_file_dir t =
  let local = Path.Build.local t.dir in
  Path.Build.append_local Dpath.Build.alias_dir local

let fully_qualified_name t = Path.Build.relative t.dir (Name.to_string t.name)

let stamp_file t =
  Path.Build.relative (stamp_file_dir t) (Name.to_string t.name ^ suffix)

(* This mutable table is safe: it's modified only at the top level. *)
let standard_aliases = Table.create (module Name) 7

let is_standard name = Table.mem standard_aliases name

let make_standard name =
  Table.add_exn standard_aliases name ();
  make name

let default = make_standard Name.default

let runtest = make_standard Name.runtest

let install = make_standard Name.install

let doc = make_standard (Name.of_string "doc")

let private_doc = make_standard (Name.of_string "doc-private")

let lint = make_standard (Name.of_string "lint")

let all = make_standard (Name.of_string "all")

let check = make_standard (Name.of_string "check")

let fmt = make_standard (Name.of_string "fmt")

let encode { dir; name } =
  let open Dune_lang.Encoder in
  record [ ("dir", Dpath.encode (Path.build dir)); ("name", Name.encode name) ]
