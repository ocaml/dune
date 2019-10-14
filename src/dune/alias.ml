open! Stdune
open Import

module Name = struct
  type t = string

  let of_string_opt s =
    if Filename.basename s <> s then
      None
    else
      Some s

  let invalid_alias = Pp.textf "%S is not a valid alias name"

  let parse_string_exn (loc, s) =
    match of_string_opt s with
    | None -> User_error.raise ~loc [ invalid_alias s ]
    | Some s -> s

  let decode =
    let open Dune_lang.Decoder in
    plain_string (fun ~loc s -> parse_string_exn (loc, s))

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

  module Map = String.Map

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
    ; name : string
    }

  val make : string -> dir:Path.Build.t -> t

  val of_user_written_path : loc:Loc.t -> Path.t -> t
end = struct
  type t =
    { dir : Path.Build.t
    ; name : string
    }

  let make name ~dir =
    if String.contains name '/' then
      Code_error.raise "Alias0.make: Invalid alias"
        [ ("name", Dyn.Encoder.string name); ("dir", Path.Build.to_dyn dir) ];
    { dir; name }

  let of_user_written_path ~loc path =
    match Path.as_in_build_dir path with
    | Some path ->
      { dir = Path.Build.parent_exn path; name = Path.Build.basename path }
    | None ->
      User_error.raise ~loc
        [ Pp.text "Invalid alias!"
        ; Pp.textf "Tried to reference path outside build dir: %S"
            (Path.to_string_maybe_quoted path)
        ]
end

include T

let compare x y =
  match String.compare x.name y.name with
  | (Lt | Gt) as x -> x
  | Eq -> Path.Build.compare x.dir y.dir

let equal x y = compare x y = Eq

let hash { dir; name } = Tuple.T2.hash Path.Build.hash String.hash (dir, name)

let pp fmt t = Path.Build.pp fmt (Path.Build.relative t.dir t.name)

let to_dyn { dir; name } =
  let open Dyn in
  Record [ ("dir", Path.Build.to_dyn dir); ("name", String name) ]

let suffix = "-" ^ String.make 32 '0'

let name t = t.name

let dir t = t.dir

(* Where we store stamp files for aliases *)
let alias_dir = Path.Build.(relative root ".aliases")

let stamp_file_dir t =
  let local = Path.Build.local t.dir in
  Path.Build.append_local alias_dir local

let fully_qualified_name t = Path.Build.relative t.dir t.name

let stamp_file t = Path.Build.relative (stamp_file_dir t) (t.name ^ suffix)

let find_dir_specified_on_command_line ~dir =
  match File_tree.find_dir dir with
  | None ->
    User_error.raise
      [ Pp.textf "Don't know about directory %s specified on the command line!"
          (Path.Source.to_string_maybe_quoted dir)
      ]
  | Some dir -> dir

let standard_aliases = Table.create (module String) 7

let is_standard name = Table.mem standard_aliases name

let make_standard name =
  Table.add_exn standard_aliases name ();
  make name

let default = make_standard Name.default

let runtest = make_standard Name.runtest

let install = make_standard Name.install

let doc = make_standard "doc"

let private_doc = make_standard "doc-private"

let lint = make_standard "lint"

let all = make_standard "all"

let check = make_standard "check"

let fmt = make_standard "fmt"

let encode { dir; name } =
  let open Dune_lang.Encoder in
  record [ ("dir", Dpath.encode (Path.build dir)); ("name", string name) ]
