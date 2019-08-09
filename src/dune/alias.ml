open! Stdune
open Import

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
        [ "name", Dyn.Encoder.string name
        ; "dir", Path.Build.to_dyn dir
        ];
    { dir; name }

  let of_user_written_path ~loc path =
    match Path.as_in_build_dir path with
    | Some path ->
      { dir = Path.Build.parent_exn path
      ; name = Path.Build.basename path
      }
    | None ->
      User_error.raise ~loc
        [ Pp.text "Invalid alias!"
        ; Pp.textf "Tried to reference path outside build dir: %S"
            (Path.to_string_maybe_quoted path)
        ];
end
include T

let compare x y =
  match String.compare x.name y.name with
  | Lt | Gt as x -> x
  | Eq -> Path.Build.compare x.dir y.dir

let equal x y = compare x y = Eq

let hash { dir ; name } =
  Tuple.T2.hash Path.Build.hash String.hash (dir, name)

let pp fmt t = Path.Build.pp fmt (Path.Build.relative t.dir t.name)

let to_dyn { dir ; name } =
  let open Dyn in
  Record
    [ "dir", Path.Build.to_dyn dir
    ; "name", String name
    ]

let suffix = "-" ^ String.make 32 '0'

let name t = t.name
let dir  t = t.dir

(* Where we store stamp files for aliases *)
let alias_dir = Path.Build.(relative root ".aliases")

let stamp_file_dir t =
  let local = Path.Build.local t.dir in
  Path.Build.append_local alias_dir local

let fully_qualified_name t = Path.Build.relative t.dir t.name

let stamp_file t =
  Path.Build.relative
    (stamp_file_dir t)
    (t.name ^ suffix)

let find_dir_specified_on_command_line ~dir ~file_tree =
  match File_tree.find_dir file_tree dir with
  | None ->
    User_error.raise
      [ Pp.textf "Don't know about directory %s specified on the \
                  command line!"
          (Path.Source.to_string_maybe_quoted dir)
      ]
  | Some dir -> dir

let standard_aliases = Table.create (module String) 7

let is_standard name = Table.mem standard_aliases name

let make_standard name =
  Table.add_exn standard_aliases name ();
  make name

let default     = make_standard "default"
let runtest     = make_standard "runtest"
let install     = make_standard "install"
let doc         = make_standard "doc"
let private_doc = make_standard "doc-private"
let lint        = make_standard "lint"
let all         = make_standard "all"
let check       = make_standard "check"
let fmt         = make_standard "fmt"

let encode { dir ; name } =
  let open Dune_lang.Encoder in
  record
    [ "dir", Dpath.encode (Path.build dir)
    ; "name", string name
    ]
