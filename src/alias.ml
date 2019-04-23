open! Stdune
open Import

module T : sig
  type t = private
    { dir : Path.t
    ; name : string
    }
  val make : string -> dir:Path.t -> t
  val of_user_written_path : loc:Loc.t -> Path.t -> t
end = struct
  type t =
    { dir : Path.t
    ; name : string
    }

  let make name ~dir =
    if not (Path.is_in_build_dir dir) || String.contains name '/' then
      Exn.code_error "Alias0.make: Invalid alias"
        [ "name", Sexp.Encoder.string name
        ; "dir", Path.to_sexp dir
        ];
    { dir; name }

  let of_user_written_path ~loc path =
    if not (Path.is_in_build_dir path) then
      Errors.fail loc "Invalid alias!\n\
                       Tried to reference path outside build dir: %S"
        (Path.to_string_maybe_quoted path);
    { dir = Path.parent_exn path
    ; name = Path.basename path
    }
end
include T

let compare x y =
  match String.compare x.name y.name with
  | Lt | Gt as x -> x
  | Eq -> Path.compare x.dir y.dir

let equal x y = compare x y = Eq

let hash { dir ; name } =
  Hashtbl.hash (Path.hash dir, String.hash name)

let pp fmt t = Path.pp fmt (Path.relative t.dir t.name)

let to_dyn { dir ; name } =
  let open Dyn in
  Record
    [ "dir", Path.to_dyn dir
    ; "name", String name
    ]

let to_sexp t = Dyn.to_sexp (to_dyn t)

let suffix = "-" ^ String.make 32 '0'

let name t = t.name
let dir  t = t.dir

let fully_qualified_name t = Path.relative t.dir t.name

let stamp_file t =
  Path.relative (Path.insert_after_build_dir_exn t.dir ".aliases")
    (t.name ^ suffix)

let find_dir_specified_on_command_line ~dir ~file_tree =
  match File_tree.find_dir file_tree dir with
  | None ->
    die "From the command line:\n\
         @{<error>Error@}: Don't know about directory %s!"
      (Path.Source.to_string_maybe_quoted dir)
  | Some dir -> dir

let standard_aliases = Hashtbl.create 7

let is_standard name = Hashtbl.mem standard_aliases name

let make_standard name =
  Hashtbl.add standard_aliases name ();
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
    [ "dir", Path_dune_lang.encode dir
    ; "name", string name
    ]
