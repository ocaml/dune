include Stdlib.Filename

type t = string

module Extension = struct
  type t = string

  let ml = ".ml"
  let mli = ".mli"
  let vo = ".vo"
  let vos = ".vos"
  let mllib = ".mllib"
  let mlpack = ".mllib"
  let mll = ".mll"
  let mly = ".mly"
  let theory_d = ".theory.d"
  let map = ".map"
  let odocl = ".odocl"
  let deps = ".deps"
  let cma = ".cma"
  let cmx = ".cmx"
  let cmxa = ".cmxa"
  let cmxs = ".cmxs"
  let cmi = ".cmi"
  let cmo = ".cmo"
  let cmj = ".cmj"
  let cmi_dump = ".cmi.dump"
  let cmo_dump = ".cmo.dump"
  let exe = ".exe"
  let bc = ".bc"
  let bc_exe = ".bc.exe"
  let ml_gen = ".ml-gen"
  let cmt = ".cmt"
  let cmti = ".cmti"
  let cms = ".cms"
  let cmsi = ".cmsi"
  let odoc = ".odoc"
  let d = ".d"
  let all_deps = ".all-deps"
  let js = ".js"
  let h = ".h"
  let mlg = ".mlg"
  let json = ".json"

  let is_valid s =
    (not (String.is_empty s)) && Char.equal s.[0] '.' && not (String.contains s '/')
  ;;

  let of_string s = Option.some_if (is_valid s) s

  let of_string_exn s =
    match of_string s with
    | Some t -> t
    | None ->
      Code_error.raise
        "Filename.Extension.of_string_exn: invalid extension"
        [ "extension", Dyn.string s ]
  ;;

  let to_string t = t
  let compare = String.compare
  let equal = String.equal
  let hash = String.hash
  let to_dyn t = Dyn.string t
  let drop_dot t = String.drop t 1

  module T = struct
    type nonrec t = t

    let compare = compare
    let to_dyn = to_dyn
  end

  module O = Comparable.Make (T)
  module Set = O.Set
  module Map = O.Map

  module Or_empty = struct
    type t = string

    let check = String.equal

    let of_string_exn = function
      | "" -> ""
      | s when s.[0] = '.' -> s
      | s -> Code_error.raise "invalid extension" [ "s", Dyn.string s ]
    ;;

    let empty = ""
    let to_string s = s
    let is_empty s = s = empty
    let is_extension s = not (is_empty s)

    let extension = function
      | "" -> None
      | s -> Some s
    ;;

    let extension_exn = function
      | "" -> Code_error.raise "no extension" []
      | s -> s
    ;;
  end
end

let split_extension fn =
  let ext = extension fn in
  if String.is_empty ext
  then fn, Extension.Or_empty.empty
  else String.sub fn ~pos:0 ~len:(String.length fn - String.length ext), ext
;;

let split_extension_after_dot fn =
  match extension fn with
  | "" -> fn, ""
  | s -> String.split_n fn (String.length fn - String.length s + 1)
;;

type program_name_kind =
  | In_path
  | Relative_to_current_dir
  | Absolute

let analyze_program_name fn =
  if not (is_relative fn)
  then Absolute
  else if String.contains fn '/' || (Stdlib.Sys.win32 && String.contains fn '\\')
  then Relative_to_current_dir
  else In_path
;;

let compare = String.compare
let equal = String.equal
let chop_extension = `Use_remove_extension

module Set = String.Set
module Map = String.Map
