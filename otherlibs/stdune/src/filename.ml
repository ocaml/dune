include Stdlib.Filename

type t = string

let is_valid s =
  (not (String.is_empty s))
  && (not (String.equal s current_dir_name))
  && (not (String.equal s parent_dir_name))
  && not (String.contains s '/')
;;

let of_string s = Option.some_if (is_valid s) s

let of_string_exn s =
  match of_string s with
  | Some t -> t
  | None ->
    Code_error.raise
      "Filename.of_string_exn: invalid filename"
      [ "filename", Dyn.string s ]
;;

let to_string t = t
let append dir t = concat dir (to_string t)
let to_dyn t = Dyn.string (to_string t)
let pp t = Pp.text (to_string t)
let remove_extension t = Stdlib.Filename.remove_extension t
let repr = Repr.view Repr.string ~to_:to_string

module L = struct
  let to_string t = t
end

module Extension = struct
  type t = string

  let filename_of_string_exn = of_string_exn
  let corrected = ".corrected"
  let ml = ".ml"
  let mli = ".mli"
  let vo = ".vo"
  let vos = ".vos"
  let mllib = ".mllib"
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
  let expected = ".expected"
  let bc = ".bc"
  let bc_exe = ".bc.exe"
  let ml_gen = ".ml-gen"
  let cmt = ".cmt"
  let cmti = ".cmti"
  let cms = ".cms"
  let cmsi = ".cmsi"
  let odoc = ".odoc"
  let opam = ".opam"
  let d = ".d"
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
  let to_filename t = filename_of_string_exn t
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

    let of_string_exn =
      let of_non_empty_string_exn = of_string_exn in
      function
      | "" -> ""
      | s -> of_non_empty_string_exn s
    ;;

    let empty = ""
    let to_string s = s
    let is_empty s = s = empty
    let is_extension s = not (is_empty s)

    let drop_suffix string t =
      if is_empty t
      then string
      else String.sub string ~pos:0 ~len:(String.length string - String.length t)
    ;;

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

let actions_dir_basename = ".actions"
let bin_dir_basename = ".bin"
let cc_vendor = "cc_vendor"
let cinaps_corrected = ".cinaps-corrected"
let checksum = "checksum"
let coqc = "coqc"
let corrected = Extension.to_filename Extension.corrected
let dev_tool_dir_basename = ".dev-tool"
let dev_tool_locks_dir_basename = ".dev-tool-locks"
let doc_dir_basename = "_doc"
let doc_new_dir_basename = "_doc_new"
let dune = "dune"
let dune_dir_basename = ".dune"
let dune_file = "dune-file"
let dune_project = "dune-project"
let dune_workspace = "dune-workspace"
let expected = Extension.to_filename Extension.expected
let fdo_profile = ".fdo-profile"
let formatted_dir_basename = ".formatted"
let generated = ".generated"
let git_dir_basename = ".git"
let gmake = "gmake"
let hg_dir_basename = ".hg"
let jbuild = "jbuild"
let json = Extension.to_filename Extension.json
let js_dir_basename = Extension.to_filename Extension.js
let lock_dune = "lock.dune"
let linker_script = ".linker-script"
let lock_dir_basename = ".lock"
let make = "make"
let mdx_deps = ".mdx.deps"
let merlin_conf_dir_basename = ".merlin-conf"
let meta = "META"
let ocamlfind = "ocamlfind"
let opam = "opam"
let ppx_dir_basename = ".ppx"
let pkg_dir_basename = ".pkg"
let rocq = "rocq"
let run_t = "run.t"
let template = ".template"
let topmod_dir_basename = ".topmod"
let url = "url"
let utop_dir_basename = ".utop"
let findlib_conf = "findlib.conf"
let extension fn = Stdlib.Filename.extension fn |> Extension.Or_empty.of_string_exn

let split_extension fn =
  let ext = extension fn in
  Extension.Or_empty.drop_suffix fn ext, ext
;;

let split_extension_after_dot fn =
  match Extension.Or_empty.to_string (extension fn) with
  | "" -> fn, ""
  | s -> String.split_n fn (String.length fn - String.length s + 1)
;;

let add_extension fn ext = fn ^ Extension.to_string ext
let set_extension fn ext = add_extension (remove_extension fn) ext
let extend fn ~suffix = fn ^ suffix

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
let hash = String.hash
let chop_extension = `Use_remove_extension

module Set = String.Set
module Map = String.Map
module Array0 = Array

module Array = Array0.Sorted.Make (struct
    type nonrec t = t

    let compare = compare
    let to_dyn t = Dyn.string (to_string t)
  end)
