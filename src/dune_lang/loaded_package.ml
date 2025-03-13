type opam_file =
  | Exists of bool
  | Generated

type t = {
  package : Package.t;
  has_opam_file : opam_file;
}

let package t = t.package
let has_opam_file t = t.has_opam_file

let create ~package ~has_opam_file = { package; has_opam_file }
