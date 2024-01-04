open Import

type extra_files =
  | Inside_files_dir
  | Git_files of Rev_store.File.t list

type nonrec t =
  { opam_file : OpamFile.OPAM.t
  ; package : OpamPackage.t
  ; opam_file_path : Path.Local.t
  ; source : Source_backend.t
  ; extra_files : extra_files
  }

let file t =
  match t.source with
  | Directory d -> Path.append_local d t.opam_file_path
  | Repo _ ->
    (* XXX fake path *)
    Path.source @@ Path.Source.of_local t.opam_file_path
;;

let package t = t.package
let opam_file t = t.opam_file
let extra_files t = t.extra_files
let source t = t.source

let create opam_file package opam_file_path source extra_files =
  { opam_file; package; opam_file_path; source; extra_files }
;;
