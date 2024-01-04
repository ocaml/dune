open Import

type extra_files =
  | Inside_files_dir of Path.t
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

let git_repo package opam_file ~opam_file_path source ~extra_files =
  { opam_file; package; opam_file_path; source; extra_files = Git_files extra_files }
;;

let local_fs package ~dir ~opam_file_path ~files_dir =
  let opam_file =
    Path.append_local dir opam_file_path
    |> Path.to_string
    |> OpamFilename.raw
    |> OpamFile.make
    |> OpamFile.OPAM.read
  in
  { package
  ; opam_file_path
  ; source = Directory dir
  ; extra_files = Inside_files_dir files_dir
  ; opam_file
  }
;;
