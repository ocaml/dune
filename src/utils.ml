open! Stdune
open Import

let system_shell_exn =
  let cmd, arg, os =
    if Sys.win32 then
      ("cmd", "/c", "on Windows")
    else
      ("sh", "-c", "")
  in
  let bin = lazy (Bin.which ~path:(Env.path Env.initial) cmd) in
  fun ~needed_to ->
    match Lazy.force bin with
    | Some path -> (path, arg)
    | None ->
      User_error.raise
        [ Pp.textf "I need %s to %s but I couldn't find it :(\n\
                    Who doesn't have %s%s?!"
            cmd needed_to cmd os ]

let bash_exn =
  let bin = lazy (Bin.which ~path:(Env.path Env.initial) "bash") in
  fun ~needed_to ->
    match Lazy.force bin with
    | Some path -> path
    | None ->
      User_error.raise
        [ Pp.textf "I need bash to %s but I couldn't find it :("
            needed_to
        ]

let library_object_directory ~dir name =
  Path.Build.relative dir ("." ^ Lib_name.Local.to_string name ^ ".objs")

let library_native_dir ~obj_dir =
  Path.Build.relative obj_dir "native"

let library_byte_dir ~obj_dir =
  Path.Build.relative obj_dir "byte"

let library_public_cmi_dir ~obj_dir =
  Path.Build.relative obj_dir "public_cmi"

let library_private_dir ~obj_dir =
  Path.Build.relative obj_dir "private"

(* Use "eobjs" rather than "objs" to avoid a potential conflict with a
   library of the same name *)
let executable_object_directory ~dir name =
  Path.Build.relative dir ("." ^ name ^ ".eobjs")

let not_found fmt ?loc ?context ?hint x =
  User_error.raise ?loc
    (Pp.textf fmt (String.maybe_quoted x)
     :: match context with
     | None -> []
     | Some name -> [Pp.textf " (context: %s)" name])
    ~hints:(match hint with
      | None -> []
      | Some hint -> [Pp.text hint])

let program_not_found ?context ?hint ~loc prog =
  not_found "Program %s not found in the tree or in PATH"
    ?context ?hint ?loc prog

let library_not_found ?context ?hint lib =
  not_found "Library %s not found"
    ?context ?hint lib

let install_file ~(package : Package.Name.t) ~findlib_toolchain =
  let package = Package.Name.to_string package in
  match findlib_toolchain with
  | None -> package ^ ".install"
  | Some x -> sprintf "%s-%s.install" package x

let line_directive ~filename:fn ~line_number =
  let directive =
    if C.c_cxx_or_header ~fn then
      "line"
    else
      ""
  in
  sprintf "#%s %d %S\n" directive line_number fn

let local_bin p = Path.Build.relative p ".bin"

module type Persistent_desc = sig
  type t
  val name : string
  val version : int
end

module Persistent(D : Persistent_desc) = struct
  let magic = sprintf "DUNE-%sv%d:" D.name D.version

  let to_out_string (v : D.t) =
    magic ^ Marshal.to_string v []

  let dump file (v : D.t) =
    Io.with_file_out file ~f:(fun oc ->
      output_string oc magic;
      Marshal.to_channel oc v [])

  let load file =
    if Path.exists file then
      Io.with_file_in file ~f:(fun ic ->
        match really_input_string ic (String.length magic) with
        | exception End_of_file -> None
        | s ->
          if s = magic then
            Some (Marshal.from_channel ic : D.t)
          else
            None)
    else
      None
end
