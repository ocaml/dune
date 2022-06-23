open Import
open Memo.O

module Vars : sig
  type t

  val get : t -> string -> string

  val get_path : t -> string -> Path.t

  val get_bool : t -> ?default:bool -> string -> bool

  val of_lines : string list -> (t, string) result

  exception E of string
end = struct
  open Result.O

  type t = string String.Map.t

  let of_lines lines =
    let rec loop acc = function
      | [] -> Ok acc
      | line :: lines -> (
        match String.index line '=' with
        | Some i ->
          let x = (String.take line i, String.drop line (i + 1)) in
          loop (x :: acc) lines
        | None -> Error (Printf.sprintf "Unrecognized line: %S" line))
    in
    let* vars = loop [] lines in
    Result.map_error (String.Map.of_list vars) ~f:(fun (var, _, _) ->
        Printf.sprintf "Variable %S present twice." var)

  exception E of string

  let fail fmt = Printf.ksprintf (fun msg -> raise (E msg)) fmt

  let get_opt = String.Map.find

  let get t var =
    match get_opt t var with
    | Some s -> s
    | None -> fail "Variable %S not found." var

  let get_path t var = get t var |> Path.of_string

  let get_bool t ?(default = false) var =
    match get_opt t var with
    | None -> default
    | Some s -> (
      match String.uncapitalize s with
      | "true" | "yes" | "1" -> true
      | _ -> default)
end

type t =
  { version : int * int * int
  ; version_string : string
  ; ocaml_version_string : string
  ; coqlib : Path.t
  ; coqcorelib : Path.t
  ; docdir : Path.t
  ; ocamlfind : Path.t
  ; camlflags : string
  ; warn : string
  ; hasnatdynlink : bool
  ; coq_src_subdirs : Path.t list
  ; coq_native_compiler_default : string
  }

let impl_version bin =
  let* _ = Build_system.build_file bin in
  Memo.of_non_reproducible_fiber
  @@ Process.run_capture_line Process.Strict bin [ "--print-version" ]

let version_memo =
  Memo.create "coq-and-ocaml-version" ~input:(module Path) impl_version

let impl_config bin =
  let* _ = Build_system.build_file bin in
  Memo.of_non_reproducible_fiber
  @@ Process.run_capture_line Process.Strict bin [ "--config" ]

let config_memo = Memo.create "coq-config" ~input:(module Path) impl_config

let make ~bin =
  let open Memo.O in
  let+ version_string, ocaml_version_string =
    Memo.exec version_memo bin >>| String.lsplit2 ~on:' ' >>| function
    | Some (coq, ocaml) -> (coq, ocaml)
    | None ->
      User_error.raise
        Pp.[ textf "cannot get version of %s" (Path.to_string bin) ]
  and+ config_lines = Memo.exec config_memo bin >>| String.split_lines in
  let version =
    match version_string |> String.split ~on:'.' with
    | major :: minor :: patch :: _ -> (
      match (Int.of_string major, Int.of_string minor, Int.of_string patch) with
      | Some major, Some minor, Some patch -> (major, minor, patch)
      | _, _, _ ->
        User_error.raise
          Pp.[ textf "cannot parse release version of %s" (Path.to_string bin) ]
      )
    | _ ->
      User_error.raise
        Pp.[ textf "cannot get release version of %s" (Path.to_string bin) ]
  in
  match Vars.of_lines config_lines with
  | Error msg ->
    User_error.raise
      Pp.[ textf "cannot parse %s -config: %s" (Path.to_string bin) msg ]
  | Ok vars ->
    let coqlib = Vars.get_path vars "COQLIB" in
    let coqcorelib = Vars.get_path vars "COQCORELIB" in
    let docdir = Vars.get_path vars "DOCDIR" in
    let ocamlfind = Vars.get_path vars "OCAMLFIND" in
    let camlflags = Vars.get vars "CAMLFLAGS" in
    let warn = Vars.get vars "WARN" in
    let hasnatdynlink = Vars.get_bool vars "HASNATDYNLINK" in
    let coq_src_subdirs =
      Vars.get vars "COQ_SRC_SUBDIRS"
      |> String.split ~on:' ' |> List.map ~f:Path.of_string
    in
    let coq_native_compiler_default =
      Vars.get vars "COQ_NATIVE_COMPILER_DEFAULT"
    in
    { version
    ; version_string
    ; ocaml_version_string
    ; coqlib
    ; coqcorelib
    ; docdir
    ; ocamlfind
    ; camlflags
    ; warn
    ; hasnatdynlink
    ; coq_src_subdirs
    ; coq_native_compiler_default
    }

let version ~bin =
  let open Memo.O in
  let+ t = make ~bin in
  t.version

module Value = struct
  type t =
    | Bool of bool
    | String of string
    | Path of Path.t
    | Paths of Path.t list
    | Version of int * int * int
end

let by_name
    { version = (major, minor, patch)
    ; version_string
    ; ocaml_version_string
    ; coqlib
    ; coqcorelib
    ; docdir
    ; ocamlfind
    ; camlflags
    ; warn
    ; hasnatdynlink
    ; coq_src_subdirs
    ; coq_native_compiler_default
    } name : Value.t option =
  match name with
  | "version" -> Some (Version (major, minor, patch))
  | "version_string" -> Some (String version_string)
  | "ocaml_version" -> Some (String ocaml_version_string)
  | "coqlib" -> Some (Path coqlib)
  | "coqcorelib" -> Some (Path coqcorelib)
  | "docdir" -> Some (Path docdir)
  | "ocamlfind" -> Some (Path ocamlfind)
  | "camlflags" -> Some (String camlflags)
  | "warn" -> Some (String warn)
  | "hasnatdynlink" -> Some (Bool hasnatdynlink)
  | "coq_src_subdirs" -> Some (Paths coq_src_subdirs)
  | "coq_native_compiler_default" -> Some (String coq_native_compiler_default)
  | _ -> None
