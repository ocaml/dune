open Import
open Memo.O

module Vars : sig
  type t

  val get : t -> string -> string

  val get_path : t -> string -> Path.t

  val of_lines : string list -> (t, User_message.Style.t Pp.t) result

  exception E of User_message.Style.t Pp.t
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
        | None -> Error Pp.(textf "Unrecognized line: %S" line))
    in
    let* vars = loop [] lines in
    Result.map_error (String.Map.of_list vars) ~f:(fun (var, _, _) ->
        Pp.(textf "Variable %S present twice." var))

  exception E of User_message.Style.t Pp.t

  let fail fmt msg = raise (E (Pp.textf fmt msg))

  let get_opt = String.Map.find

  let get t var =
    match get_opt t var with
    | Some s -> s
    | None -> fail "Variable %S not found." var

  let get_path t var = get t var |> Path.of_string
end

module Version = struct
  module Num = struct
    type t =
      { major : int
      ; minor : int
      ; suffix : string
      }

    let make version_string =
      let open Dune_re in
      let regex =
        let open Re in
        seq
          [ rep digit |> group; char '.'; rep digit |> group; rep any |> group ]
        |> compile
      in
      let open Result.O in
      let* groups =
        Re.exec_opt regex version_string |> function
        | Some groups -> Result.Ok groups
        | None -> Result.Error Pp.(textf "Could not parse version string.")
      in
      let* major =
        let major = Group.get groups 1 in
        major |> Int.of_string |> function
        | Some major -> Result.Ok major
        | None -> Result.Error Pp.(textf "Could not parse int: %S." major)
      in
      let* minor =
        let minor = Group.get groups 2 in
        minor |> Int.of_string |> function
        | Some minor -> Result.Ok minor
        | None -> Result.Error Pp.(textf "Could not parse int: %S." minor)
      in
      let suffix = Group.get groups 3 in
      Result.Ok { major; minor; suffix }

    let by_name { major; minor; suffix } name =
      match name with
      | "major" -> Some (`Int major)
      | "minor" -> Some (`Int minor)
      | "suffix" -> Some (`String suffix)
      | _ -> None
  end

  type t =
    { version_num : Num.t
    ; version_string : string
    ; ocaml_version_string : string
    }

  let impl_version bin =
    let* _ = Build_system.build_file bin in
    Memo.of_reproducible_fiber
    @@ Process.run_capture_line Process.Strict bin [ "--print-version" ]

  let version_memo =
    Memo.create "coq-and-ocaml-version" ~input:(module Path) impl_version

  let make ~(coqc : Action.Prog.t) =
    match coqc with
    | Ok coqc_path ->
      let open Memo.O in
      let+ coq_and_ocaml_version = Memo.exec version_memo coqc_path in
      let sbin = Path.to_string coqc_path in
      let open Result.O in
      let* version_string, ocaml_version_string =
        String.lsplit2 ~on:' ' coq_and_ocaml_version |> function
        | Some (version_string, ocaml_version_string) ->
          Result.ok (version_string, ocaml_version_string)
        | None ->
          Result.Error
            Pp.(textf "Unable to parse output of %s --print-version." sbin)
      in
      let* version_num = Num.make version_string in
      Result.ok { version_num; version_string; ocaml_version_string }
    | Error e -> Action.Prog.Not_found.raise e

  let by_name t name =
    match t with
    | Error msg ->
      User_error.raise Pp.[ textf "Could not parse coqc version: "; msg ]
    | Ok { version_num; version_string; ocaml_version_string } -> (
      match name with
      | "version.major" -> Num.by_name version_num "major"
      | "version.minor" -> Num.by_name version_num "minor"
      | "version.revision" -> Num.by_name version_num "revision"
      | "version.suffix" -> Num.by_name version_num "suffix"
      | "version" -> Some (`String version_string)
      | "ocaml-version" -> Some (`String ocaml_version_string)
      | _ -> None)
end

type t =
  { version_info : (Version.t, User_message.Style.t Pp.t) Result.t
  ; coqlib : Path.t
  ; coq_native_compiler_default : string
  }

let impl_config bin =
  let* _ = Build_system.build_file bin in
  Memo.of_reproducible_fiber
  @@ Process.run_capture_lines Process.Strict bin [ "--config" ]

let config_memo = Memo.create "coq-config" ~input:(module Path) impl_config

let version ~coqc =
  let open Memo.O in
  let+ t = Version.make ~coqc in
  let open Result.O in
  let+ t = t in
  t.version_string

let make ~(coqc : Action.Prog.t) =
  match coqc with
  | Ok coqc_path -> (
    let open Memo.O in
    let+ config_lines = Memo.exec config_memo coqc_path
    and+ version_info = Version.make ~coqc in
    match Vars.of_lines config_lines with
    | Ok vars ->
      let coqlib = Vars.get_path vars "COQLIB" in
      let coq_native_compiler_default =
        Vars.get vars "COQ_NATIVE_COMPILER_DEFAULT"
      in
      { version_info; coqlib; coq_native_compiler_default }
    | Error msg ->
      User_error.raise
        Pp.
          [ textf "cannot parse output of %S --config:"
              (Path.to_string coqc_path)
          ; msg
          ])
  | Error e -> Action.Prog.Not_found.raise e

let by_name { version_info; coqlib; coq_native_compiler_default } name =
  match name with
  | "version.major"
  | "version.minor"
  | "version.revision"
  | "version.suffix"
  | "version"
  | "ocaml-version" -> Version.by_name version_info name
  | "coqlib" -> Some (`Path coqlib)
  | "coq_native_compiler_default" -> Some (`String coq_native_compiler_default)
  | _ -> None
