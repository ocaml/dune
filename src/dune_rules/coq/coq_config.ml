open Import
open Memo.O

module Vars : sig
  type t

  val get_opt : t -> string -> string option
  val get_path : t -> string -> Path.t
  val get_path_opt : t -> string -> Path.t option
  val of_lines : string list -> (t, User_message.Style.t Pp.t) result
end = struct
  open Result.O

  type t = string String.Map.t

  let of_lines lines =
    let rec loop acc = function
      | [] -> Ok acc
      | line :: lines ->
        (match String.index line '=' with
         | Some i ->
           let x = String.take line i, String.drop line (i + 1) in
           loop (x :: acc) lines
         | None -> Error Pp.(textf "Unrecognized line: %S" line))
    in
    let* vars = loop [] lines in
    Result.map_error (String.Map.of_list vars) ~f:(fun (var, _, _) ->
      Pp.(textf "Variable %S present twice." var))
  ;;

  let get_opt = String.Map.find

  let get t var =
    match get_opt t var with
    | Some s -> s
    | None -> Code_error.raise "Variable not found." [ "var", Dyn.string var ]
  ;;

  let get_path t var = get t var |> Path.of_string
  let get_path_opt t var = Option.map ~f:Path.of_string (get_opt t var)
end

module Value : sig
  type t = private
    | Int of int
    | Path of Path.t
    | String of string

  val int : int -> t
  val string : string -> t
  val path : Path.t -> t
  val to_dyn : t -> Dyn.t
end = struct
  type t =
    | Int of int
    | Path of Path.t
    | String of string

  let int i = Int i
  let string s = String s
  let path p = Path p

  let to_dyn = function
    | Int i -> Dyn.Int i
    | Path p -> Path.to_dyn p
    | String s -> Dyn.String s
  ;;
end

let get_output_from_config_or_version ~(coqc : Action.Prog.t) ~what memo =
  match coqc with
  | Ok coqc_path ->
    let open Memo.O in
    let+ output, exit_code = Memo.exec memo coqc_path in
    let sbin = Path.to_string coqc_path in
    if exit_code <> 0
    then
      Error
        (Pp.vbox
         @@ Pp.concat_map
              ~sep:Pp.space
              ~f:(fun x -> Pp.hbox (Pp.text x))
              (sprintf "%s %s failed with exit code %d." sbin what exit_code :: output))
    else Ok output
  | Error e -> Action.Prog.Not_found.raise e
;;

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
        seq [ rep digit |> group; char '.'; rep digit |> group; rep any |> group ]
        |> compile
      in
      let open Result.O in
      let* groups =
        Re.exec_opt regex version_string
        |> function
        | Some groups -> Result.Ok groups
        | None -> Result.Error Pp.(textf "Could not parse version string.")
      in
      let* major =
        let major = Group.get groups 1 in
        major
        |> Int.of_string
        |> function
        | Some major -> Result.Ok major
        | None -> Result.Error Pp.(textf "Could not parse int: %S." major)
      in
      let* minor =
        let minor = Group.get groups 2 in
        minor
        |> Int.of_string
        |> function
        | Some minor -> Result.Ok minor
        | None -> Result.Error Pp.(textf "Could not parse int: %S." minor)
      in
      let suffix = Group.get groups 3 in
      Result.Ok { major; minor; suffix }
    ;;

    let by_name { major; minor; suffix } name =
      match name with
      | "major" -> Some (Value.int major)
      | "minor" -> Some (Value.int minor)
      | "suffix" -> Some (Value.string suffix)
      | _ -> None
    ;;
  end

  type t =
    { version_num : Num.t
    ; version_string : string
    ; ocaml_version_string : string
    }

  let impl_version bin =
    let* _ = Build_system.build_file bin in
    Memo.of_reproducible_fiber
    @@ Process.run_capture_lines
         ~display:Quiet
         ~stderr_to:
           (Process.Io.make_stderr
              ~output_on_success:Swallow
              ~output_limit:Execution_parameters.Action_output_limit.default)
         Return
         bin
         (* we pass -boot since this means it will work with just coq-core *)
         [ "--print-version"; "-boot" ]
  ;;

  let version_memo = Memo.create "coq-and-ocaml-version" ~input:(module Path) impl_version

  let make ~(coqc : Action.Prog.t) =
    let open Memo.O in
    let+ version_output =
      get_output_from_config_or_version ~coqc ~what:"--print-version" version_memo
    in
    let open Result.O in
    let* version_output = version_output in
    let* version_string, ocaml_version_string =
      (* First check that the command returned a single line *)
      let* line =
        match version_output with
        | [ line ] -> Ok line
        | _ -> Error Pp.(textf "Multiple lines in output of coqc --print-version.")
      in
      (* Next split into two parts, version looks like ["<coq-version> <ocaml-version>"] *)
      match String.lsplit2 ~on:' ' line with
      | Some (version_string, ocaml_version_string) ->
        Ok (version_string, ocaml_version_string)
      | None -> Error Pp.(textf "Unable to parse output of coqc --print-version.")
    in
    let+ version_num = Num.make version_string in
    { version_num; version_string; ocaml_version_string }
  ;;

  let by_name t name =
    match t with
    | Error msg -> User_error.raise Pp.[ textf "Could not parse coqc version: "; msg ]
    | Ok { version_num; version_string; ocaml_version_string } ->
      (match name with
       | "version.major" -> Num.by_name version_num "major"
       | "version.minor" -> Num.by_name version_num "minor"
       | "version.revision" -> Num.by_name version_num "revision"
       | "version.suffix" -> Num.by_name version_num "suffix"
       | "version" -> Some (Value.string version_string)
       | "ocaml-version" -> Some (Value.string ocaml_version_string)
       | _ -> None)
  ;;
end

type t =
  { version_info : (Version.t, User_message.Style.t Pp.t) Result.t
  ; coqlib : Path.t
  ; coqcorelib : Path.t option (* this is not available in Coq < 8.14 *)
  ; coq_native_compiler_default : string option (* this is not available in Coq < 8.13 *)
  }

let impl_config bin =
  let* _ = Build_system.build_file bin in
  Memo.of_reproducible_fiber
  @@ Process.run_capture_lines
       ~display:Quiet
       ~stderr_to:
         (Process.Io.make_stderr
            ~output_on_success:Swallow
            ~output_limit:Execution_parameters.Action_output_limit.default)
       Return
       bin
       [ "--config" ]
;;

let config_memo = Memo.create "coq-config" ~input:(module Path) impl_config

let version ~coqc =
  let open Memo.O in
  let+ t = Version.make ~coqc in
  let open Result.O in
  let+ t = t in
  t.version_string
;;

let make ~(coqc : Action.Prog.t) =
  let open Memo.O in
  let+ config_output =
    get_output_from_config_or_version ~coqc ~what:"--config" config_memo
  and+ version_info = Version.make ~coqc in
  let open Result.O in
  let* config_output = config_output in
  match Vars.of_lines config_output with
  | Ok vars ->
    let coqlib = Vars.get_path vars "COQLIB" in
    (* this is not available in Coq < 8.14 *)
    let coqcorelib = Vars.get_path_opt vars "COQCORELIB" in
    (* this is not available in Coq < 8.13 *)
    let coq_native_compiler_default = Vars.get_opt vars "COQ_NATIVE_COMPILER_DEFAULT" in
    Ok { version_info; coqlib; coqcorelib; coq_native_compiler_default }
  | Error msg ->
    User_error.raise Pp.[ textf "Cannot parse output of coqc --config:"; msg ]
;;

let by_name { version_info; coqlib; coqcorelib; coq_native_compiler_default } name =
  match name with
  | "version.major"
  | "version.minor"
  | "version.revision"
  | "version.suffix"
  | "version"
  | "ocaml-version" -> Version.by_name version_info name
  | "coqlib" -> Some (Value.path coqlib)
  | "coqcorelib" -> Option.map ~f:Value.path coqcorelib
  | "coq_native_compiler_default" ->
    Option.map ~f:Value.string coq_native_compiler_default
  | _ ->
    Code_error.raise
      "Unknown name was requested from coq_config"
      [ "name", Dyn.string name ]
;;

let expand source macro artifacts_host =
  let s = Pform.Macro_invocation.Args.whole macro in
  let open Memo.O in
  let* coqc = Artifacts.binary artifacts_host ~where:Original_path ~loc:None "coqc" in
  let+ t = make ~coqc in
  match t with
  | Error msg ->
    User_error.raise
      ~loc:(Dune_lang.Template.Pform.loc source)
      [ Pp.textf "Could not expand %%{coq:%s} as running coqc --config failed." s; msg ]
      ~hints:
        [ Pp.textf
            "coqc --config requires the coq-stdlib package in order to function properly."
        ]
  | Ok t ->
    (match by_name t s with
     | None ->
       User_error.raise
         ~loc:(Dune_lang.Template.Pform.loc source)
         [ Pp.textf "Unknown Coq configuration variable %S" s ]
     | Some v ->
       (match v with
        | Int x -> [ Dune_lang.Value.String (string_of_int x) ]
        | String x -> [ String x ]
        | Path x -> Dune_lang.Value.L.paths [ x ]))
;;
