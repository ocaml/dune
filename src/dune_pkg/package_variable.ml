open Stdune
open Dune_lang

module Name = struct
  include String

  include (
    Dune_util.Stringlike.Make (struct
      type t = string

      let to_string x = x
      let module_ = "Package_variable.Name"
      let description = "package variable name"
      let description_of_valid_string = None
      let hint_valid = None
      let of_string_opt s = if s = "" then None else Some s
    end) :
      Dune_util.Stringlike with type t := t)
end

module Scope = struct
  type t =
    | Self
    | Package of Package_name.t
end

type t =
  { name : Name.t
  ; scope : Scope.t
  }

let self_scoped name = { name; scope = Self }
let package_scoped name package_name = { name; scope = Package package_name }

let of_macro_invocation ~loc ({ Pform.Macro_invocation.macro; _ } as macro_invocation) =
  match macro with
  | Pkg_self ->
    let variable_name = Pform.Macro_invocation.Args.whole macro_invocation in
    Ok (self_scoped (Name.of_string variable_name))
  | Pkg ->
    let package_name, variable_name =
      Pform.Macro_invocation.Args.lsplit2_exn macro_invocation loc
    in
    Ok
      (package_scoped
         (Name.of_string variable_name)
         (Package_name.of_string package_name))
  | _ -> Error `Unexpected_macro
;;

let to_macro_invocation { name; scope } =
  match scope with
  | Self ->
    { Pform.Macro_invocation.macro = Pkg_self
    ; payload = Pform.Payload.of_args [ Name.to_string name ]
    }
  | Package package_name ->
    { Pform.Macro_invocation.macro = Pkg
    ; payload =
        Pform.Payload.of_args [ Package_name.to_string package_name; Name.to_string name ]
    }
;;

let to_pform t = Pform.Macro (to_macro_invocation t)

let global_variable variable =
  match variable with
  | "root" -> Error (`Unsupported_variable variable)
  | s -> Ok (Pform.Var.of_opam_global_variable_name s)
;;

let package_variable variable =
  match variable with
  | "hash" | "build-id" | "misc" | "opam-version" | "depends" | "build" | "opamfile" ->
    Error (`Unsupported_variable variable)
  | s -> Ok (Name.of_string s)
;;

let of_opam_ident ident =
  let ident = OpamVariable.to_string ident in
  match String.lsplit2 ident ~on:':' with
  | Some (package, variable) ->
    let variable = package_variable variable in
    Result.map variable ~f:(fun var ->
      `Package_variable
        (if package = "_"
         then self_scoped var
         else package_scoped var (Package_name.of_string package)))
  | None ->
    global_variable ident
    |> Result.bind ~f:(function
      | Some var -> Ok (`Global_variable var)
      | None ->
        let variable = package_variable ident in
        Result.map variable ~f:(fun var -> `Package_variable (self_scoped var)))
;;

let pform_of_opam_ident (loc, ident) =
  match of_opam_ident ident with
  | Ok (`Package_variable t) -> to_pform t
  | Ok (`Global_variable var) -> Pform.Var var
  | Error (`Unsupported_variable name) ->
    User_error.raise ~loc [ Pp.textf "Variable %S is not supported." name ]
;;
