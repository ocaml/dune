open Import

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
