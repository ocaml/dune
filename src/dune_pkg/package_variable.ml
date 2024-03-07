open Import

module Scope = struct
  type t =
    | Self
    | Package of Package_name.t

  let compare x y =
    match x, y with
    | Self, Self -> Eq
    | Self, Package _ -> Gt
    | Package _, Self -> Lt
    | Package x, Package y -> Package_name.compare x y
  ;;

  let to_dyn = function
    | Self -> Dyn.variant "Self" []
    | Package name -> Dyn.variant "Package" [ Package_name.to_dyn name ]
  ;;
end

module T = struct
  type t =
    { name : Package_variable_name.t
    ; scope : Scope.t
    }

  let compare t { name; scope } =
    match Scope.compare t.scope scope with
    | Eq -> Package_variable_name.compare t.name name
    | x -> x
  ;;

  let to_dyn { name; scope } =
    let open Dyn in
    record [ "name", Package_variable_name.to_dyn name; "scope", Scope.to_dyn scope ]
  ;;
end

include T
module C = Comparable.Make (T)
include C

let self_scoped name = { name; scope = Self }
let package_scoped name package_name = { name; scope = Package package_name }

let of_macro_invocation ~loc ({ Pform.Macro_invocation.macro; _ } as macro_invocation) =
  match macro with
  | Pkg_self ->
    let variable_name = Pform.Macro_invocation.Args.whole macro_invocation in
    Ok (self_scoped (Package_variable_name.of_string variable_name))
  | Pkg ->
    let package_name, variable_name =
      Pform.Macro_invocation.Args.lsplit2_exn macro_invocation loc
    in
    Ok
      (package_scoped
         (Package_variable_name.of_string variable_name)
         (Package_name.of_string package_name))
  | _ -> Error `Unexpected_macro
;;

let to_macro_invocation { name; scope } =
  match scope with
  | Self ->
    { Pform.Macro_invocation.macro = Pkg_self
    ; payload = Pform.Payload.of_args [ Package_variable_name.to_string name ]
    }
  | Package package_name ->
    { Pform.Macro_invocation.macro = Pkg
    ; payload =
        Pform.Payload.of_args
          [ Package_name.to_string package_name; Package_variable_name.to_string name ]
    }
;;

let to_pform t = Pform.Macro (to_macro_invocation t)
