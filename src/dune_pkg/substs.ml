open Stdune

module Variable = struct
  type t = OpamVariable.t

  let to_dyn v = Dyn.string @@ OpamVariable.to_string v
  let compare a b = Ordering.of_int @@ OpamVariable.compare a b
  let encode v = Dune_lang.atom_or_quoted_string @@ OpamVariable.to_string v
  let of_string = OpamVariable.of_string
end

module Var = struct
  module T = struct
    type t =
      { package : Dune_lang.Package_name.t option
      ; variable : Variable.t
      }

    let compare a b =
      match Option.compare Dune_lang.Package_name.compare a.package b.package with
      | Eq -> Ordering.of_int @@ OpamVariable.compare a.variable b.variable
      | otherwise -> otherwise
    ;;

    let to_dyn { package; variable } =
      Dyn.pair
        (Dyn.option Dune_lang.Package_name.to_dyn)
        Variable.to_dyn
        (package, variable)
    ;;
  end

  include Comparable.Make (T)
  include T
end

module Map = Var.Map

let subst env self ~src ~dst =
  let self' = self |> Dune_lang.Package_name.to_string |> OpamPackage.Name.of_string in
  let env full_variable =
    let variable = OpamVariable.Full.variable full_variable in
    let package =
      OpamVariable.Full.package ~self:self' full_variable
      |> Option.map ~f:(fun package ->
        package |> OpamPackage.Name.to_string |> Dune_lang.Package_name.of_string)
    in
    let key = { Var.T.package; variable } in
    match Map.find env key with
    | Some _ as v -> v
    | None -> Map.find env { Var.T.package = Some self; variable }
  in
  let src = OpamFilename.of_string (Path.to_string src) in
  let dst = OpamFilename.of_string (Path.Build.to_string dst) in
  OpamFilter.expand_interpolations_in_file_full env ~src ~dst
;;
