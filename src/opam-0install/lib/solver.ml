module Make (Context : S.CONTEXT) = struct
  open Fiber.O
  open Pp.O
  module Input = Model.Make (Context)

  let version = Input.version
  let package_name = Input.package_name
  let formula = Input.formula

  let requirements ~context pkgs =
    let role =
      match pkgs with
      | [ pkg ] -> Input.role context pkg
      | pkgs ->
        let impl = Input.virtual_impl ~context ~depends:pkgs () in
        Input.virtual_role [ impl ]
    in
    role
  ;;

  module Solver = Zeroinstall_solver.Make (Input)
  module Diagnostics = Zeroinstall_solver.Diagnostics (Solver.Output)

  type t = Context.t
  type selections = Solver.Output.t
  type diagnostics = Input.Role.t (* So we can run another solve *)

  let solve context pkgs =
    let req = requirements ~context pkgs in
    Solver.do_solve ~closest_match:false req
    >>| function
    | Some sels -> Ok sels
    | None -> Error req
  ;;

  let rec partition_three f = function
    | [] -> [], [], []
    | first :: rest ->
      let xs, ys, zs = partition_three f rest in
      (match f first with
       | `Left x -> x :: xs, ys, zs
       | `Middle y -> xs, y :: ys, zs
       | `Right z -> xs, ys, z :: zs)
  ;;

  let pp_rolemap ~verbose reasons =
    let good, bad, unknown =
      reasons
      |> Solver.Output.RoleMap.bindings
      |> partition_three (fun (role, component) ->
        match Diagnostics.Component.selected_impl component with
        | Some impl when Diagnostics.Component.notes component = [] -> `Left impl
        | _ ->
          (match Diagnostics.Component.rejects component with
           | _, `No_candidates -> `Right role
           | _, _ -> `Middle component))
    in
    let pp_bad = Diagnostics.Component.pp ~verbose in
    let pp_unknown role = Pp.box (Solver.Output.Role.pp role) in
    match unknown with
    | [] ->
      Pp.paragraph "Selected candidates: "
      ++ Pp.hovbox (Pp.concat_map ~sep:Pp.space good ~f:Input.pp_impl)
      ++ Pp.cut
      ++ Pp.enumerate bad ~f:pp_bad
    | _ ->
      (* In case of unknown packages, no need to print the full diagnostic list, the problem is simpler. *)
      Pp.hovbox
        (Pp.text "The following packages couldn't be found: "
         ++ Pp.concat_map ~sep:Pp.space unknown ~f:pp_unknown)
  ;;

  let diagnostics_rolemap req =
    Solver.do_solve req ~closest_match:true >>| Option.get >>= Diagnostics.of_result
  ;;

  let diagnostics ?(verbose = false) req =
    let+ diag = diagnostics_rolemap req in
    Pp.paragraph "Couldn't solve the package dependency formula."
    ++ Pp.cut
    ++ Pp.vbox (pp_rolemap ~verbose diag)
  ;;

  let packages_of_result sels =
    sels
    |> Solver.Output.to_map
    |> Solver.Output.RoleMap.to_seq
    |> List.of_seq
    |> List.filter_map (fun (_role, sel) -> Input.version (Solver.Output.unwrap sel))
  ;;
end
