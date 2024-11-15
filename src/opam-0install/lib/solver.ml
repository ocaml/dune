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

  let pp_short = Input.pp_impl

  let rec partition f = function
    | [] -> [], []
    | x :: xs ->
      let ys, zs = partition f xs in
      (match f x with
       | `Left y -> y :: ys, zs
       | `Right z -> ys, z :: zs)
  ;;

  let pp_rolemap ~verbose reasons =
    let short, long =
      reasons
      |> Solver.Output.RoleMap.bindings
      |> partition (fun (_role, component) ->
        match Diagnostics.Component.selected_impl component with
        | Some impl when Diagnostics.Component.notes component = [] -> `Left impl
        | _ -> `Right component)
    in
    let pp_item = Diagnostics.Component.pp ~verbose in
    Pp.paragraph "Selected: "
    ++ Pp.hovbox (Pp.concat_map ~sep:Pp.space short ~f:pp_short)
    ++ Pp.cut
    ++ Pp.enumerate long ~f:pp_item
  ;;

  let diagnostics_rolemap req =
    Solver.do_solve req ~closest_match:true >>| Option.get >>= Diagnostics.of_result
  ;;

  let diagnostics ?(verbose = false) req =
    let+ diag = diagnostics_rolemap req in
    Pp.paragraph "Can't find all required versions."
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
