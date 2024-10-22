module Make(Monad : S.Monad)(Context : S.CONTEXT with type 'a monad = 'a Monad.t) = struct
  open Monad.O

  type 'a monad = 'a Monad.t

  module Input = Model.Make(Monad)(Context)

  let version = Input.version
  let package_name = Input.package_name
  let formula = Input.formula

  let requirements ~context pkgs =
    let role =
      match pkgs with
      | [pkg] -> Input.role context pkg
      | pkgs ->
        let impl = Input.virtual_impl ~context ~depends:pkgs () in
        Input.virtual_role [impl]
    in
    { Input.role; command = None }

  module Solver = Zeroinstall_solver.Make(Monad)(Input)
  module Diagnostics = Zeroinstall_solver.Diagnostics(Monad)(Solver.Output)

  type t = Context.t
  type selections = Solver.Output.t
  type diagnostics = Input.requirements   (* So we can run another solve *)

  let solve context pkgs =
    let req = requirements ~context pkgs in
    Solver.do_solve ~closest_match:false req >>| function
    | Some sels -> Ok sels
    | None -> Error req

  let pp_short f (impl : Input.impl) =
    Input.pp_impl f impl

  let rec partition f = function
    | [] -> [], []
    | x :: xs ->
      let ys, zs = partition f xs in
      match f x with
      | `Left y  -> (y :: ys), zs
      | `Right z -> ys, (z :: zs)

  let pp_rolemap ~verbose f reasons =
    let short, long =
      reasons
      |> Solver.Output.RoleMap.bindings
      |> partition (fun (_role, component) ->
          match Diagnostics.Component.selected_impl component with
          | Some impl when Diagnostics.Component.notes component = [] -> `Left impl
          | _ -> `Right component
        )
    in
    let pp_item f c = Fmt.pf f "- @[%a@]" (Diagnostics.Component.pp ~verbose) c in
    Fmt.pf f "Selected: @[<hov>%a@]@,%a" (Fmt.(list ~sep:sp) pp_short) short
      (Fmt.(list ~sep:cut) pp_item) long

  let diagnostics_rolemap req =
    Solver.do_solve req ~closest_match:true
    >>| Option.get
    >>= Diagnostics.of_result

  let diagnostics ?(verbose=false) req =
    diagnostics_rolemap req
    >>| Fmt.str "Can't find all required versions.@\n@[<v0>%a@]" (pp_rolemap ~verbose)

  let packages_of_result sels =
    sels
    |> Solver.Output.to_map |> Solver.Output.RoleMap.to_seq |> List.of_seq
    |> List.filter_map (fun (_role, sel) -> Input.version (Solver.Output.unwrap sel))
end
