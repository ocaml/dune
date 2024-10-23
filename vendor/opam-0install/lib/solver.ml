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

  let rec partition_three f = function
    | [] -> [], [], []
    | first :: rest ->
      let xs, ys, zs = partition_three f rest in
      match f first with
      | `Left x  -> (x :: xs), ys, zs
      | `Middle y -> xs, (y :: ys), zs
      | `Right z -> xs, ys, (z :: zs)

  let pp_rolemap ~verbose f reasons =
    let short, unknowns, long =
      reasons
      |> Solver.Output.RoleMap.bindings
      |> partition_three (fun (role, component) ->
          match Diagnostics.Component.selected_impl component with
          | Some impl when Diagnostics.Component.notes component = [] -> `Left impl
          | _ -> 
            let _, fail_reason = Diagnostics.Component.rejects component in
            if fail_reason = `No_candidates
            then `Middle (role, component)
            else `Right component)
    in
    let pp_item f c = Fmt.pf f "- @[%a@]" (Diagnostics.Component.pp ~verbose) c in
    let pp_unknown f (role, _) = Fmt.pf f "- @[%a@]" Solver.Output.Role.pp role in
    let pp_normals f short long =
      Fmt.pf
        f
        "Selected: @[<hov>%a@]@,%a"
        (Fmt.(list ~sep:sp) pp_short)
        short
        (Fmt.(list ~sep:cut) pp_item)
        long
    in
    if List.is_empty unknowns
    then pp_normals f short long
    else
      Fmt.pf
        f
        "The following packages couldn't be found: @[<hov>%a@]"
        Fmt.(list ~sep:sp pp_unknown)
        unknowns

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
