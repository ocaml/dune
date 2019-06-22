open Stdune

type wrapped =
  { modules : Module.Name_map.t
  ; wrapped_compat : Module.Name_map.t
  ; alias_module : Module.t
  ; lib_interface : Module.t option
  }

type t =
  | Unwrapped of Module.Name_map.t
  | Wrapped of wrapped
  | Impl of impl

and impl =
  { impl : t
  ; vlib : t
  }

let rec lib_interface = function
  | Unwrapped _ -> None
  | Wrapped w -> w.lib_interface
  | Impl { impl = _; vlib } -> lib_interface vlib

let exe m = Unwrapped m

let lib lm =
  let modules = Lib_modules.modules lm in
  match Lib_modules.alias_module lm with
  | None -> Unwrapped modules
  | Some alias_module ->
    let wrapped_compat = Lib_modules.wrapped_compat lm in
    let lib_interface = Lib_modules.lib_interface_module lm in
    Wrapped { modules
            ; wrapped_compat
            ; alias_module
            ; lib_interface
            }

let impl t ~vlib = Impl { impl = t; vlib } (* TODO validation *)

type from = Vlib | Impl_or_lib

let from_impl_or_lib = Option.map ~f:(fun m -> (Impl_or_lib, m))

let rec find_from t name =
  match t with
  | Unwrapped m -> from_impl_or_lib (Module.Name.Map.find m name)
  | Wrapped { modules ; wrapped_compat ; alias_module; lib_interface = _ } ->
    from_impl_or_lib (
      if Module.name alias_module = name then
        Some alias_module
      else
        begin match Module.Name.Map.find modules name with
        | Some _ as m -> m
        | None -> Module.Name.Map.find wrapped_compat name
        end
    )
  | Impl { impl; vlib } ->
    begin match find_from impl name with
    | Some _ as m -> m
    | None ->
      let open Option.O in
      let+ (_, m) = find_from vlib name in
      (Vlib, m)
    end

let find t name = Option.map (find_from t name) ~f:snd

let find_dep t name =
  let open Option.O in
  let* (from, m) = find_from t name in
  let kind = Module.kind m in
  let* m = Option.some_if (kind <> Wrapped_compat) m in
  match from with
  | Impl_or_lib -> Some m
  | Vlib -> Option.some_if (Module.visibility m = Public) m

let singleton m =
  let name = Module.name m in
  Unwrapped (Module.Name.Map.singleton name m)

let rec impl_only = function
  | Unwrapped m -> Module.Name_map.impl_only m
  | Wrapped { modules ; wrapped_compat ; alias_module ; lib_interface = _ } ->
    alias_module
    :: Module.Name_map.impl_only modules
    @ Module.Name.Map.values wrapped_compat
  | Impl { vlib ; impl } ->
    impl_only impl @ impl_only vlib

let rec fold t ~init ~f =
  match t with
  | Unwrapped m -> Module.Name.Map.fold m ~f ~init
  | Wrapped { modules ; wrapped_compat ; alias_module ; lib_interface = _ } ->
    let init = f alias_module init in
    let init = Module.Name.Map.fold modules ~f ~init in
    Module.Name.Map.fold wrapped_compat ~f ~init
  | Impl { vlib; impl } ->
    let init = fold impl ~f ~init in
    fold vlib ~f ~init

let compat_for_exn t m =
  match t with
  | Unwrapped _ -> assert false
  | Wrapped { modules ; _ } ->
    Module.Name.Map.find modules (Module.name m)
    |> Option.value_exn
  | Impl _ ->
    Code_error.raise "wrapped compat not supported for vlib"
      []

let iter t ~f = fold t ~init:() ~f:(fun x () -> f x)

let rec for_alias_exn = function
  | Unwrapped _ -> assert false
  | Wrapped { modules ; lib_interface
            ; alias_module = _; wrapped_compat = _ } ->
    begin match lib_interface with
    | None -> modules
    | Some lib_interface ->
      let name = Module.name lib_interface in
      Module.Name.Map.remove modules name
    end
  | Impl { vlib ; impl } ->
    let impl = for_alias_exn impl in
    let vlib = for_alias_exn vlib in
    Module.Name.Map.merge impl vlib ~f:(fun _ impl vlib ->
      match impl, vlib with
      | None, None -> assert false
      | Some _, _ -> impl
      | _, Some vlib ->
        Option.some_if (Module.visibility vlib = Public) vlib)

let rec main_module_name_exn = function
  | Unwrapped _ -> assert false
  | Wrapped { lib_interface; alias_module; modules = _; wrapped_compat = _ } ->
    begin match lib_interface with
    | Some m -> Module.name m
    | None -> Module.name alias_module
    end
  | Impl { vlib ; impl = _ } -> main_module_name_exn vlib
