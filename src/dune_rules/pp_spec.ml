open Import

type t = (Module.t -> lint:bool -> Module.t Memo.t) Module_reference.Per_item.t

let make x = x
let dummy : t = Module_reference.Per_item.for_all (fun m ~lint:_ -> Memo.return m)
let find t m ~name = Module_reference.Per_item.find t ~path:(Module.logical_path m) ~name
let pp_module t ?(lint = true) m = find t m ~name:(Module.name m) m ~lint
let pp_module_as t ?(lint = true) name m = find t m ~name m ~lint

let pped_modules_map preprocess v =
  let map =
    Module_reference.Per_item.map preprocess ~f:(fun pp ->
      match Preprocess.remove_future_syntax ~for_:Compiler pp v with
      | No_preprocessing -> Module.ml_source
      | Action (_, _) -> fun m -> Module.ml_source (Module.pped m)
      | Pps { loc = _; pps = _; flags = _; staged } ->
        if staged then Module.ml_source else fun m -> Module.pped (Module.ml_source m))
  in
  Staged.stage (fun m ->
    Module_reference.Per_item.find
      map
      ~path:(Module.logical_path m)
      ~name:(Module.name m)
      m)
;;
