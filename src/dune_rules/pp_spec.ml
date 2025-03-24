open Import

type t = (Module.t -> lint:bool -> Module.t Memo.t) Module_name.Per_item.t

let make x = x
let dummy : t = Module_name.Per_item.for_all (fun m ~lint:_ -> Memo.return m)
let pp_module t ?(lint = true) m = Module_name.Per_item.get t (Module.name m) m ~lint
let pp_module_as t ?(lint = true) name m = Module_name.Per_item.get t name m ~lint

let pped_modules_map preprocess v =
  let map =
    Module_name.Per_item.map preprocess ~f:(fun pp ->
      match Preprocess.remove_future_syntax ~for_:Compiler pp v with
      | No_preprocessing -> Module.ml_source
      | Action (_, _) -> fun m -> Module.ml_source (Module.pped m)
      | Pps { loc = _; pps = _; flags = _; staged } ->
        if staged then Module.ml_source else fun m -> Module.pped (Module.ml_source m))
  in
  Staged.stage (fun m -> Module_name.Per_item.get map (Module.name m) m)
;;
