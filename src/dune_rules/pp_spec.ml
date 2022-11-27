type t = (Module.t -> lint:bool -> Module.t Memo.t) Module_name.Per_item.t

let make x = x

let dummy : t = Module_name.Per_item.for_all (fun m ~lint:_ -> Memo.return m)

let pp_module t ?(lint = true) m =
  Module_name.Per_item.get t (Module.name m) m ~lint

let pp_module_as t ?(lint = true) name m =
  Module_name.Per_item.get t name m ~lint
