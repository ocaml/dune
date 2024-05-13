open Import

type t =
  { vlib : Lib.t
  ; impl : Library.t
  ; vlib_modules : Modules.t
  ; vlib_foreign_objects : Path.t list
  ; impl_cm_kind : Cm_kind.t
  }

let vlib_modules t = t.vlib_modules
let vlib t = t.vlib
let impl t = t.impl
let impl_cm_kind t = t.impl_cm_kind

let impl_modules t m =
  match t with
  | None -> Modules.With_vlib.modules m
  | Some t -> Modules.With_vlib.impl ~vlib:t.vlib_modules m
;;

let make ~vlib ~impl ~vlib_modules ~vlib_foreign_objects =
  let impl_cm_kind =
    let vlib_info = Lib.info vlib in
    let { Lib_mode.Map.ocaml = { byte; native = _ }; melange = _ } =
      Lib_info.modes vlib_info
    in
    Mode.cm_kind (if byte then Byte else Native)
  in
  { impl; impl_cm_kind; vlib; vlib_modules; vlib_foreign_objects }
;;

let vlib_stubs_o_files = function
  | None -> []
  | Some t -> t.vlib_foreign_objects
;;

let vlib_obj_map t = Modules.obj_map t.vlib_modules
