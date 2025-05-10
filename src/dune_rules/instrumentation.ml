open Import

let filter_map_resolve (t : _ Preprocess.t) ~f =
  let open Resolve.Memo.O in
  match t with
  | Pps t ->
    let+ pps = Resolve.Memo.List.filter_map t.pps ~f in
    let pps, flags = List.split pps in
    if pps = []
    then Preprocess.No_preprocessing
    else Pps { t with pps; flags = t.flags @ List.flatten flags }
  | (No_preprocessing | Action _ | Future_syntax _) as t -> Resolve.Memo.return t
;;

module Resolve_traversals = Module_name.Per_item.Make_monad_traversals (Resolve.Memo)

let fold = Resolve_traversals.fold

let with_instrumentation
      (t : Preprocess.With_instrumentation.t Preprocess.Per_module.t)
      ~instrumentation_backend
  =
  let f = function
    | Preprocess.With_instrumentation.Ordinary libname ->
      Resolve.Memo.return (Some (libname, []))
    | Instrumentation_backend { libname; flags; _ } ->
      Resolve.Memo.map
        (instrumentation_backend libname)
        ~f:(Option.map ~f:(fun backend -> backend, flags))
  in
  Resolve_traversals.map t ~f:(filter_map_resolve ~f)
;;
