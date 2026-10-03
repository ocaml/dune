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

module Resolve_traversals = Module_reference.Per_item.Make_monad_traversals (Resolve.Memo)

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

let active_libraries t ~instrumentation_backend =
  let open Resolve.Memo.O in
  (* Per-module preprocessing copies each instrumentation field into every
     preprocessing specification. The location of the backend name identifies
     the field, so we use it to keep a single copy. *)
  fold t ~init:[] ~f:(fun t init ->
    match t with
    | Preprocess.Pps t ->
      Resolve.Memo.List.fold_left t.pps ~init ~f:(fun acc -> function
        | Preprocess.With_instrumentation.Ordinary _ -> Resolve.Memo.return acc
        | Instrumentation_backend
            { libname = (loc, _) as libname; libraries; deps = _; flags = _ } ->
          if List.exists acc ~f:(fun (loc', _) -> Loc.equal loc loc')
          then Resolve.Memo.return acc
          else
            instrumentation_backend libname
            >>| (function
             | Some _ -> (loc, libraries) :: acc
             | None -> acc))
    | Preprocess.No_preprocessing | Action _ | Future_syntax _ -> Resolve.Memo.return init)
  >>| List.rev_map ~f:snd
  >>| List.concat
;;
