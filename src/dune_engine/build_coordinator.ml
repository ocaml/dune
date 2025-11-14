open Import
open Fiber.O

(* Coordinates multiple concurrent builds. Allows RPC and watch mode builds
   to execute simultaneously while maintaining correctness guarantees. *)

type active_build =
  { context : Build_session.t
  ; completion : unit Fiber.Ivar.t
  }

type t =
  { mutable active_builds : active_build Build_id.Map.t
  ; mutex : Fiber.Mutex.t
  }

let create () = { active_builds = Build_id.Map.empty; mutex = Fiber.Mutex.create () }

let active_build_ids t =
  Fiber.Mutex.with_lock t.mutex ~f:(fun () ->
    Fiber.return (Build_id.Map.keys t.active_builds))
;;

let get_build_context t build_id =
  Fiber.Mutex.with_lock t.mutex ~f:(fun () ->
    Fiber.return
      (match Build_id.Map.find t.active_builds build_id with
       | Some build -> Some build.context
       | None -> None))
;;

let get_all_contexts t =
  Fiber.Mutex.with_lock t.mutex ~f:(fun () ->
    let contexts =
      Build_id.Map.to_list t.active_builds
      |> List.map ~f:(fun (id, build) -> id, build.context)
    in
    Fiber.return contexts)
;;

let get_all_progress t =
  let* contexts = get_all_contexts t in
  Fiber.parallel_map contexts ~f:(fun (id, ctx) ->
    let+ progress = Build_session.get_progress ctx in
    id, progress)
;;

let get_all_errors t =
  let* contexts = get_all_contexts t in
  Fiber.parallel_map contexts ~f:(fun (id, ctx) ->
    let+ errors = Build_session.get_errors ctx in
    id, errors)
;;

let has_active_builds t =
  Fiber.Mutex.with_lock t.mutex ~f:(fun () ->
    Fiber.return (not (Build_id.Map.is_empty t.active_builds)))
;;

let register_build t context =
  Fiber.Mutex.with_lock t.mutex ~f:(fun () ->
    let build_id = Build_session.id context in
    let completion = Fiber.Ivar.create () in
    let active_build = { context; completion } in
    t.active_builds <- Build_id.Map.add_exn t.active_builds build_id active_build;
    Fiber.return completion)
;;

let unregister_build t build_id =
  Fiber.Mutex.with_lock t.mutex ~f:(fun () ->
    let* () =
      match Build_id.Map.find t.active_builds build_id with
      | Some build -> Fiber.Ivar.fill build.completion ()
      | None -> Fiber.return ()
    in
    t.active_builds <- Build_id.Map.remove t.active_builds build_id;
    Fiber.return ())
;;

let submit t ~f =
  let context = Build_session.create () in
  let build_id = Build_session.id context in
  let* _completion = register_build t context in
  Fiber.finalize (fun () -> f context) ~finally:(fun () -> unregister_build t build_id)
;;

let to_dyn t =
  let active_ids = Build_id.Map.keys t.active_builds in
  Dyn.record [ "active_builds", Dyn.list Build_id.to_dyn active_ids ]
;;
