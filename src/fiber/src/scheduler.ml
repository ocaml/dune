open Stdune
open Core

type fill = Fill : 'a ivar * 'a -> fill

module Jobs = struct
  type t =
    | Empty
    | Job : context * 'a continuation * 'a * t -> t
    | Error of context * Exn_with_backtrace.t * t
    | Work of context * work * t
    | Concat : t * t -> t

  let concat a b =
    match a, b with
    | Empty, x | x, Empty -> x
    | _ -> Concat (a, b)
  ;;

  let rec enqueue_readers (readers : (_, [ `Empty ]) ivar_state) x jobs =
    match readers with
    | Empty -> jobs
    | Empty_with_readers (ctx, k, readers) ->
      enqueue_readers readers x (Job (ctx, k, x, jobs))
  ;;

  let fill_ivar ivar x jobs =
    match ivar.state with
    | Full _ -> failwith "Fiber.Ivar.fill"
    | Empty ->
      ivar.state <- Full x;
      jobs
    | Empty_with_readers (ctx, k, readers) ->
      ivar.state <- Full x;
      let jobs = Job (ctx, k, x, jobs) in
      enqueue_readers readers x jobs
  ;;

  let rec exec_fills fills acc =
    match fills with
    | [] -> acc
    | Fill (ivar, x) :: fills ->
      let acc = fill_ivar ivar x acc in
      exec_fills fills acc
  ;;

  let exec_fills fills = exec_fills (List.rev fills) Empty
end

type step' =
  | Done of value
  | Stalled

module type Witness = sig
  type t
  type value += X of t
end

type 'a stalled = (module Witness with type t = 'a)

type 'a step =
  | Done of 'a
  | Stalled of 'a stalled

let update_var ctx key f =
  (* CR-someday rgrinberg: If [vars = ctx.vars], we could elide the re-allocation of
     [ctx] here. This doesn't seem important for us at the moment though because all
     existing call sites do change the value of the variable. *)
  let vars = Var_map.update ctx.vars ~f key in
  { ctx with parent = ctx; vars }
;;

let rec loop : Jobs.t -> step' = function
  | Empty -> Stalled
  | Job (ctx, run, x, jobs) -> exec ctx run x jobs
  | Error (ctx, exn, jobs) -> handle_captured_exception ctx exn jobs
  | Work (ctx, work, jobs) -> exec_work ctx work jobs
  | Concat (a, b) -> loop2 a b

and loop2 a b =
  match a with
  | Empty -> loop b
  | Job (ctx, run, x, a) -> exec ctx run x (Jobs.concat a b)
  | Error (ctx, exn, a) -> handle_captured_exception ctx exn (Jobs.concat a b)
  | Work (ctx, work, a) -> exec_work ctx work (Jobs.concat a b)
  | Concat (a1, a2) -> loop2 a1 (Jobs.concat a2 b)

and exec : type a. context -> a continuation -> a -> Jobs.t -> step' =
  fun ctx k x jobs ->
  match k with
  | Function f -> exec_function ctx f x jobs
  | Map (f, k) -> exec_map ctx f x k jobs
  | Map2 (f, g, k) -> exec_map2 ctx f g x k jobs
  | Map3 (f, g, h, k) -> exec_map3 ctx f g h x k jobs
  | Bind (f, k) -> exec_fiber_apply ctx f x k jobs
  | Apply (f, y, k) -> exec_fiber_apply2 ctx f x y k jobs
  | Apply_map (f, y, k) -> exec_apply_map ctx f x y k jobs
  | Unwind_to k -> exec ctx.parent k x jobs
  | Unwind_map_reduce_to k -> unwind_map_reduce ctx k (Ok x) jobs
  | End as k -> exec_core_continuation ctx k x jobs
  | Parallel_unit_complete _ as k -> exec_core_continuation ctx k x jobs
  | Map_reduce_complete _ as k -> exec_core_continuation ctx k x jobs
  | Array_map_complete _ as k -> exec_core_continuation ctx k x jobs
  | Fork_join_left _ as k -> exec_core_continuation ctx k x jobs
  | Fork_join_right _ as k -> exec_core_continuation ctx k x jobs
  | Resume_many _ as k -> exec_core_continuation ctx k x jobs
  | Unreachable -> Nothing.unreachable_code x
  | Never_called -> assert false
  | Accumulate_error map_reduce_context ->
    (match map_reduce_context.combine map_reduce_context.errors x with
     | exception exn -> handle_exception ctx exn jobs
     | errors ->
       map_reduce_context.errors <- errors;
       deref map_reduce_context jobs)
  | Collect_errors_complete k ->
    let x =
      match x with
      | Ok x -> Ok x
      | Error errors -> Error (Appendable_list.to_list errors)
    in
    exec ctx k x jobs
  | Finalize_body_complete (finally, k) ->
    map_reduce_errors
      ctx
      (module Exns)
      collect_error
      finally
      (Finalize_finally_complete (x, k))
      jobs
  | Finalize_finally_complete (body_result, k) ->
    (match finalize_result body_result x with
     | Ok x -> exec ctx k x jobs
     | Error errors -> reraise_all ctx (Appendable_list.to_list errors) jobs)

and exec_core_continuation : 'a. context -> 'a continuation -> 'a -> Jobs.t -> step' =
  fun ctx k x jobs ->
  match continue k x with
  | exception exn -> handle_exception ctx exn jobs
  | eff -> exec_effect ctx eff jobs

and exec_function : 'a. context -> ('a -> eff) -> 'a -> Jobs.t -> step' =
  fun ctx f x jobs ->
  match f x with
  | exception exn -> handle_exception ctx exn jobs
  | eff -> exec_effect ctx eff jobs

and exec_map : 'a 'b. context -> ('a -> 'b) -> 'a -> 'b continuation -> Jobs.t -> step' =
  fun ctx f x k jobs ->
  match f x with
  | exception exn -> handle_exception ctx exn jobs
  | y -> exec ctx k y jobs

and exec_map2
  :  'a 'b 'c.
     context
  -> ('a -> 'b)
  -> ('b -> 'c)
  -> 'a
  -> 'c continuation
  -> Jobs.t
  -> step'
  =
  fun ctx f g x k jobs ->
  match g (f x) with
  | exception exn -> handle_exception ctx exn jobs
  | y -> exec ctx k y jobs

and exec_map3
  :  'a 'b 'c 'd.
     context
  -> ('a -> 'b)
  -> ('b -> 'c)
  -> ('c -> 'd)
  -> 'a
  -> 'd continuation
  -> Jobs.t
  -> step'
  =
  fun ctx f g h x k jobs ->
  match h (g (f x)) with
  | exception exn -> handle_exception ctx exn jobs
  | y -> exec ctx k y jobs

and exec_apply_map
  :  'a 'b 'c.
     context
  -> ('a -> 'b -> 'c)
  -> 'a
  -> 'b
  -> 'c continuation
  -> Jobs.t
  -> step'
  =
  fun ctx f x y k jobs ->
  match f x y with
  | exception exn -> handle_exception ctx exn jobs
  | z -> exec ctx k z jobs

and handle_exception ctx exn jobs =
  let exn = Exn_with_backtrace.capture exn in
  handle_captured_exception ctx exn jobs

and handle_captured_exception ctx exn jobs =
  match ctx.on_error with
  | Top_level_error -> Exn_with_backtrace.reraise exn
  | Handle_error (handler_ctx, f) -> exec_fiber_apply handler_ctx f exn Unreachable jobs
  | Collect_errors ->
    let (Map_reduce_context map_reduce_context) = ctx.map_reduce_context in
    exec_fiber_apply
      map_reduce_context.ctx
      map_reduce_context.on_error
      exn
      (Accumulate_error map_reduce_context)
      jobs

and unwind_map_reduce
  : 'a 'b. context -> ('a, 'b) result continuation -> ('a, 'b) result -> Jobs.t -> step'
  =
  fun ctx k x jobs ->
  let (Map_reduce_context r) = ctx.map_reduce_context in
  let ref_count = r.ref_count - 1 in
  r.ref_count <- ref_count;
  assert (ref_count = 0);
  exec ctx.parent k x jobs

and exec_effect ctx eff jobs =
  match eff with
  | Run (t, k) -> exec_fiber ctx t k jobs
  | Done v -> Done v
  | Toplevel_exception exn -> Exn_with_backtrace.reraise exn
  | Unwind (k, x) -> exec ctx.parent k x jobs
  | Read_ivar (ivar, k) ->
    (match ivar.state with
     | (Empty | Empty_with_readers _) as readers ->
       ivar.state <- Empty_with_readers (ctx, k, readers);
       loop jobs
     | Full x -> exec ctx k x jobs)
  | Fill_ivar (ivar, x, k) ->
    let jobs = Jobs.concat jobs (Jobs.fill_ivar ivar x Empty) in
    exec ctx k () jobs
  | Suspend (f, k) ->
    let k = { ctx; run = k } in
    f k;
    loop jobs
  | Resume (suspended, x, k) ->
    exec ctx k () (Jobs.concat jobs (Job (suspended.ctx, suspended.run, x, Empty)))
  | Get_var (key, k) -> exec ctx k (Var_map.get ctx.vars key) jobs
  | Set_var (key, x, f, k) ->
    let ctx = { ctx with parent = ctx; vars = Var_map.set ctx.vars key x } in
    exec_fiber_thunk ctx f (Unwind_to k) jobs
  | Update_var (key, f, body, k) ->
    let ctx = update_var ctx key f in
    exec_fiber_thunk ctx body (Unwind_to k) jobs
  | Set_var_apply (key, x, f, y, k) ->
    let ctx = { ctx with parent = ctx; vars = Var_map.set ctx.vars key x } in
    exec_fiber_apply ctx f y (Unwind_to k) jobs
  | Update_var_apply (key, f, body, x, k) ->
    let ctx = update_var ctx key f in
    exec_fiber_apply ctx body x (Unwind_to k) jobs
  | With_error_handler (on_error, f, k) -> with_error_handler ctx on_error f k jobs
  | Map_reduce_errors (m, on_error, f, k) -> map_reduce_errors ctx m on_error f k jobs
  | Run_collect_errors (f, k) ->
    map_reduce_errors ctx (module Exns) collect_error f (Collect_errors_complete k) jobs
  | Run_finalize (f, finally, k) ->
    map_reduce_errors
      ctx
      (module Exns)
      collect_error
      f
      (Finalize_body_complete (finally, k))
      jobs
  | Run_finally (finally, body_result, k) ->
    map_reduce_errors
      ctx
      (module Exns)
      collect_error
      finally
      (Finalize_finally_complete (body_result, k))
      jobs
  | End_of_fiber () ->
    let (Map_reduce_context r) = ctx.map_reduce_context in
    deref r jobs
  | Unwind_map_reduce (k, x) ->
    let (Map_reduce_context r) = ctx.map_reduce_context in
    let ref_count = r.ref_count - 1 in
    r.ref_count <- ref_count;
    assert (ref_count = 0);
    exec ctx.parent k x jobs
  | End_of_map_reduce_error_handler map_reduce_context -> deref map_reduce_context jobs
  | Never () -> loop jobs
  | Fork (a, b) ->
    let (Map_reduce_context r) = ctx.map_reduce_context in
    r.ref_count <- r.ref_count + 1;
    exec_effect ctx a (Work (ctx, b, jobs))
  | Reraise exn -> handle_captured_exception ctx exn jobs
  | Reraise_all exns -> reraise_all ctx exns jobs

and exec_work ctx work jobs =
  match run_work work with
  | exception exn -> handle_exception ctx exn jobs
  | eff -> exec_effect ctx eff jobs

and with_error_handler
  :  'a.
     context
  -> (Exn_with_backtrace.t -> Nothing.t t)
  -> (unit -> 'a t)
  -> 'a continuation
  -> Jobs.t
  -> step'
  =
  fun ctx on_error f k jobs ->
  let on_error = Handle_error (ctx, on_error) in
  let ctx = { ctx with parent = ctx; on_error } in
  exec_fiber_thunk ctx f (Unwind_to k) jobs

and reraise_all ctx exns jobs =
  match length_and_rev exns with
  | 0, _ -> loop jobs
  | n, exns ->
    let (Map_reduce_context r) = ctx.map_reduce_context in
    r.ref_count <- r.ref_count + (n - 1);
    let jobs =
      List.fold_left exns ~init:jobs ~f:(fun jobs exn -> Jobs.Error (ctx, exn, jobs))
    in
    loop jobs

and exec_fiber : type a. context -> a t -> a continuation -> Jobs.t -> step' =
  fun ctx t k jobs ->
  match t with
  | Return_t x -> exec ctx k x jobs
  | Never_t -> loop jobs
  | Map_t (t, f) -> exec_fiber ctx t (Map (f, k)) jobs
  | Map2_t (t, f, g) -> exec_fiber ctx t (Map2 (f, g, k)) jobs
  | Map3_t (t, f, g, h) -> exec_fiber ctx t (Map3 (f, g, h, k)) jobs
  | Bind_t (t, f) -> exec_fiber ctx t (Bind (f, k)) jobs
  | Thunk_t f -> exec_fiber_thunk ctx f k jobs
  | Thunk_apply_t (f, x) -> exec_fiber_apply ctx f x k jobs
  | With_error_handler_t (f, on_error) -> with_error_handler ctx on_error f k jobs
  | Map_reduce_errors_t (m, on_error, f) -> map_reduce_errors ctx m on_error f k jobs
  | Collect_errors_t f ->
    map_reduce_errors ctx (module Exns) collect_error f (Collect_errors_complete k) jobs
  | Finalize_t (f, finally) ->
    map_reduce_errors
      ctx
      (module Exns)
      collect_error
      f
      (Finalize_body_complete (finally, k))
      jobs
  | Suspend_t f ->
    let k = { ctx; run = k } in
    f k;
    loop jobs
  | Resume_t (suspended, x) ->
    exec ctx k () (Jobs.concat jobs (Job (suspended.ctx, suspended.run, x, Empty)))
  | Reraise_all_t exns -> reraise_all ctx exns jobs
  | Ivar_read_t ivar ->
    (match ivar.state with
     | (Empty | Empty_with_readers _) as readers ->
       ivar.state <- Empty_with_readers (ctx, k, readers);
       loop jobs
     | Full x -> exec ctx k x jobs)
  | Ivar_fill_t (ivar, x) ->
    let jobs = Jobs.concat jobs (Jobs.fill_ivar ivar x Empty) in
    exec ctx k () jobs
  | Get_var_t key -> exec ctx k (Var_map.get ctx.vars key) jobs
  | Set_var_t (key, x, f) ->
    let ctx = { ctx with parent = ctx; vars = Var_map.set ctx.vars key x } in
    exec_fiber_thunk ctx f (Unwind_to k) jobs
  | Update_var_t (key, f, body) ->
    let ctx = update_var ctx key f in
    exec_fiber_thunk ctx body (Unwind_to k) jobs
  | Get_apply_t (key, f, x) -> exec ctx (Apply (f, x, k)) (Var_map.get ctx.vars key) jobs
  | Get_apply_map_t (key, f, x) ->
    exec ctx (Apply_map (f, x, k)) (Var_map.get ctx.vars key) jobs
  | Set_apply_t (key, value, f, x) ->
    let ctx = { ctx with parent = ctx; vars = Var_map.set ctx.vars key value } in
    exec_fiber_apply ctx f x (Unwind_to k) jobs
  | Update_apply_t (key, f, body, x) ->
    let ctx = update_var ctx key f in
    exec_fiber_apply ctx body x (Unwind_to k) jobs
  | Primitive_t (f, x) -> exec_primitive1 ctx f x k jobs
  | Primitive2_t (f, x, y) -> exec_primitive2 ctx f x y k jobs
  | Primitive3_t (f, x, y, z) -> exec_primitive3 ctx f x y z k jobs
  | Primitive4_t (f, w, x, y, z) -> exec_primitive4 ctx f w x y z k jobs

and exec_primitive1
  :  'a 'b.
     context
  -> ('a -> 'b continuation -> eff)
  -> 'a
  -> 'b continuation
  -> Jobs.t
  -> step'
  =
  fun ctx f x k jobs ->
  match f x k with
  | exception exn -> handle_exception ctx exn jobs
  | eff -> exec_effect ctx eff jobs

and exec_primitive2
  :  'a 'b 'c.
     context
  -> ('a -> 'b -> 'c continuation -> eff)
  -> 'a
  -> 'b
  -> 'c continuation
  -> Jobs.t
  -> step'
  =
  fun ctx f x y k jobs ->
  match f x y k with
  | exception exn -> handle_exception ctx exn jobs
  | eff -> exec_effect ctx eff jobs

and exec_primitive3
  :  'a 'b 'c 'd.
     context
  -> ('a -> 'b -> 'c -> 'd continuation -> eff)
  -> 'a
  -> 'b
  -> 'c
  -> 'd continuation
  -> Jobs.t
  -> step'
  =
  fun ctx f x y z k jobs ->
  match f x y z k with
  | exception exn -> handle_exception ctx exn jobs
  | eff -> exec_effect ctx eff jobs

and exec_primitive4
  :  'a 'b 'c 'd 'e.
     context
  -> ('a -> 'b -> 'c -> 'd -> 'e continuation -> eff)
  -> 'a
  -> 'b
  -> 'c
  -> 'd
  -> 'e continuation
  -> Jobs.t
  -> step'
  =
  fun ctx f w x y z k jobs ->
  match f w x y z k with
  | exception exn -> handle_exception ctx exn jobs
  | eff -> exec_effect ctx eff jobs

and exec_fiber_thunk : 'a. context -> (unit -> 'a t) -> 'a continuation -> Jobs.t -> step'
  =
  fun ctx f k jobs ->
  match f () with
  | exception exn -> handle_exception ctx exn jobs
  | t -> exec_fiber ctx t k jobs

and exec_fiber_apply
  : 'a 'b. context -> ('a -> 'b t) -> 'a -> 'b continuation -> Jobs.t -> step'
  =
  fun ctx f x k jobs ->
  match f x with
  | exception exn -> handle_exception ctx exn jobs
  | t -> exec_fiber ctx t k jobs

and exec_fiber_apply2
  :  'a 'b 'c.
     context
  -> ('a -> 'b -> 'c t)
  -> 'a
  -> 'b
  -> 'c continuation
  -> Jobs.t
  -> step'
  =
  fun ctx f x y k jobs ->
  match f x y with
  | exception exn -> handle_exception ctx exn jobs
  | t -> exec_fiber ctx t k jobs

and deref : 'a 'b. ('a, 'b) map_reduce_context' -> Jobs.t -> step' =
  fun r jobs ->
  let ref_count = r.ref_count - 1 in
  r.ref_count <- ref_count;
  match ref_count with
  | 0 -> exec r.ctx r.k (Error r.errors) jobs
  | _ ->
    assert (ref_count > 0);
    loop jobs

and map_reduce_errors
  : type errors b.
    context
    -> (module Monoid with type t = errors)
    -> (Exn_with_backtrace.t -> errors t)
    -> (unit -> b t)
    -> (b, errors) result continuation
    -> Jobs.t
    -> step'
  =
  fun ctx (module M : Monoid with type t = errors) on_error f k jobs ->
  let map_reduce_context =
    { ctx; k; on_error; combine = M.combine; ref_count = 1; errors = M.empty }
  in
  let ctx =
    { ctx with
      parent = ctx
    ; on_error = Collect_errors
    ; map_reduce_context = Map_reduce_context map_reduce_context
    }
  in
  exec_fiber_thunk ctx f (Unwind_map_reduce_to k) jobs
;;

let repack_step (type a) (module W : Witness with type t = a) (step' : step') =
  match step' with
  | Done (W.X a) -> Done a
  | Done _ ->
    Code_error.raise
      "advance: it's illegal to call advance with a fiber created in a different \
       scheduler"
      []
  | Stalled -> Stalled (module W)
;;

let advance (type a) (module W : Witness with type t = a) fill : a step =
  fill |> Jobs.exec_fills |> loop |> repack_step (module W)
;;

let never_handle_error _ =
  Code_error.raise "Fiber scheduler dummy error handler called" []
;;

let combine_unit () () = ()

let start (type a) (t : a t) =
  let module W = struct
    type t = a
    type value += X of a
  end
  in
  let rec ctx =
    { parent = ctx
    ; on_error = Top_level_error
    ; vars = Var_map.empty
    ; map_reduce_context =
        Map_reduce_context
          { ctx
          ; k = Never_called
          ; on_error = never_handle_error
          ; combine = combine_unit
          ; ref_count = 1
          ; errors = ()
          }
    }
  in
  exec_fiber ctx t (Function (fun x -> Done (W.X x))) Empty |> repack_step (module W)
;;
