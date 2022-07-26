open Stdune
open Core

type fill = Fill : 'a ivar * 'a -> fill

module Jobs = struct
  type t =
    | Empty
    | Job : context * ('a -> eff) * 'a * t -> t
    | Concat : t * t -> t

  let concat a b =
    match (a, b) with
    | Empty, x | x, Empty -> x
    | _ -> Concat (a, b)

  let rec enqueue_readers (readers : (_, [ `Empty ]) ivar_state) x jobs =
    match readers with
    | Empty -> jobs
    | Empty_with_readers (ctx, k, readers) ->
      enqueue_readers readers x (Job (ctx, k, x, jobs))

  let fill_ivar ivar x jobs =
    match ivar.state with
    | Full _ -> failwith "Fiber.Ivar.fill"
    | Empty ->
      ivar.state <- Full x;
      jobs
    | Empty_with_readers _ as readers ->
      ivar.state <- Full x;
      enqueue_readers readers x jobs

  let rec exec_fills fills acc =
    match fills with
    | [] -> acc
    | Fill (ivar, x) :: fills ->
      let acc = fill_ivar ivar x acc in
      exec_fills fills acc

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

let rec loop : Jobs.t -> step' = function
  | Empty -> Stalled
  | Job (ctx, run, x, jobs) -> exec ctx run x jobs
  | Concat (a, b) -> loop2 a b

and loop2 a b =
  match a with
  | Empty -> loop b
  | Job (ctx, run, x, a) -> exec ctx run x (Jobs.concat a b)
  | Concat (a1, a2) -> loop2 a1 (Jobs.concat a2 b)

and exec : 'a. context -> ('a -> eff) -> 'a -> Jobs.t -> step' =
 fun ctx k x jobs ->
  match k x with
  | exception exn ->
    let exn = Exn_with_backtrace.capture exn in
    exec ctx.on_error.ctx ctx.on_error.run exn jobs
  | Done v -> Done v
  | Toplevel_exception exn -> Exn_with_backtrace.reraise exn
  | Unwind (k, x) -> exec ctx.parent k x jobs
  | Read_ivar (ivar, k) -> (
    match ivar.state with
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
    exec ctx k ()
      (Jobs.concat jobs (Job (suspended.ctx, suspended.run, x, Empty)))
  | Get_var (key, k) -> exec ctx k (Univ_map.find ctx.vars key) jobs
  | Set_var (key, x, k) ->
    let ctx = { ctx with parent = ctx; vars = Univ_map.set ctx.vars key x } in
    exec ctx k () jobs
  | Unset_var (key, k) ->
    let ctx = { ctx with parent = ctx; vars = Univ_map.remove ctx.vars key } in
    exec ctx k () jobs
  | With_error_handler (on_error, k) ->
    let on_error =
      { ctx; run = (fun exn -> on_error exn Nothing.unreachable_code) }
    in
    let ctx = { ctx with parent = ctx; on_error } in
    exec ctx k () jobs
  | Map_reduce_errors (m, on_error, f, k) ->
    map_reduce_errors ctx m on_error f k jobs
  | End_of_fiber () ->
    let (Map_reduce_context r) = ctx.map_reduce_context in
    deref r jobs
  | Unwind_map_reduce (k, x) ->
    let (Map_reduce_context r) = ctx.map_reduce_context in
    let ref_count = r.ref_count - 1 in
    r.ref_count <- ref_count;
    assert (ref_count = 0);
    exec ctx.parent k x jobs
  | End_of_map_reduce_error_handler map_reduce_context ->
    deref map_reduce_context jobs
  | Never () -> loop jobs
  | Fork (a, b) ->
    let (Map_reduce_context r) = ctx.map_reduce_context in
    r.ref_count <- r.ref_count + 1;
    exec ctx Fun.id a (Job (ctx, b, (), jobs))
  | Reraise exn ->
    let { ctx; run } = ctx.on_error in
    exec ctx run exn jobs
  | Reraise_all exns -> (
    match length_and_rev exns with
    | 0, _ -> loop jobs
    | n, exns ->
      let (Map_reduce_context r) = ctx.map_reduce_context in
      r.ref_count <- r.ref_count + (n - 1);
      let { ctx; run } = ctx.on_error in
      let jobs =
        List.fold_left exns ~init:jobs ~f:(fun jobs exn ->
            Jobs.Job (ctx, run, exn, jobs))
      in
      loop jobs)

and deref : 'a 'b. ('a, 'b) map_reduce_context' -> Jobs.t -> step' =
 fun r jobs ->
  let ref_count = r.ref_count - 1 in
  r.ref_count <- ref_count;
  match ref_count with
  | 0 -> exec r.k.ctx r.k.run (Error r.errors) jobs
  | _ ->
    assert (ref_count > 0);
    loop jobs

and map_reduce_errors :
    type errors b.
       context
    -> (module Monoid with type t = errors)
    -> (Exn_with_backtrace.t -> errors t)
    -> (unit -> eff)
    -> ((b, errors) result -> eff)
    -> Jobs.t
    -> step' =
 fun ctx (module M : Monoid with type t = errors) on_error f k jobs ->
  let map_reduce_context =
    { k = { ctx; run = k }; ref_count = 1; errors = M.empty }
  in
  let on_error =
    { ctx
    ; run =
        (fun exn ->
          on_error exn (fun m ->
              map_reduce_context.errors <- M.combine map_reduce_context.errors m;
              End_of_map_reduce_error_handler map_reduce_context))
    }
  in
  let ctx =
    { ctx with
      parent = ctx
    ; on_error
    ; map_reduce_context = Map_reduce_context map_reduce_context
    }
  in
  exec ctx f () jobs

let repack_step (type a) (module W : Witness with type t = a) (step' : step') =
  match step' with
  | Done (W.X a) -> Done a
  | Done _ ->
    Code_error.raise
      "advance: it's illegal to call advance with a fiber created in a \
       different scheduler"
      []
  | Stalled -> Stalled (module W)

let advance (type a) (module W : Witness with type t = a) fill : a step =
  fill |> Nonempty_list.to_list |> Jobs.exec_fills |> loop
  |> repack_step (module W)

let start (type a) (t : a t) =
  let module W = struct
    type t = a

    type value += X of a
  end in
  let rec ctx =
    { parent = ctx
    ; on_error = { ctx; run = (fun exn -> Toplevel_exception exn) }
    ; vars = Univ_map.empty
    ; map_reduce_context =
        Map_reduce_context
          { k = { ctx; run = (fun _ -> assert false) }
          ; ref_count = 1
          ; errors = ()
          }
    }
  in
  exec ctx t (fun x -> Done (W.X x)) Empty |> repack_step (module W)
