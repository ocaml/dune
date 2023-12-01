open Stdune

let invalidation_acc = ref Memo.Invalidation.empty

module Memo = struct
  include Memo

  let sample_count =
    (* Count number of samples of all lifted computations, to allow simple
       detection of looping tests executed by [run] *)
    ref 0
  ;;

  let exec build =
    (* not expected to be used in re-entrant way *)
    sample_count := 0;
    Memo.reset !invalidation_acc;
    invalidation_acc := Memo.Invalidation.empty;
    let fiber = Memo.run build in
    Fiber.run fiber ~iter:(fun _ -> failwith "deadlock?")
  ;;

  let memoize t =
    let l = Memo.lazy_ ~cutoff:(fun _ _ -> false) (fun () -> t) in
    Memo.of_thunk (fun () -> Memo.Lazy.force l)
  ;;

  let map2 x y ~f =
    map ~f:(fun (x, y) -> f x y) (Memo.fork_and_join (fun () -> x) (fun () -> y))
  ;;

  let all l = Memo.all_concurrently l
end

let run tenacious = Memo.exec tenacious

module Var = struct
  type 'a t =
    { value : 'a ref
    ; cell : (unit, 'a) Memo.Cell.t
    }

  let create value =
    let value = ref value in
    { value
    ; cell = Memo.lazy_cell ~cutoff:(fun _ _ -> false) (fun () -> Memo.return !value)
    }
  ;;

  let set t v =
    t.value := v;
    invalidation_acc
    := Memo.Invalidation.combine
         !invalidation_acc
         (Memo.Cell.invalidate ~reason:Memo.Invalidation.Reason.Test t.cell)
  ;;

  let read t = Memo.of_thunk (fun () -> Memo.Cell.read t.cell)
  let peek t = !(t.value)
end

let incr v = Var.set v (Var.peek v)

module Case = struct
  (* The first [unit] it to delay the creation of functions until benchmarking
     is ready to run. *)
  type 'a t =
    { create_and_compute : unit -> unit -> 'a
    ; incr_and_recompute : unit -> unit -> 'a
    ; restore_from_cache : unit -> unit -> 'a
    }

  let create (f : unit -> _ Var.t * 'a Memo.t) : 'a t =
    let create_and_compute () () = run (f () |> snd) in
    let incr_and_recompute () =
      let var, build = f () in
      let (_ : 'a) = run build in
      fun () ->
        incr var;
        run build
    in
    let restore_from_cache () =
      let build = f () |> snd in
      let (_ : 'a) = run build in
      fun () -> run build
    in
    { create_and_compute; incr_and_recompute; restore_from_cache }
  ;;
end

let one_bind =
  Case.create (fun () ->
    let v = Var.create 0 in
    ( v
    , List.fold_left
        ~init:(Memo.return 0)
        (List.init 1 ~f:(fun _i -> ()))
        ~f:(fun acc () ->
          Memo.bind acc ~f:(fun acc -> Memo.map (Var.read v) ~f:(fun v -> acc + v))) ))
;;

let%bench_fun "1-bind (create and compute)" = one_bind.create_and_compute ()
let%bench_fun "1-bind (incr and recompute)" = one_bind.incr_and_recompute ()
let%bench_fun "1-bind (restore from cache)" = one_bind.restore_from_cache ()

let twenty_reads =
  Case.create (fun () ->
    let v = Var.create 0 in
    ( v
    , List.fold_left
        ~init:(Memo.return 0)
        (List.init 20 ~f:(fun _i -> ()))
        ~f:(fun acc () ->
          Memo.bind acc ~f:(fun acc -> Memo.map (Var.read v) ~f:(fun v -> acc + v))) ))
;;

let%bench_fun "20-reads (create and compute)" = twenty_reads.create_and_compute ()
let%bench_fun "20-reads (incr and recompute)" = twenty_reads.incr_and_recompute ()
let%bench_fun "20-reads (restore from cache)" = twenty_reads.restore_from_cache ()

let clique =
  Case.create (fun () ->
    let v = Var.create 0 in
    let read_v = Memo.memoize (Var.read v) in
    ( v
    , List.fold_left
        ~init:read_v
        (List.init 30 ~f:(fun _i -> ()))
        ~f:(fun acc () ->
          let node = Memo.memoize acc in
          Memo.map2 node acc ~f:( + )) ))
;;

let%bench_fun "clique (create and compute)" = clique.create_and_compute ()
let%bench_fun "clique (incr and recompute)" = clique.incr_and_recompute ()
let%bench_fun "clique (restore from cache)" = clique.restore_from_cache ()

let bipartite =
  Case.create (fun () ->
    let first_var = Var.create 0 in
    let inputs =
      List.init 30 ~f:(fun i ->
        let v = if i = 0 then first_var else Var.create 0 in
        Memo.memoize (Var.read v))
    in
    let matrix i j = if i = j then 1 else 0 in
    let outputs =
      List.init 30 ~f:(fun i ->
        Memo.memoize
          (Memo.all
             (List.mapi inputs ~f:(fun j x -> Memo.map x ~f:(fun x -> matrix i j * x)))
           |> Memo.map ~f:(List.fold_left ~init:0 ~f:( + ))))
    in
    first_var, Memo.memoize (Memo.all outputs))
;;

let%bench_fun "bipartite (create and compute)" = bipartite.create_and_compute ()
let%bench_fun "bipartite (incr and recompute)" = bipartite.incr_and_recompute ()
let%bench_fun "bipartite (restore from cache)" = bipartite.restore_from_cache ()

let memo_diamonds =
  Case.create (fun () ->
    let v = Var.create 0 in
    ( v
    , List.fold_left
        ~init:(Var.read v)
        (List.init 20 ~f:(fun _i -> ()))
        ~f:(fun acc () ->
          Memo.memoize (Memo.bind acc ~f:(fun x -> Memo.map acc ~f:(fun y -> x + y)))) ))
;;

let%bench_fun "memo diamonds (create and compute)" = memo_diamonds.create_and_compute ()
let%bench_fun "memo diamonds (incr and recompute)" = memo_diamonds.incr_and_recompute ()
let%bench_fun "memo diamonds (restore from cache)" = memo_diamonds.restore_from_cache ()
