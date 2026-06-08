open! Import
open Node
open Fiber.O

(* The dependencies discovered by the currently running computation. We keep it in
     fiber-local storage (rather than on the call-stack frame) so that parallel branches
     can each collect into their own sub-collector. *)
type t = Dep_node.packed Deps.Dynamic.t ref

let var : t option Fiber.Var.t = Fiber.Var.create None
let create () : t = ref Deps.Dynamic.empty
let run (t : t) ~f = Fiber.Var.set var (Some t) f
let get (t : t) : Dep_node.packed Deps.t = Deps.Dynamic.to_static !t
let add (t : t) dep_node = t := Deps.Dynamic.append_seq !t ~node:(Dep_node.T dep_node)

(* Add a dependency on [dep_node] to the currently running computation, if there is one. *)
let add_dep_from_caller dep_node =
  Fiber.Var.get var
  >>| function
  | None -> ()
  | Some t -> add t dep_node
;;

(* A way to run a parallel thread while collecting its dependencies separately. *)
type run_thread = { run : 'a. f:(unit -> 'a Fiber.t) -> 'a Fiber.t } [@@unboxed]

(* A pool of large per-thread sub-collector arrays, keyed by power-of-two size, to avoid
   repeated major-heap allocations of the outer thread array in [run_parallel] when the
   parallel fan-out is large. Not thread- or domain-safe. *)
module Pool : sig
  type collector := t
  type t

  val acquire : int -> t
  val get : t -> int -> collector
  val release : t -> unit
end = struct
  type nonrec collector = t

  type t =
    { arr : collector Array.Immutable.t
    ; size : int
    }

  (* [Max_young_wosize] is OCaml's minor-heap allocation limit (in words): arrays at or
     below it are allocated cheaply in the minor heap and reclaimed by the next minor
     collection, so pooling them is pointless. We only pool larger arrays, which would
     otherwise pay a major-heap allocation on every [run_parallel] call. *)
  external max_young_wosize_stub : unit -> int = "dune_memo_max_young_wosize" [@@noalloc]

  let max_young_wosize = max_young_wosize_stub ()

  (* The smallest power of two that is at least [n] (for [n >= 1]). *)
  let ceil_pow2 n =
    let p = ref 1 in
    while !p < n do
      p := !p * 2
    done;
    !p
  ;;

  (* [floor (log2 n)] for [n >= 1]. *)
  let floor_log2 n =
    let n = ref n in
    let r = ref 0 in
    while !n > 1 do
      n := !n / 2;
      incr r
    done;
    !r
  ;;

  (* One queue of recycled arrays per power-of-two size class, indexed by
     [floor_log2 array_size]. *)
  let num_size_classes = 64

  let table =
    Array.Immutable.of_array_unsafe
      (Array.init num_size_classes ~f:(fun _ -> Queue.create ()))
  ;;

  let make ~array_size =
    Array.Immutable.of_array_unsafe (Array.init array_size ~f:(fun _ -> create ()))
  ;;

  let acquire size =
    let array_size = ceil_pow2 size in
    let arr =
      if array_size <= max_young_wosize
      then make ~array_size
      else (
        match Queue.pop (Array.Immutable.get table (floor_log2 array_size)) with
        | None -> make ~array_size
        | Some arr -> arr)
    in
    { arr; size }
  ;;

  let get { arr; size } i =
    if i >= size (* [Array.Immutable.get] checks [i >= 0]. *)
    then
      Code_error.raise
        "Pool.get: index outside active range"
        [ "i", Dyn.int i; "size", Dyn.int size ];
    Array.Immutable.get arr i
  ;;

  let release { arr; size } =
    let array_size = Array.Immutable.length arr in
    if array_size <= max_young_wosize
    then (* Let the minor GC reclaim it. *) ()
    else (
      for i = 0 to size - 1 do
        Array.Immutable.get arr i := Deps.Dynamic.empty
      done;
      Queue.push (Array.Immutable.get table (floor_log2 array_size)) arr)
  ;;
end

(* Run [num_threads] parallel threads, collecting each thread's dependencies into its own
     sub-collector (via fiber-local storage) and recording them as a single parallel section
     in the caller's collector. [k] is called with [None] when [num_threads < 2] or when
     there is no active collector, in which case dependencies are collected sequentially as
     usual.

     Dependencies are recorded even when a thread raises, because some errors are cached; we
     reraise after recording. *)
let run_parallel ~num_threads k =
  if num_threads < 2
  then k None
  else
    Fiber.Var.get var
    >>= function
    | None -> k None
    | Some t ->
      let threads = Pool.acquire num_threads in
      let thread_index = ref 0 in
      let run_thread =
        { run =
            (fun ~f ->
              (* The scheduler is cooperative and single-threaded, so reading and bumping
                   [thread_index] is atomic and each thread gets a distinct sub-collector. *)
              let i = !thread_index in
              incr thread_index;
              Fiber.Var.set var (Some (Pool.get threads i)) f)
        }
      in
      let* result = Fiber.collect_errors (fun () -> k (Some run_thread)) in
      t
      := Deps.Dynamic.append_par_init !t ~num_threads ~f:(fun i ->
           Deps.Dynamic.to_static !(Pool.get threads i));
      Pool.release threads;
      (match result with
       | Ok res -> Fiber.return res
       | Error exns -> Fiber.reraise_all exns)
;;
