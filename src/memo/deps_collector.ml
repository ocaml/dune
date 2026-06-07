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
      let threads = Array.init num_threads ~f:(fun _ -> create ()) in
      let thread_index = ref 0 in
      let run_thread =
        { run =
            (fun ~f ->
              (* The scheduler is cooperative and single-threaded, so reading and bumping
                   [thread_index] is atomic and each thread gets a distinct sub-collector. *)
              let i = !thread_index in
              incr thread_index;
              Fiber.Var.set var (Some threads.(i)) f)
        }
      in
      let* result = Fiber.collect_errors (fun () -> k (Some run_thread)) in
      t
      := Deps.Dynamic.append_par
           !t
           ~threads:
             (List.init num_threads ~f:(fun i -> Deps.Dynamic.to_static !(threads.(i))));
      (match result with
       | Ok res -> Fiber.return res
       | Error exns -> Fiber.reraise_all exns)
;;
