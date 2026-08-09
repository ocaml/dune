open Stdune
open Core

type 'a t =
  { writers : ('a * unit k) Queue.t
  ; readers : 'a k Queue.t
  ; mutable value : 'a option
  }

(* Invariant enforced on mvars. We don't actually call this function, but we
   keep it here for documentation and to help understand the implementation: *)
let _invariant t =
  match t.value with
  | None -> Queue.is_empty t.writers
  | Some _ -> Queue.is_empty t.readers
;;

let create () = { value = None; writers = Queue.create (); readers = Queue.create () }

let create_full x =
  { value = Some x; writers = Queue.create (); readers = Queue.create () }
;;

let run_read t k =
  match t.value with
  | None -> Suspend ((fun k -> Queue.push t.readers k), k)
  | Some v ->
    (match Queue.pop t.writers with
     | None ->
       t.value <- None;
       continue k v
     | Some (v', w) ->
       t.value <- Some v';
       Resume (w, (), Map ((fun () -> v), k)))
;;

let read t = primitive run_read t

let run_write t x k =
  match t.value with
  | Some _ -> Suspend ((fun k -> Queue.push t.writers (x, k)), k)
  | None ->
    (match Queue.pop t.readers with
     | None ->
       t.value <- Some x;
       continue k ()
     | Some r -> Resume (r, x, Map ((fun () -> ()), k)))
;;

let write t x = primitive2 run_write t x
