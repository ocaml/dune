open Stdune

module Key = struct
  type t =
    { priority : int
    ; sequence : int
    ; id : int
    }

  let compare a b =
    match Int.compare a.priority b.priority with
    | Lt -> Lt
    | Gt -> Gt
    | Eq ->
      (match Int.compare b.sequence a.sequence with
       | Lt -> Lt
       | Gt -> Gt
       | Eq -> Int.compare a.id b.id)
  ;;

  let to_dyn { priority; sequence; id } =
    Dyn.Record
      [ "priority", Dyn.Int priority; "sequence", Dyn.Int sequence; "id", Dyn.Int id ]
  ;;
end

module Key_map = Map.Make (Key)

module Waiter = struct
  type 'a t =
    { sequence : int
    ; value : 'a
    }
end

module type S = sig
  type 'a t
  type 'a priority

  val create : unit -> 'a t
  val create_priority : ?priority:int -> 'a t -> 'a priority
  val priority : 'a priority -> int
  val set_priority : 'a priority -> int -> unit
  val increase_priority : 'a priority -> unit
  val increase_priority_by : 'a priority -> int -> unit
  val check_priority : 'a t -> 'a priority -> unit
  val push : 'a t -> 'a priority -> 'a -> unit
  val pop : 'a t -> 'a option
  val peek : 'a t -> 'a option
  val max_priority : 'a t -> int option
  val is_empty : 'a t -> bool
  val length : 'a t -> int
end

type 'a t =
  { mutable entries : 'a priority Key_map.t
  ; mutable length : int
  ; mutable next_sequence : int
  ; mutable next_priority_id : int
  }

and 'a priority =
  { owner : 'a t
  ; id : int
  ; mutable value : int
  ; waiters : 'a Waiter.t Queue.t
  ; mutable key : Key.t option
  }

let create () =
  { entries = Key_map.empty; length = 0; next_sequence = 0; next_priority_id = 0 }
;;

let next_counter value ~name =
  if value = Int.max_int
  then Code_error.raise "Fiber.Priority_queue counter overflow" [ name, Dyn.Int value ];
  value + 1
;;

let create_priority ?(priority = 0) owner =
  let id = owner.next_priority_id in
  owner.next_priority_id <- next_counter id ~name:"priority id";
  { owner; id; value = priority; waiters = Queue.create (); key = None }
;;

let priority t = t.value

let key t =
  let { Waiter.sequence; value = _ } = Queue.peek_exn t.waiters in
  { Key.priority = t.value; sequence; id = t.id }
;;

let insert t =
  if Option.is_some t.key || Queue.is_empty t.waiters
  then
    Code_error.raise
      "Fiber.Priority_queue.insert: invalid priority state"
      [ "priority", Dyn.Int t.value
      ; "has key", Dyn.Bool (Option.is_some t.key)
      ; "waiters", Dyn.Int (Queue.length t.waiters)
      ];
  let key = key t in
  t.owner.entries <- Key_map.add_exn t.owner.entries key t;
  t.key <- Some key
;;

let remove t =
  match t.key with
  | None -> ()
  | Some key ->
    t.owner.entries <- Key_map.remove t.owner.entries key;
    t.key <- None
;;

let set_priority t priority =
  if priority <> t.value
  then (
    let is_queued = Option.is_some t.key in
    if is_queued then remove t;
    t.value <- priority;
    if is_queued then insert t)
;;

let increase_priority_by t by =
  if by < 0
  then
    Code_error.raise
      "Fiber.Priority_queue.increase_priority_by: negative increment"
      [ "increment", Dyn.Int by ];
  if by > 0 && t.value < Int.max_int
  then
    set_priority
      t
      (if t.value >= 0 && by > Int.max_int - t.value then Int.max_int else t.value + by)
;;

let increase_priority t = increase_priority_by t 1

let check_priority owner t =
  if not Stdlib.(owner == t.owner)
  then Code_error.raise "Fiber.Priority_queue: priority belongs to another queue" []
;;

let push owner t value =
  check_priority owner t;
  let sequence = owner.next_sequence in
  owner.next_sequence <- next_counter sequence ~name:"sequence";
  let was_empty = Queue.is_empty t.waiters in
  Queue.push t.waiters { Waiter.sequence; value };
  owner.length <- owner.length + 1;
  if was_empty then insert t
;;

let pop t =
  match Key_map.max_binding t.entries with
  | None -> None
  | Some (_, priority) ->
    remove priority;
    let { Waiter.sequence = _; value } = Queue.pop_exn priority.waiters in
    t.length <- t.length - 1;
    if not (Queue.is_empty priority.waiters) then insert priority;
    Some value
;;

let peek t =
  match Key_map.max_binding t.entries with
  | None -> None
  | Some (_, priority) ->
    let { Waiter.sequence = _; value } = Queue.peek_exn priority.waiters in
    Some value
;;

let max_priority t =
  Option.map (Key_map.max_binding t.entries) ~f:(fun (key, _) -> key.Key.priority)
;;

let is_empty t = t.length = 0
let length t = t.length
