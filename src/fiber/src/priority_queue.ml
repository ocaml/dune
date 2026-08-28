open Stdune

module Priority = struct
  type t =
    { primary : int
    ; secondary : int
    ; tertiary : int
    }

  let make ~primary ~secondary ~tertiary = { primary; secondary; tertiary }
  let zero = { primary = 0; secondary = 0; tertiary = 0 }
  let of_int primary = { primary; secondary = 0; tertiary = 0 }

  let compare a b =
    match Int.compare a.primary b.primary with
    | Lt -> Lt
    | Gt -> Gt
    | Eq ->
      (match Int.compare a.secondary b.secondary with
       | Lt -> Lt
       | Gt -> Gt
       | Eq -> Int.compare a.tertiary b.tertiary)
  ;;

  let equal a b = compare a b = Eq

  let to_dyn { primary; secondary; tertiary } =
    Dyn.Record
      [ "primary", Dyn.Int primary
      ; "secondary", Dyn.Int secondary
      ; "tertiary", Dyn.Int tertiary
      ]
  ;;
end

module Enqueue = struct
  type t =
    { sequence : int
    ; random_key : int
    ; attempt_id : int
    }
end

module Key = struct
  type t =
    { priority : Priority.t
    ; order_key : int
    ; id : int
    }

  let compare a b =
    match Priority.compare a.priority b.priority with
    | Lt -> Lt
    | Gt -> Gt
    | Eq ->
      (match Int.compare a.order_key b.order_key with
       | Lt -> Lt
       | Gt -> Gt
       | Eq -> Int.compare b.id a.id)
  ;;

  let to_dyn { priority; order_key; id } =
    Dyn.Record
      [ "priority", Priority.to_dyn priority
      ; "order_key", Dyn.Int order_key
      ; "id", Dyn.Int id
      ]
  ;;
end

module Key_map = Map.Make (Key)

module type S = sig
  type 'a t
  type 'a priority

  val create : unit -> 'a t
  val create_with_order_key : order_key:(Enqueue.t -> int) -> 'a t
  val create_priority : ?priority:int -> 'a t -> 'a priority
  val create_rank : rank:Priority.t -> 'a t -> 'a priority
  val priority : 'a priority -> int
  val rank : 'a priority -> Priority.t
  val set_priority : 'a priority -> int -> unit
  val set_rank : 'a priority -> Priority.t -> unit
  val increase_priority : 'a priority -> unit
  val increase_priority_by : 'a priority -> int -> unit
  val check_priority : 'a t -> 'a priority -> unit
  val push : 'a t -> 'a priority -> 'a -> unit
  val pop : 'a t -> 'a option
  val peek : 'a t -> 'a option
  val max_priority : 'a t -> int option
  val max_rank : 'a t -> Priority.t option
  val is_empty : 'a t -> bool
  val length : 'a t -> int
end

type 'a t =
  { mutable entries : 'a waiter Key_map.t
  ; mutable length : int
  ; mutable next_sequence : int
  ; mutable next_waiter_id : int
  ; order_key : Enqueue.t -> int
  }

and 'a priority =
  { owner : 'a t
  ; mutable value : Priority.t
  ; mutable waiters : 'a waiter Int.Map.t
  }

and 'a waiter =
  { priority : 'a priority
  ; enqueue : Enqueue.t
  ; value : 'a
  ; mutable key : Key.t
  }

let fifo_order_key { Enqueue.sequence; random_key = _; attempt_id = _ } = -sequence

let create_with_order_key ~order_key =
  { entries = Key_map.empty
  ; length = 0
  ; next_sequence = 0
  ; next_waiter_id = 0
  ; order_key
  }
;;

let create () = create_with_order_key ~order_key:fifo_order_key

let next_counter value ~name =
  if value = Int.max_int
  then Code_error.raise "Fiber.Priority_queue counter overflow" [ name, Dyn.Int value ];
  value + 1
;;

let create_rank ~rank owner = { owner; value = rank; waiters = Int.Map.empty }

let create_priority ?(priority = 0) owner =
  create_rank ~rank:(Priority.of_int priority) owner
;;

let priority t = t.value.primary
let rank t = t.value

let insert_waiter owner waiter =
  owner.entries <- Key_map.add_exn owner.entries waiter.key waiter
;;

let remove_waiter owner waiter = owner.entries <- Key_map.remove owner.entries waiter.key

let set_rank t rank =
  if not (Priority.equal rank t.value)
  then (
    let waiters = Int.Map.values t.waiters in
    List.iter waiters ~f:(remove_waiter t.owner);
    t.value <- rank;
    List.iter waiters ~f:(fun waiter ->
      waiter.key <- { waiter.key with priority = rank };
      insert_waiter t.owner waiter))
;;

let set_priority t primary = set_rank t { t.value with primary }

let increase_priority_by t by =
  if by < 0
  then
    Code_error.raise
      "Fiber.Priority_queue.increase_priority_by: negative increment"
      [ "increment", Dyn.Int by ];
  if by > 0 && t.value.primary < Int.max_int
  then
    set_priority
      t
      (if t.value.primary >= 0 && by > Int.max_int - t.value.primary
       then Int.max_int
       else t.value.primary + by)
;;

let increase_priority t = increase_priority_by t 1

let check_priority owner t =
  if not Stdlib.(owner == t.owner)
  then Code_error.raise "Fiber.Priority_queue: priority belongs to another queue" []
;;

let random_key attempt_id =
  let open Int64 in
  let mix x = mul (logxor x (shift_right_logical x 16)) 0x45d9f3bL in
  let x = of_int attempt_id |> mix |> mix in
  logxor x (shift_right_logical x 16) |> logand 0x3fffffffL |> to_int
;;

let push owner priority value =
  check_priority owner priority;
  let sequence = owner.next_sequence in
  owner.next_sequence <- next_counter sequence ~name:"sequence";
  let attempt_id = owner.next_waiter_id in
  owner.next_waiter_id <- next_counter attempt_id ~name:"waiter id";
  let enqueue = { Enqueue.sequence; random_key = random_key attempt_id; attempt_id } in
  let key =
    { Key.priority = priority.value
    ; order_key = owner.order_key enqueue
    ; id = attempt_id
    }
  in
  let waiter = { priority; enqueue; value; key } in
  insert_waiter owner waiter;
  priority.waiters <- Int.Map.set priority.waiters attempt_id waiter;
  owner.length <- owner.length + 1
;;

let remove_selected owner waiter =
  remove_waiter owner waiter;
  let attempt_id = waiter.enqueue.attempt_id in
  waiter.priority.waiters <- Int.Map.remove waiter.priority.waiters attempt_id;
  owner.length <- owner.length - 1
;;

let pop t =
  match Key_map.max_binding t.entries with
  | None -> None
  | Some (_, waiter) ->
    remove_selected t waiter;
    Some waiter.value
;;

let peek t =
  Option.map (Key_map.max_binding t.entries) ~f:(fun (_, waiter) -> waiter.value)
;;

let max_rank t =
  Option.map (Key_map.max_binding t.entries) ~f:(fun (key, _) -> key.Key.priority)
;;

let max_priority t = Option.map (max_rank t) ~f:(fun rank -> rank.Priority.primary)
let is_empty t = t.length = 0
let length t = t.length
