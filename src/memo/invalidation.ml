module Table0 = Table
open! Import

(* This is currently used only for informing the user about the reason for
   restarting a build. *)
module Reason = struct
  type t =
    | Unknown
    | Path_changed of Path.t
    | Event_queue_overflow
    | Upgrade
    | Test
    | Variable_changed of string

  let to_string_hum = function
    | Unknown -> None
    | Path_changed path -> Some (Path.to_string path ^ " changed")
    | Event_queue_overflow -> Some "Event queue overflow; full rebuild required"
    | Upgrade -> Some "Dune upgrader initiated a full rebuild"
    | Test -> Some "Rebuild initiated by an internal testsuite"
    | Variable_changed v -> Some (sprintf "Variable %s changed" v)
  ;;

  let equal a b =
    match a, b with
    | Unknown, Unknown
    | Event_queue_overflow, Event_queue_overflow
    | Upgrade, Upgrade
    | Test, Test -> true
    | Path_changed a, Path_changed b -> Path.equal a b
    | Variable_changed a, Variable_changed b -> String.equal a b
    | ( ( Unknown
        | Path_changed _
        | Event_queue_overflow
        | Upgrade
        | Test
        | Variable_changed _ )
      , _ ) -> false
  ;;
end

module Leaf = struct
  type kind =
    | Invalidate_node : _ Node.Dep_node.t -> kind
    | Clear_cache : ('input, ('input, 'output) Node.Dep_node.t) Store.t -> kind
    | Clear_caches
    | Custom of (unit -> unit)

  type t =
    { kind : kind
    ; reason : Reason.t
    }

  let to_string_hum { reason; _ } = Reason.to_string_hum reason
end

module T = struct
  (* Represented as a tree mainly to get a tail-recursive execution. *)
  type t =
    | Empty
    | Leaf of Leaf.t
    | Combine of t * t

  let empty : t = Empty

  let combine a b =
    match a, b with
    | Empty, x | x, Empty -> x
    | x, y -> Combine (x, y)
  ;;
end

include T
include (Monoid.Make (T) : Monoid.S with type t := t)

let invalidate_store = Store.iter ~f:Node.invalidate

let execute_leaf { Leaf.kind; _ } =
  match kind with
  | Invalidate_node dep_node -> Node.invalidate dep_node
  | Clear_cache store -> invalidate_store store
  | Clear_caches -> Caches.clear ()
  | Custom f -> f ()
;;

let rec execute x xs =
  match x with
  | Empty -> execute_list xs
  | Leaf f ->
    execute_leaf f;
    execute_list xs
  | Combine (x, y) -> execute x (y :: xs)

and execute_list = function
  | [] -> ()
  | x :: xs -> execute x xs
;;

let rec to_list_x_xs x xs acc =
  match x with
  | Empty -> to_list_xs xs acc
  | Leaf f -> to_list_xs xs (f :: acc)
  | Combine (x, y) -> to_list_x_xs x (y :: xs) acc

and to_list_xs xs acc =
  match xs with
  | [] -> acc
  | x :: xs -> to_list_x_xs x xs acc
;;

let to_list t = to_list_x_xs t [] []

let details_hum ?(max_elements = 1) t =
  assert (max_elements > 0);
  let details =
    List.filter_map ~f:Leaf.to_string_hum (to_list t)
    |> String.Set.of_list
    |> String.Set.to_list
  in
  (* CR-someday amokhov: Right now we just take first [max_elements] elements
     from the sorted list, but we could prioritise some reasons over others,
     e.g. if there is a global reset because of [Event_queue_overflow], it may
     be better to ensure that this reason is included. *)
  match List.truncate ~max_length:max_elements details with
  | `Not_truncated [] -> [ "Restarting for an unknown reason, please report it as a bug" ]
  | `Not_truncated details -> details
  | `Truncated truncated_details ->
    let extra_message =
      let remaining_details = List.length details - max_elements in
      let plural =
        match remaining_details > 1 with
        | true -> "s"
        | false -> ""
      in
      sprintf ", and %d more change%s" remaining_details plural
    in
    (match List.destruct_last truncated_details with
     | None -> assert false
     | Some (all_but_last, last) -> all_but_last @ [ last ^ extra_message ])
;;

let changed_paths t =
  List.filter_map (to_list t) ~f:(fun ({ Leaf.reason; _ } : Leaf.t) ->
    match reason with
    | Path_changed path -> Some path
    | Unknown | Event_queue_overflow | Upgrade | Test | Variable_changed _ -> None)
  |> Path.Set.of_list
  |> Path.Set.to_list
;;

let execute x = execute x []

let is_empty = function
  | Empty -> true
  | _ -> false
;;

let clear_caches ~reason = Leaf { kind = Clear_caches; reason }

let invalidate_table ~reason ({ cache; _ } : _ Table0.t) =
  Leaf { kind = Clear_cache cache; reason }
;;

let invalidate_node ~reason (dep_node : _ Node.Dep_node.t) =
  Leaf { kind = Invalidate_node dep_node; reason }
;;

(* For out-of-band caching mechanisms that need to run an arbitrary action when the build
   is invalidated. *)
let custom ~reason ~f = Leaf { kind = Custom f; reason }

let to_reason_list t =
  List.fold_left (to_list t) ~init:[] ~f:(fun acc ({ Leaf.reason; _ } : Leaf.t) ->
    if List.mem acc reason ~equal:Reason.equal then acc else reason :: acc)
  |> List.rev
;;
