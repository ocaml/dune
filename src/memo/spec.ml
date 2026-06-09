open Import

module Event = struct
  type t =
    | Live
    | Validated
end

(** Memo nodes can have some special features, for example, [cutoff]
    predicates. *)
module Node_kind : sig
  type ('i, 'o) t

  val create
    :  cutoff:('o -> 'o -> bool) option
    -> on_event:('i -> Event.t -> unit) option
    -> ('i, 'o) t

  (** Does the [new_value] differ from the [old_value]? *)
  val output_changed : ('i, 'o) t -> old_value:'o -> new_value:'o -> bool

  (** If this function returns [false], [output_changed] is guaranteed to return
      [true] for any pair of values. *)
  val has_cutoff : _ t -> bool

  (** Notify the node about an event. *)
  val notify : ('i, _) t -> 'i -> Event.t -> unit
end = struct
  (* This is a product of two options, flattened to avoid unnecessary
     indirections. Note that only a small number of [t]s have event tracking. *)
  type ('i, 'o) t =
    | Vanilla
    | With_event_tracker of { on_event : 'i -> Event.t -> unit }
    | With_cutoff of { equal : 'o -> 'o -> bool }
    | With_cutoff_and_event_tracker of
        { equal : 'o -> 'o -> bool
        ; on_event : 'i -> Event.t -> unit
        }

  let create ~cutoff ~on_event =
    match cutoff, on_event with
    | None, None -> Vanilla
    | None, Some on_event -> With_event_tracker { on_event }
    | Some equal, None -> With_cutoff { equal }
    | Some equal, Some on_event -> With_cutoff_and_event_tracker { equal; on_event }
  ;;

  let output_changed t ~old_value ~new_value =
    match t with
    | Vanilla | With_event_tracker _ -> true
    | With_cutoff { equal } | With_cutoff_and_event_tracker { equal; on_event = _ } ->
      not (equal old_value new_value)
  ;;

  let has_cutoff = function
    | Vanilla | With_event_tracker _ -> false
    | With_cutoff _ | With_cutoff_and_event_tracker _ -> true
  ;;

  let notify t input event =
    match t with
    | Vanilla | With_cutoff _ -> ()
    | With_event_tracker { on_event }
    | With_cutoff_and_event_tracker { on_event; equal = _ } -> on_event input event
  ;;
end

type ('i, 'o) t =
  { name : string option
  ; (* If the field [witness] precedes any of the functional values ([input],
         [f], and the closures inside [node_kind]), then polymorphic comparison
         actually works for [Spec.t]s. *)
    witness : 'i Type_eq.Id.t
  ; input : (module Store_intf.Input with type t = 'i)
  ; node_kind : ('i, 'o) Node_kind.t
  ; f : 'i -> 'o Fiber.t
  ; human_readable_description : ('i -> User_message.Style.t Pp.t) option
  }

let create ~name ~input ~human_readable_description ~cutoff ?on_event f =
  let name =
    match name with
    | None when !Memo_debug.track_locations_of_lazy_values ->
      Option.map (Caller_id.get ~skip:[ __FILE__ ]) ~f:(fun loc ->
        sprintf "lazy value created at %s" (Loc.to_file_colon_line loc))
    | _ -> name
  in
  { name
  ; input
  ; node_kind = Node_kind.create ~cutoff ~on_event
  ; witness = Type_eq.Id.create ()
  ; f
  ; human_readable_description
  }
;;

let output_changed t ~old_value ~new_value =
  Node_kind.output_changed t.node_kind ~old_value ~new_value
;;

let has_cutoff t = Node_kind.has_cutoff t.node_kind
let notify t input event = Node_kind.notify t.node_kind input event
