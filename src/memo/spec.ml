open Import

module Allow_cutoff = struct
  type 'o t =
    | No
    | Yes of ('o -> 'o -> bool)
end

type ('i, 'o) t =
  { name : string option
  ; (* If the field [witness] precedes any of the functional values ([input]
         and [f]), then polymorphic comparison actually works for [Spec.t]s. *)
    witness : 'i Type_eq.Id.t
  ; input : (module Store_intf.Input with type t = 'i)
  ; allow_cutoff : 'o Allow_cutoff.t
  ; f : 'i -> 'o Fiber.t
  ; human_readable_description : ('i -> User_message.Style.t Pp.t) option
  }

let create ~name ~input ~human_readable_description ~cutoff f =
  let name =
    match name with
    | None when !Debug.track_locations_of_lazy_values ->
      Option.map (Caller_id.get ~skip:[ __FILE__ ]) ~f:(fun loc ->
        sprintf "lazy value created at %s" (Loc.to_file_colon_line loc))
    | _ -> name
  in
  let allow_cutoff =
    match cutoff with
    | None -> Allow_cutoff.No
    | Some equal -> Yes equal
  in
  { name
  ; input
  ; allow_cutoff
  ; witness = Type_eq.Id.create ()
  ; f
  ; human_readable_description
  }
;;
