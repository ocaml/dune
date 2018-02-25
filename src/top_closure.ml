open Import

module type Elt = sig
  type t
  type graph
  type key
  val key : t -> key
  val deps : t -> graph -> t list
end

module Make(Key : Comparable.S)(Elt : Elt with type key := Key.t) = struct
  module Set = Set.Make(Key)

  let top_closure graph elements =
    let visited = ref Set.empty in
    let res = ref [] in
    let rec loop elt ~temporarily_marked =
      let key = Elt.key elt in
      if Set.mem temporarily_marked key then
        Error [elt]
      else if not (Set.mem !visited key) then begin
        visited := Set.add !visited key;
        let temporarily_marked = Set.add temporarily_marked key in
        match iter_elts (Elt.deps elt graph) ~temporarily_marked with
        | Ok () -> res := elt :: !res; Ok ()
        | Error l -> Error (elt :: l)
      end else
        Ok ()
    and iter_elts elts ~temporarily_marked =
      match elts with
      | [] -> Ok ()
      | elt :: elts ->
        match loop elt ~temporarily_marked with
        | Error _ as result -> result
        | Ok () -> iter_elts elts ~temporarily_marked
    in
    match iter_elts elements ~temporarily_marked:Set.empty with
    | Ok () -> Ok (List.rev !res)
    | Error elts -> Error elts
end
