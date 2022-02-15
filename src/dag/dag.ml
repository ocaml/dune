open! Stdune
include Dag_intf

module Make (Value : Value) () : S with type value := Value.t = struct
  (* Raw_graph here should have the same complexity than the assumed interface
     on the incremental_cycles proofs, in particular [get_outgoing] should run
     in constant time. *)
  module Raw_graph = struct
    type mark = int

    type graph = unit

    module Node_map = Map.Make (Int)

    module Id = Id.Make ()

    type node_info =
      { id : Id.t
      ; mutable mark : mark
      ; mutable level : int
      ; mutable deps : node list
      ; mutable rev_deps : node list
      ; mutable parent : node option
      }

    and node =
      { data : Value.t
      ; info : node_info
      }

    type vertex = node

    let node_id { info; _ } = info.id

    let new_mark =
      let fresh_mark = ref 0 in
      fun () ->
        let mark = !fresh_mark in
        incr fresh_mark;
        mark

    let vertex_eq v1 v2 = v1 == v2

    let is_marked _ v m = v.info.mark = m

    let set_mark _ v m = v.info.mark <- m

    let get_level _ v = v.info.level

    let set_level _ v l = v.info.level <- l

    let get_incoming _ v = v.info.rev_deps

    let clear_incoming _ v = v.info.rev_deps <- []

    let add_incoming _ v w = v.info.rev_deps <- w :: v.info.rev_deps

    let get_outgoing _ v = v.info.deps

    let get_parent _ v =
      match v.info.parent with
      | None -> assert false
      | Some v -> v

    let set_parent _ v p = v.info.parent <- Some p

    let raw_add_edge _ v w = v.info.deps <- w :: v.info.deps

    let raw_add_vertex _ _ = ()
  end

  include Raw_graph
  module IC = Incremental_cycles.Make (Raw_graph)

  exception Cycle of node list

  let create_node_info () =
    let id = Id.gen () in
    { id; mark = -1; level = 1; deps = []; rev_deps = []; parent = None }

  (* [add_assuming_missing dag v w] creates an arc going from [v] to [w]. @raise
     Cycle if creating the arc would create a cycle. This assumes that the arc
     does not already exist. *)
  let add_assuming_missing v w =
    match IC.add_edge_or_detect_cycle () v w with
    | IC.EdgeAdded -> ()
    | IC.EdgeCreatesCycle compute_cycle ->
      raise
        (Cycle
           (let path = compute_cycle () in
            assert (List.hd path == w);
            assert (Option.value_exn (List.last path) == v);
            List.rev path))

  let rec pp_depth depth pp_value fmt n =
    if depth >= 20 then Format.fprintf fmt "..."
    else
      Format.fprintf fmt "(%d: k=%d) (%a) [@[%a@]]" (Id.to_int n.info.id)
        n.info.level pp_value n.data
        (pp_depth (depth + 1) pp_value
        |> Format.pp_print_list ~pp_sep:(fun fmt () ->
               Format.fprintf fmt ";@, "))
        n.info.deps

  let pp_node pp_value fmt n = pp_depth 0 pp_value fmt n
end
