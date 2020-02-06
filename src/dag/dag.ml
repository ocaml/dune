open! Stdune
include Dag_intf

module Make (Value : Value) : S with type value := Value.t = struct

  (* Raw_graph here should have the same complexity than the assumed
     interface on the incremental_cycles proofs, in particular
     [get_outgoing] should run in constant time. *)
  module Raw_graph = struct
    type mark = int

    type t =
      { mutable fresh_id : int
      ; mutable fresh_mark : int
      }

    type graph = t

    module Node_map = Map.Make(Int)

    type node_info =
      { id : int
      ; (* only used for printing *)
        mutable mark : mark
      ; mutable level : int
      ; mutable deps : node list
      ; (* see #2959, we need to implement is_child efficiently *)
        mutable deps_set : unit Node_map.t
      ; mutable rev_deps : node list
      ; mutable parent : node option
      }

    and node =
      { data : Value.t
      ; info : node_info
      }

    type vertex = node

    let new_mark g =
      let m = g.fresh_mark in
      g.fresh_mark <- g.fresh_mark + 1;
      m

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

    let raw_add_edge _ v w =
      v.info.deps <- w :: v.info.deps;
      v.info.deps_set <- Node_map.add_exn v.info.deps_set w.info.id ()

    let raw_add_vertex _ _ = ()
  end

  include Raw_graph
  module IC = Incremental_cycles.Make (Raw_graph)

  exception Cycle of node list

  let create () = { fresh_id = 1; fresh_mark = 0 }

  let create_node_info g =
    let id = g.fresh_id in
    g.fresh_id <- g.fresh_id + 1;
    { id; mark = -1; level = 1; deps = []; deps_set = Node_map.empty; rev_deps = []; parent = None }

  let add g v w =
    match IC.add_edge_or_detect_cycle g v w with
    | IC.EdgeAdded -> ()
    | IC.EdgeCreatesCycle compute_cycle ->
      raise
        (Cycle
           (let path = compute_cycle () in
            assert (List.hd path == w);
            assert (Option.value_exn (List.last path) == v);
            List.rev path @ [ v ]))

  let children node = node.info.deps

  let rec pp_depth depth pp_value fmt n =
    if depth >= 20 then
      Format.fprintf fmt "..."
    else
      Format.fprintf fmt "(%d: k=%d) (%a) [@[%a@]]" n.info.id n.info.level
        pp_value n.data
        ( pp_depth (depth + 1) pp_value
        |> Fmt.list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@, ") )
        n.info.deps

  let pp_node pp_value fmt n = pp_depth 0 pp_value fmt n

  let is_child v w = Node_map.mem v.info.deps_set w.info.id

  let add_idempotent g v w =
    (* if the edge doesn't already exist, we
       add it to the graph; note that the complexity guarantees for
       `Dag.add` don't hold if the edge is already in the graph,
       hence the check , see #2959 for more details and the
       README of the vendored library *)
    if is_child v w then () else add g v w
end
