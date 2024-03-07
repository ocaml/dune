open! Stdune
include Dag_intf

module Make (Value : Value) () : S with type value := Value.t = struct
  (* Raw_graph here should have the same complexity than the assumed interface
     on the incremental_cycles proofs, in particular [get_outgoing] should run
     in constant time. *)
  module Raw_graph = struct
    type mark = int
    type graph = unit

    module Id = Id.Make ()

    type node =
      { id : Id.t
      ; mutable mark : mark
      ; mutable level : int
      ; mutable deps : node list
      ; mutable rev_deps : node list
      ; mutable parent : node Option.Unboxed.t
      ; value : Value.t
      }

    type vertex = node

    let node_id { id; _ } = id

    let new_mark =
      let fresh_mark = ref 0 in
      fun () ->
        let mark = !fresh_mark in
        incr fresh_mark;
        mark
    ;;

    let vertex_eq v1 v2 = v1 == v2
    let is_marked _ v m = v.mark = m
    let set_mark _ v m = v.mark <- m
    let get_level _ v = v.level
    let set_level _ v l = v.level <- l
    let get_incoming _ v = v.rev_deps
    let clear_incoming _ v = v.rev_deps <- []
    let add_incoming _ v w = v.rev_deps <- w :: v.rev_deps
    let get_outgoing _ v = v.deps
    let get_parent _ v = Option.Unboxed.value_exn v.parent
    let set_parent _ v p = v.parent <- Option.Unboxed.some p
    let raw_add_edge _ v w = v.deps <- w :: v.deps
    let raw_add_vertex _ _ = ()
  end

  include Raw_graph
  module IC = Incremental_cycles.Make (Raw_graph)

  exception Cycle of node list

  let create_node value =
    let id = Id.gen () in
    { id
    ; mark = -1
    ; level = 1
    ; deps = []
    ; rev_deps = []
    ; parent = Option.Unboxed.none
    ; value
    }
  ;;

  let value t = t.value

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
  ;;

  let rec pp_depth depth pp_value fmt n =
    if depth >= 20
    then Format.fprintf fmt "..."
    else
      Format.fprintf
        fmt
        "(%d: k=%d) (%a) [@[%a@]]"
        (Id.to_int n.id)
        n.level
        pp_value
        n.value
        (pp_depth (depth + 1) pp_value
         |> Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@, "))
        n.deps
  ;;

  let pp_node pp_value fmt n = pp_depth 0 pp_value fmt n
end
