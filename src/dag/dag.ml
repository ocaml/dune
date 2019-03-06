open! Stdune

include Dag_intf

module Make(Value : Value) : S with type value := Value.t = struct
  type t = {
    mutable num : int;
    mutable index : int;
    mutable arcs : int;
  }

  type node_info = {
    id : int;
    mutable index : int;
    mutable level : int;
    mutable deps : node list;
    mutable rev_deps : node list;
  }

  and node =
    { data : Value.t
    ; info : node_info
    }

  exception Cycle of node list

  let create () = {
    num = 0;
    index = 0;
    arcs = 0;
  }

  let gen_index (dag : t) =
    dag.index <- dag.index - 1;
    dag.index

  let create_node_info dag =
    dag.num <- dag.num + 1;
    {
      id = dag.num;
      index = gen_index dag;
      level = 1;
      deps = [];
      rev_deps = [];
    }

  let delta dag =
    let n = dag.num in
    let m = dag.arcs in
    min (float m ** (1.0 /. 2.0)) (float n ** (2.0 /. 3.0)) |> int_of_float

  let add dag v w =
    let delta = delta dag in

    dag.arcs <- dag.arcs + 1;

    let marked_ids = ref Int.Set.empty in
    let arcs = ref 0 in
    let f = ref [] in
    let b = ref [] in

    let rec bvisit y acc =
      marked_ids := Int.Set.union !marked_ids (Int.Set.singleton y.info.id);
      if List.exists y.info.rev_deps ~f:(fun x -> btraverse x y (x :: acc)) |> not then begin
        b := List.append !b [y];
        false end
      else
        true
    and btraverse x _y acc =
      if x.info.id = w.info.id then raise (Cycle acc);
      arcs := !arcs + 1;
      if !arcs >= delta then begin
        w.info.level <- v.info.level + 1;
        w.info.rev_deps <- [];
        b := [];
        true
      end else begin
        if Int.Set.mem !marked_ids x.info.id then
          false
        else
          bvisit x acc
      end in

    let rec reconstruct_b_path  y acc x =
      if x.info.id = y.info.id then
        Some (acc)
      else
        List.find_map ~f:(reconstruct_b_path y (x :: acc)) x.info.rev_deps in

    let rec fvisit x acc =
      List.iter x.info.deps ~f:(fun y -> ftraverse x y (y :: acc));
      f := x :: !f
    and ftraverse x y acc =
      if y.info.id = v.info.id || List.exists ~f:(fun n -> n.info.id = y.info.id) !b then begin
        let path = reconstruct_b_path y [] v |> Option.value_exn in
        Cycle (List.rev_append path acc) |> raise
      end;
      if y.info.level < w.info.level then begin
        y.info.level <- w.info.level;
        y.info.rev_deps <- [];
        fvisit y acc
      end;
      if y.info.level = w.info.level then
        y.info.rev_deps <- x :: y.info.rev_deps in

    (* step 1: test order *)
    if (v.info.level < w.info.level || (v.info.level = w.info.level && v.info.index < w.info.index)) |> not then
      begin
        (* step 2 *)
        let step2res = bvisit v [v; w] in

        let acc = [w; v] in
        (* step 3 *)
        if step2res then
          fvisit w acc
        else if w.info.level <> v.info.level then begin
          w.info.level <- v.info.level;
          w.info.rev_deps <- [];
          fvisit w acc
        end;

        (* step 4 *)
        let l = List.rev (List.append !b !f) in
        List.iter ~f:(fun x -> x.info.index <- gen_index dag) l
      end;

    (* step 5 *)
    v.info.deps <- w :: v.info.deps;
    if v.info.level = w.info.level then
      w.info.rev_deps <- v :: w.info.rev_deps

  let children node = node.info.deps

  let rec pp_depth depth pp_value fmt n =
    if depth >= 20 then
      Format.fprintf fmt "..."
    else
      Format.fprintf fmt "(%d: k=%d, i=%d) (%a) [@[%a@]]"
        n.info.id n.info.level n.info.index pp_value n.data
        (pp_depth (depth + 1) pp_value
         |> Fmt.list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@, "))
        n.info.deps

  let pp_node pp_value fmt n =
    pp_depth 0 pp_value fmt n

  let is_child v w =
    v.info.deps |> List.exists ~f:(fun c -> c.info.id = w.info.id)
end
