open Stdune

module File_format = struct
  type t =
    | Gexf
    | Dot
    | Summary

  let conv =
    ( (function
        | "gexf" -> Ok Gexf
        | "dot" -> Ok Dot
        | "summary" -> Ok Summary
        | s -> Error (`Msg (Format.sprintf "%s is not a valid graph format" s)))
    , fun fmt t ->
        Format.pp_print_string
          fmt
          (match t with
           | Gexf -> "gexf"
           | Dot -> "dot"
           | Summary -> "summary") )
  ;;
end

module Attribute = struct
  type t =
    | String of string
    | Int of int
    | Float of float
    | Boolean of bool

  let to_dyn = function
    | String s -> Dyn.Variant ("String", [ Dyn.String s ])
    | Int i -> Dyn.Variant ("Int", [ Dyn.Int i ])
    | Float f -> Dyn.Variant ("Float", [ Dyn.Float f ])
    | Boolean b -> Dyn.Variant ("Boolean", [ Dyn.Bool b ])
  ;;

  let to_string = function
    | String s -> s
    | Int i -> Int.to_string i
    | Float f -> Float.to_string f
    | Boolean b -> Bool.to_string b
  ;;

  let to_kind_string = function
    | String _ -> "string"
    | Int _ -> "int"
    | Float _ -> "float"
    | Boolean _ -> "boolean"
  ;;
end

module Node = struct
  type t =
    { label : string option
    ; attributes : Attribute.t Int.Map.t
    }

  let to_dyn t =
    Dyn.Record
      [ "label", Dyn.Option (Option.map t.label ~f:(fun s -> Dyn.String s))
      ; "attributes", Int.Map.to_dyn Attribute.to_dyn t.attributes
      ]
  ;;
end

module Edge = struct
  module T = struct
    type t =
      { src_id : int
      ; dst_id : int
      }

    let to_dyn t = Dyn.Record [ "src_id", Dyn.Int t.src_id; "dst_id", Dyn.Int t.dst_id ]

    let compare { src_id; dst_id } t =
      let open Ordering.O in
      let= () = Int.compare src_id t.src_id in
      Int.compare dst_id t.dst_id
    ;;
  end

  include T
  include Comparable.Make (T)
end

type t =
  { nodes : Node.t Int.Map.t
  ; edges : Edge.Set.t
  ; attributes : (int * string) String.Map.t
  ; attribute_count : int
  }

let empty =
  { nodes = Int.Map.empty
  ; edges = Edge.Set.empty
  ; attributes = String.Map.empty
  ; attribute_count = 0
  }
;;

let to_dyn t =
  Dyn.Record
    [ "nodes", Int.Map.to_dyn Node.to_dyn t.nodes; "edges", Edge.Set.to_dyn t.edges ]
;;

let add_node ?label t ~id ~attributes =
  (* Map attributes, and possibly create a new entry *)
  let attributes, attribute_count, node_attributes =
    String.Map.foldi
      attributes
      ~init:(t.attributes, t.attribute_count, Int.Map.empty)
      ~f:(fun attr_name value (attributes, attribute_count, node_attributes) ->
        let value_kind = Attribute.to_kind_string value in
        let id, kind, attribute_count =
          match String.Map.find attributes attr_name with
          | None -> attribute_count, value_kind, attribute_count + 1
          | Some (id, kind) ->
            if not (kind = value_kind)
            then
              failwith
                (Printf.sprintf
                   "Attribute %s saw conflicting types %s and %s"
                   attr_name
                   kind
                   value_kind);
            id, kind, attribute_count
        in
        ( String.Map.set attributes attr_name (id, kind)
        , attribute_count
        , Int.Map.set node_attributes id value ))
  in
  { t with
    nodes = Int.Map.set t.nodes id { label; attributes = node_attributes }
  ; attributes
  ; attribute_count
  }
;;

let add_edge t ~src_id ~dst_id =
  { t with edges = Edge.Set.add t.edges { src_id; dst_id } }
;;

let has_node t ~id = Int.Map.mem t.nodes id

let serialize_to_gexf t oc =
  output_string
    oc
    {|<?xml version="1.0" encoding="UTF-8"?>
<gexf xmlns="http://www.gexf.net/1.2draft" version="1.2">
<graph mode="static" defaultedgetype="directed">
<nodes>
|};
  if t.attribute_count > 0
  then (
    output_string oc "<attributes class=\"node\">\n";
    String.Map.iteri t.attributes ~f:(fun name (id, kind) ->
      Printf.fprintf oc "<attribute id=\"%d\" title=\"%s\" type=\"%s\" />\n" id name kind);
    output_string oc "</attributes>\n");
  Int.Map.iteri t.nodes ~f:(fun id node ->
    let label =
      match node.label with
      | None -> ""
      | Some label -> Printf.sprintf {| label="%s"|} label
    in
    if Int.Map.cardinal node.attributes = 0
    then Printf.fprintf oc "<node id=\"%d\"%s />\n" id label
    else (
      Printf.fprintf oc "<node id=\"%d\"%s>\n<attvalues>\n" id label;
      Int.Map.iteri node.attributes ~f:(fun attr_id value ->
        Printf.fprintf
          oc
          "<attvalue for=\"%d\" value=\"%s\" />\n"
          attr_id
          (Attribute.to_string value));
      output_string oc "</attvalues>\n</node>\n"));
  output_string oc "</nodes>\n<edges>\n";
  let _ =
    Edge.Set.fold t.edges ~init:0 ~f:(fun edge id ->
      Printf.fprintf
        oc
        "<edge id=\"%d\" source=\"%d\" target=\"%d\" />\n"
        id
        edge.src_id
        edge.dst_id;
      id + 1)
  in
  output_string oc "</edges>\n</graph>\n</gexf>\n"
;;

let serialize_to_dot t oc =
  output_string oc "strict digraph {\n";
  Edge.Set.iter t.edges ~f:(fun edge ->
    Printf.fprintf oc "n_%d -> n_%d\n" edge.src_id edge.dst_id);
  output_string oc "}\n"
;;

module Aggregated = struct
  type t =
    { count : int
    ; in_degree : int
    ; out_degree : int
    ; attributes : Attribute.t Int.Map.t
    }
end

module String_opt_map = Map.Make (struct
    type t = string option

    let to_dyn t = Dyn.Option (Option.map t ~f:(fun s -> Dyn.String s))
    let compare = Option.compare String.compare
  end)

let serialize_summary t oc =
  let open Aggregated in
  (* CR-someday cmoseley: A memo node is created for each *.all-deps target
     which fills up the summary with noise. This is a hacky fix for it right
     now, it would be better to find something else to aggregate on or to move
     these nodes to a single table since they only have a single entry each *)
  let rename_all_deps label =
    Option.map label ~f:(fun label ->
      if String.is_suffix label ~suffix:".all-deps" then "*.all-deps" else label)
  in
  let by_label =
    Int.Map.fold t.nodes ~init:String_opt_map.empty ~f:(fun node acc ->
      let label = rename_all_deps node.label in
      let attributes =
        Option.value
          ~default:Int.Map.empty
          (Option.map (String_opt_map.find acc label) ~f:(fun agg -> agg.attributes))
      in
      let attributes =
        Int.Map.merge attributes node.attributes ~f:(fun _ old_val new_val ->
          match old_val, new_val with
          | None, new_val -> new_val
          | old_val, None -> old_val
          | Some (Int old_val), Some (Int new_val) -> Some (Int (old_val + new_val))
          | Some (Float old_val), Some (Float new_val) ->
            Some (Float (old_val +. new_val))
          | _, _ -> None)
      in
      String_opt_map.update acc label ~f:(function
        | None -> Some { count = 1; in_degree = 0; out_degree = 0; attributes }
        | Some agg -> Some { agg with count = agg.count + 1; attributes }))
  in
  let by_label =
    Edge.Set.fold t.edges ~init:by_label ~f:(fun edge acc ->
      let get_label id =
        Option.bind (Int.Map.find t.nodes id) ~f:(fun node -> rename_all_deps node.label)
      in
      let src_label = get_label edge.src_id in
      let dst_label = get_label edge.dst_id in
      let acc =
        String_opt_map.update
          acc
          src_label
          ~f:(Option.map ~f:(fun agg -> { agg with out_degree = agg.out_degree + 1 }))
      in
      let acc =
        String_opt_map.update
          acc
          dst_label
          ~f:(Option.map ~f:(fun agg -> { agg with in_degree = agg.in_degree + 1 }))
      in
      acc)
  in
  Printf.fprintf
    oc
    "%14s %14s %14s %14s %14s  %s\n"
    "Count"
    "Edges in"
    "Edges out"
    "Time (s)"
    "Avg time (ms)"
    "Label";
  String_opt_map.to_list by_label
  |> List.sort ~compare:(fun (_, a) (_, b) -> Int.compare b.count a.count)
  |> List.iter ~f:(fun (label, agg) ->
    let label =
      match label with
      | None -> "<unlabeled>"
      | Some label -> Printf.sprintf "\"%s\"" label
    in
    let runtime, avg_runtime =
      Option.bind (String.Map.find t.attributes "runtime") ~f:(fun (id, _) ->
        Option.bind (Int.Map.find agg.attributes id) ~f:(fun value ->
          match value with
          | Float runtime ->
            let avg_ms = 1000. *. runtime /. float_of_int agg.count in
            Some (Printf.sprintf "%.4f" runtime, Printf.sprintf "%.4f" avg_ms)
          | String _ | Int _ | Boolean _ -> None))
      |> Option.value ~default:("", "")
    in
    Printf.fprintf
      oc
      "%14d %14d %14d %14s %14s  %s\n"
      agg.count
      agg.in_degree
      agg.out_degree
      runtime
      avg_runtime
      label);
  Printf.fprintf
    oc
    "nodes: %d   edges: %d\n"
    (Int.Map.cardinal t.nodes)
    (Edge.Set.cardinal t.edges)
;;

let serialize_to_output_channel t oc ~format =
  match (format : File_format.t) with
  | Gexf -> serialize_to_gexf t oc
  | Dot -> serialize_to_dot t oc
  | Summary -> serialize_summary t oc
;;

let serialize t ~path ~format =
  Io.with_file_out path ~f:(serialize_to_output_channel t ~format)
;;

let print t ~format = serialize_to_output_channel t Stdlib.stdout ~format

module For_tests = struct
  let print t ~format ~opaque_attributes =
    let opaque_graph =
      { t with
        nodes =
          Int.Map.map t.nodes ~f:(fun node ->
            { node with
              attributes =
                Int.Map.mapi node.attributes ~f:(fun id attr ->
                  if Int.Set.mem opaque_attributes id
                  then Attribute.String "<opaque>"
                  else attr)
            })
      }
    in
    print opaque_graph ~format
  ;;
end
