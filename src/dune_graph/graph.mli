open Stdune

module File_format : sig
  type t =
    | Gexf
    | Dot
    | Summary

  val conv : (string -> (t, [> `Msg of string ]) result) * (Format.formatter -> t -> unit)
end

module Attribute : sig
  type t =
    | String of string
    | Int of int
    | Float of float
    | Boolean of bool

  val to_dyn : t -> Dyn.t
end

type t

val to_dyn : t -> Dyn.t

(* CR-someday cmoseley: This interface isn't ideal as users will need to supply
   IDs manually, which could mean overwriting nodes or adding edges between
   non-existent nodes. However, it is currently needed for Memo to associate
   dep_nodes with nodes in this graph. It would be nice to refactor it to be
   more safe

   Another improvement would be better types for attributes. Currently, typing
   is only enforced by comparing value types when creating a node and raising if
   an attribute is inconsistent within a graph. It would be nice to enforce this
   somehow using the type system *)

(** An empty graph *)
val empty : t

(** Adds a node to this graph with an ID, attributes, and optionally a label.
    attributes is a map from attribute names to values. This function will fail
    if an attribute is provided with the same name but different kind of value
    to a previously provided attribute in the graph *)
val add_node : ?label:string -> t -> id:int -> attributes:Attribute.t String.Map.t -> t

(** Add an edge between node with src_id and node with dst_id *)
val add_edge : t -> src_id:int -> dst_id:int -> t

(** Whether this graph contains a node with the given id *)
val has_node : t -> id:int -> bool

(** Serializes this graph to a file using the given format *)
val serialize : t -> path:Path.t -> format:File_format.t -> unit

(** Prints this graph to stdout using the given file format *)
val print : t -> format:File_format.t -> unit

module For_tests : sig
  (** opaque_attributes is a set of integers representing ids of attributes
      which should not be printed out in tests, perhaps because they are likely
      to change between runs. For example, measured runtime. These attributes
      will be replaced with the string <opaque> *)
  val print : t -> format:File_format.t -> opaque_attributes:Int.Set.t -> unit
end
