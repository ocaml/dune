(** Module used to represent the [(per_xxx ...)] forms

    The main different between this module and a plain [Map] is that the [map]
    operation applies transformations only once per distinct value. *)
open! Dune_engine

open! Stdune

module Make (Key : Map.Key) : Per_item_intf.S with type key = Key.t
