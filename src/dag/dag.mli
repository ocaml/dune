(** Detection of cycles in dynamic dags *)

open! Stdune

(** Based on the work by:

    Michael A. Bender, Jeremy T. Fineman, Seth Gilbert, and Robert E. Tarjan.
    2015. A New Approach to Incremental Cycle Detection and Related Problems.
    ACM Trans. Algorithms 12, 2, Article 14 (December 2015), 22 pages. DOI:
    https://doi.org/10.1145/2756553 *)

(** Note that this file uses [vendor/incremental-cycles] and has to meet some
    invariants, for more information see incremental-cycles' README *)

module type Value = Dag_intf.Value
module type S = Dag_intf.S

module Make (Value : Value) () : S with type value := Value.t
