(* $Id: fl_topo.mli,v 1.1 2002/09/22 13:32:34 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

(* The type topo.t is a partially ordered relation. You can add an element
 * by giving all descendents ...
 *)

module type IdentifiedType = 
  sig
    type t
    type id_t
    val id : t -> id_t
  end

exception Inconsistent_ordering

module type S =
  sig
    type key
    type el_t
    type t
    val create : unit -> t
    val add : t -> el_t -> unit
    val let_le : t -> key -> key -> unit
    val find : t -> key -> el_t
    val le_than : t -> key -> key -> bool
    val key : el_t -> key
    val iter_up : (el_t -> unit) -> t -> unit
    val iter_down : (el_t -> unit) -> t -> unit
    val iter_up_at : (el_t -> unit) -> t -> key list -> unit
    val iter_down_at : (el_t -> unit) -> t -> key list -> unit
    val clear : t -> unit
    val replace : t -> key -> el_t -> unit
    val delete : t -> key -> unit
  end

module Make(H: IdentifiedType): 
    (S with type el_t = H.t
        and type key = H.id_t)


(* ======================================================================
 * History:
 * 
 * $Log: fl_topo.mli,v $
 * Revision 1.1  2002/09/22 13:32:34  gerd
 * 	Renamed file from topo.mli to fl_topo.mli to avoid
 * name clashes
 *
 * ======================================================================
 * OLD LOGS FOR topo.mli:
 *
 * Revision 1.1  1999/06/20 19:26:26  gerd
 * 	Major change: Added support for META files. In META files, knowlege
 * about compilation options, and dependencies on other packages can be stored.
 * The "ocamlfind query" subcommand has been extended in order to have a
 * direct interface for that. "ocamlfind ocamlc/ocamlopt/ocamlmktop/ocamlcp"
 * subcommands have been added to simplify the invocation of the compiler.
 *
 * 
 *)
