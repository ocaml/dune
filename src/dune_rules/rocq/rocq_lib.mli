(***********************************************)
(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2019-2024                         *)
(* (c) Emilio J. Gallego Arias 2024-2025       *)
(* (c) CNRS 2025                               *)
(***********************************************)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)
(* Written by: Rodolphe Lepigre                *)
(***********************************************)

open Import

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* Written by: Emilio Jesús Gallego Arias *)

module Dune : sig
  type t

  (** Source directory *)
  val src_root : t -> (Path.Build.t, Path.t list) Either.t

  (** ML libraries *)
  val libraries : t -> (Loc.t * Lib.t) list Resolve.t
end

module Legacy : sig
  type t

  (** For each legacy library, we need:

      - the list of [.vo] files, this is because we need to make the call to
        [rocqdep] depend on it. If due to external action the list of these files
        changes, rocqdep must be re-run. Note that rocqdep sometimes checks for
        [.vo] files and sometimes for [.v] files, which is messy (in principle
        only checks for [.v] files when compiling the stdlib using make, but
        YMMV with rocqdep code).

      In the case of a [Dune.t] lib, this list is obtained from the [src_root],
      via [Dir_contents.rocq], maybe we should move that function here and make
      it common.
  *)

  (** List of vo files *)
  val vo : t -> Path.t list
end

type t =
  | Dune of Dune.t
  | Legacy of Legacy.t

val to_dyn : t -> Dyn.t
val name : t -> Rocq_lib_name.t
val obj_root : t -> Path.t
val implicit : t -> bool

(** Return the list of dependencies needed for compiling this library *)
val theories_closure : t -> t list Resolve.t

module DB : sig
  type lib := t
  type t

  module Entry : sig
    type nonrec t =
      | Redirect of t
      | Theory of Path.Build.t
  end

  (** Note the invariant for the [create_*] functions: DB resolution can't be
      used yet at the stage they are called from [Scope] and the scope build
      process, is not finished. Instead, resolution of dependencies will be done
      in [resolve_*] below and properly memoized. *)
  val create_from_rocqlib_stanzas
    :  parent:t option
    -> find_db:(Path.Build.t -> Lib.DB.t)
    -> (Rocq_stanza.Theory.t * Entry.t) list
    -> t

  (** Note that at some point we could fuse this with the above by having a
      common type for [Rocq_path.t] and [Rocq_stanza.Theory.t]. In this case, when
      libraries are installed, we would infer the right amount of information. *)
  val create_from_rocqpaths : Rocq_path.t list -> t

  val find_many
    :  t
    -> (Loc.t * Rocq_lib_name.t) list
    -> db:Lib.DB.t
    -> lib list Resolve.Memo.t

  val resolve_boot : db:Lib.DB.t -> t -> (Loc.t * lib) option Resolve.Memo.t
  val resolve : db:Lib.DB.t -> t -> Loc.t * Rocq_lib_name.t -> lib Resolve.Memo.t
end
