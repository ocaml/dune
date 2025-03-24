open Import

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* Written by: Emilio Jesús Gallego Arias *)

module Dune : sig
  type t

  (** Source directory *)
  val src_root : t -> Path.Build.t

  (** ML libraries *)
  val libraries : t -> (Loc.t * Lib.t) list Resolve.t
end

module Legacy : sig
  type t

  (** For each legacy library, we need two pieces of data:

      - the list of [.vo] files, this is because we need to make the call to
        [coqdep] depend on it. If due to external action the list of these files
        changes, coqdep must be re-run. Note that coqdep sometimes checks for
        [.vo] files and sometimes for [.v] files, which is messy (in principle
        only checks for [.v] files when compiling the stdlib using make, but
        YMMV with coqdep code).

      In the case of a [Dune.t] lib, this list is obtained from the [src_root],
      via [Dir_contents.coq], maybe we should move that function here and make
      it common.

      - the list of directories containing [.cmxs] files, so we can add them to
        the loadpath as Coq does for all [user-contrib] *)

  (** List of vo files *)
  val vo : t -> Path.t list

  (** List of directories with cmxs *)
  val cmxs_directories : t -> Path.t list
end

type t =
  | Dune of Dune.t
  | Legacy of Legacy.t

val to_dyn : t -> Dyn.t
val name : t -> Coq_lib_name.t
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
  val create_from_coqlib_stanzas
    :  parent:t option
    -> find_db:(Path.Build.t -> Lib.DB.t)
    -> (Coq_stanza.Theory.t * Entry.t) list
    -> t

  (** Note that at some point we could fuse this with the above by having a
      common type for [Coq_path.t] and [Coq_stanza.Theory.t]. In this case, when
      libraries are installed, we would infer the right amount of information. *)
  val create_from_coqpaths : Coq_path.t list -> t

  val find_many
    :  t
    -> (Loc.t * Coq_lib_name.t) list
    -> coq_lang_version:Dune_sexp.Syntax.Version.t
    -> lib list Resolve.Memo.t

  val resolve_boot
    :  t
    -> coq_lang_version:Dune_sexp.Syntax.Version.t
    -> (Loc.t * lib) option Resolve.Memo.t

  val resolve
    :  t
    -> coq_lang_version:Dune_sexp.Syntax.Version.t
    -> Loc.t * Coq_lib_name.t
    -> lib Resolve.Memo.t
end
