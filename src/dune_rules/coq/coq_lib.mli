open Import

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* Written by: Emilio Jesús Gallego Arias *)

type t

val name : t -> Coq_lib_name.t

val implicit : t -> bool

(** ml libraries *)
val libraries : t -> (Loc.t * Lib.t) list Resolve.t

val src_root : t -> Path.Build.t

val obj_root : t -> Path.Build.t

val package : t -> Package.t option

(** Return the list of dependencies needed for compiling this library *)
val theories_closure : t -> t list Resolve.t

module DB : sig
  type lib := t

  type t

  type entry =
    | Theory of Path.Build.t
    | Redirect of t

  val create_from_coqlib_stanzas :
       parent:t option
    -> find_db:(Path.Build.t -> Lib.DB.t)
    -> (Coq_stanza.Theory.t * entry) list
    -> t

  val find_many :
       t
    -> (Loc.t * Coq_lib_name.t) list
    -> coq_lang_version:Dune_sexp.Syntax.Version.t
    -> lib list Resolve.Memo.t

  val resolve_boot :
       t
    -> coq_lang_version:Dune_sexp.Syntax.Version.t
    -> (Loc.t * lib) option Resolve.Memo.t

  val resolve :
       t
    -> coq_lang_version:Dune_sexp.Syntax.Version.t
    -> Loc.t * Coq_lib_name.t
    -> lib Resolve.Memo.t

  val requires_for_user_written :
       t
    -> (Loc.t * Coq_lib_name.t) list
    -> coq_lang_version:Dune_sexp.Syntax.Version.t
    -> lib list Resolve.Memo.t
end
