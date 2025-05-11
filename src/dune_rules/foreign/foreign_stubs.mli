open Import

(** A type of foreign library "stubs", which includes all fields of the
    [Library.t] type except for the [archive_name] field. The type is parsed as
    an optional [foreign_stubs] field of the [library] stanza, or as part of the
    top-level [foreign_library] stanza. *)

(* Foreign sources can depend on a directly specified directory [Dir] or on a
     source directory of a library [Lib]. *)
module Include_dir : sig
  module Without_include : sig
    type t =
      | Dir of String_with_vars.t
      | Lib of Loc.t * Lib_name.t
  end

  type t

  val expand_include
    :  t
    -> expand:(String_with_vars.t -> Value.t Memo.t)
    -> dir:Path.t
    -> Without_include.t list Memo.t
end

type t =
  { loc : Loc.t
  ; languages : Foreign_language.t Nonempty_list.t
  ; names : Ordered_set_lang.t
  ; mode : Mode.Select.t
  ; flags : Ordered_set_lang.Unexpanded.t
  ; include_dirs : Include_dir.t list
  ; extra_deps : Dep_conf.t list
  }

(** Construct foreign library stubs with some fields set to default values. *)
val make
  :  loc:Loc.t
  -> languages:Foreign_language.t Nonempty_list.t
  -> names:Ordered_set_lang.t
  -> flags:Ordered_set_lang.Unexpanded.t
  -> t

val decode_stubs : for_library:bool -> t Dune_lang.Decoder.fields_parser
val decode : t Dune_lang.Decoder.t
val is_mode_dependent : t -> bool
