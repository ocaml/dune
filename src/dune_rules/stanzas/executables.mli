open Import

module Link_mode : sig
  type t =
    | Byte_complete
    | Other of
        { mode : Mode_conf.t
        ; kind : Binary_kind.t
        }

  include Dune_lang.Conv.S with type t := t

  val exe : t
  val object_ : t
  val shared_object : t
  val byte : t
  val native : t
  val js : t
  val compare : t -> t -> Ordering.t
  val to_dyn : t -> Dyn.t

  val extension
    :  t
    -> loc:Loc.t
    -> ext_obj:Filename.Extension.t
    -> ext_dll:Filename.Extension.t
    -> string

  module Map : sig
    include Map.S with type key = t

    val default_for_tests : version:int * int -> Loc.t t
    val decode : Loc.t t Dune_lang.Decoder.t
  end
end

type t =
  { names : (Loc.t * string) Nonempty_list.t
  ; link_flags : Link_flags.Spec.t
  ; link_deps : Dep_conf.t list
  ; modes : Loc.t Link_mode.Map.t
  ; optional : bool
  ; buildable : Buildable.t
  ; package : Package.t option
  ; promote : Rule.Promote.t option
  ; install_conf : Install_conf.t option
  ; embed_in_plugin_libraries : (Loc.t * Lib_name.t) list
  ; forbidden_libraries : (Loc.t * Lib_name.t) list
  ; bootstrap_info : string option
  ; enabled_if : Blang.t
  ; dune_version : Dune_lang.Syntax.Version.t
  }

include Stanza.S with type t := t

(** Check if the executables have any foreign stubs or archives. *)
val has_foreign : t -> bool

(** Check if the executables have any c++ foreign stubs. *)
val has_foreign_cxx : t -> bool

val obj_dir : t -> dir:Path.Build.t -> Path.Build.t Obj_dir.t
val single : t Dune_lang.Decoder.t
val multi : t Dune_lang.Decoder.t
