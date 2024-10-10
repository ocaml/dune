(** This module ensures that one merlin configuration file is generated for each
    stanza. Each of these configuration files is accompanied by a merlin exist
    file. Each of these files contain a map from every module involved in the
    stanza to a standard merlin configuration. The [Processed.t] type represents
    the Merlin configuration as it will be marshalled to and from the
    configuration files, while [Merlin.t] represents raw information coming from
    the build system. *)

open Import

(** Type of "unprocessed" merlin information *)
type t

module Processed : sig
  (** Type of "processed" merlin information *)
  type t

  val to_dyn : t -> Dyn.t

  module Pp_kind : sig
    type t =
      | Pp
      | Ppx
  end

  type pp_flag

  val pp_kind : pp_flag -> Pp_kind.t
  val pp_args : pp_flag -> string
  val load_file : Path.t -> (t, string) result

  (** [print_file path] reads the configuration at path [path] and print it as a
      s-expression *)
  val print_file : Path.t -> unit

  (** [print_generic_dot_merlin paths] will merge the given configurations and
      print the resulting configuration in dot-merlin syntax. *)
  val print_generic_dot_merlin : Path.t list -> unit

  val get : t -> file:Path.Build.t -> Sexp.t option
end

val make
  :  requires_compile:Lib.t list Resolve.t
  -> requires_hidden:Lib.t list Resolve.t
  -> stdlib_dir:Path.t
  -> flags:Ocaml_flags.t
  -> preprocess:Preprocess.Without_instrumentation.t Preprocess.t Module_name.Per_item.t
  -> libname:Lib_name.Local.t option
  -> modules:Modules.With_vlib.t
  -> obj_dir:Path.Build.t Obj_dir.t
  -> dialects:Dialect.DB.t
  -> ident:Merlin_ident.t
  -> modes:[ `Lib of Lib_mode.Map.Set.t | `Exe | `Melange_emit ]
  -> t

val more_src_dirs : Dir_contents.t -> source_dirs:Path.Source.t list -> Path.Source.t list

(** Add rules for generating the merlin configuration of a specific stanza
    identified by [ident] in a directory *)
val add_rules
  :  Super_context.t
  -> dir:Path.Build.t
  -> more_src_dirs:Path.Source.t list
  -> expander:Expander.t
  -> t
  -> unit Memo.t

val pp_config
  :  t
  -> Context.t
  -> expander:Expander.t
  -> Processed.pp_flag option Module_name.Per_item.t Action_builder.t
