(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Manpages.

    See {!Cmdliner.Manpage}. *)

type section_name = string

type block =
  [ `S of section_name | `P of string | `Pre of string | `I of string * string
  | `Noblank | `Blocks of block list ]

val escape : string -> string
(** [escape s] escapes [s] from the doc language. *)

type title = string * int * string * string * string

type t = title * block list

type xref =
  [ `Main | `Cmd of string | `Tool of string | `Page of string * int ]

(** {1 Standard section names} *)

val s_name : section_name
val s_synopsis : section_name
val s_description : section_name
val s_commands : section_name
val s_arguments : section_name
val s_options : section_name
val s_common_options : section_name
val s_exit_status : section_name
val s_environment : section_name
val s_files : section_name
val s_bugs : section_name
val s_examples : section_name
val s_authors : section_name
val s_see_also : section_name
val s_none : section_name

(** {1 Section maps}

    Used for handling the merging of metadata doc strings. *)

type smap
val smap_of_blocks : block list -> smap
val smap_to_blocks : smap -> block list
val smap_has_section : smap -> sec:section_name -> bool
val smap_append_block : smap -> sec:section_name -> block -> smap
(** [smap_append_block smap sec b] appends [b] at the end of section
    [sec] creating it at the right place if needed. *)

(** {1 Content boilerplate} *)

val s_exit_status_intro : block
val s_environment_intro : block

(** {1 Output} *)

type subst = string -> string option
(** The type for variable substitution functions. *)

type format = [ `Auto | `Pager | `Plain | `Groff ]
val print :
  ?env:(string -> string option) ->
  ?errs:Format.formatter -> ?subst:subst -> format ->
  Format.formatter -> t -> unit

(** {1 Printers and escapes used by Cmdliner module} *)

val subst_vars :
  errs:Format.formatter -> subst:subst -> Buffer.t -> string -> string
(** [subst b ~subst s], using [b], substitutes in [s] variables of the form
    "$(doc)" by their [subst] definition. This leaves escapes and markup
    directives $(markup,…) intact.

    @raise Invalid_argument in case of illegal syntax. *)

val doc_to_plain :
  errs:Format.formatter -> subst:subst -> Buffer.t -> string -> string
(** [doc_to_plain b ~subst s] using [b], substitutes in [s] variables by
    their [subst] definition and renders cmdliner directives to plain
    text.

    Raises Invalid_argument in case of illegal syntax. *)

val doc_to_styled :
  ?buffer:Buffer.t -> errs:Format.formatter -> subst:subst -> string -> string
(** [doc_to_styled] is like {!doc_to_plain} but uses ANSI escapes. *)
