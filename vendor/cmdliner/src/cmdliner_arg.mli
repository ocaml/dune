(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command line arguments as terms. *)

(* Converters *)

type 'a conv

module Completion : sig
  type 'a directive

  val value : ?doc:string -> 'a -> 'a directive
  val string : ?doc:string -> string -> 'a directive
  val files : 'a directive
  val dirs : 'a directive
  val restart : 'a directive
  val message : string -> 'a directive
  val raw : string -> 'a directive

  type ('ctx, 'a) func =
    'ctx option -> token:string -> ('a directive list, string) result

  type 'a complete =
  | Complete : 'ctx Cmdliner_term.t option * ('ctx, 'a) func -> 'a complete

  type 'a t

  val make : ?context:'ctx Cmdliner_term.t -> ('ctx, 'a) func -> 'a t

  val complete : 'a t -> 'a complete
  val complete_none : 'a t
  val complete_files : 'a t
  val complete_dirs : 'a t
  val complete_paths : 'a t
  val complete_restart : 'a t
end

module Conv : sig
  type 'a parser = string -> ('a, string) result
  type 'a fmt = Format.formatter -> 'a -> unit
  type 'a t = 'a conv
  val make :
    ?completion:'a Completion.t -> docv:string -> parser:'a parser ->
    pp:'a fmt -> unit -> 'a t

  val of_conv :
    ?completion:'a Completion.t -> ?docv:string -> ?parser:'a parser ->
    ?pp:'a fmt -> 'a t -> 'a t

  val docv : 'a conv -> string
  val parser : 'a conv -> 'a parser
  val pp : 'a conv -> 'a fmt
  val completion : 'a t -> 'a Completion.t
end

val some : ?none:string -> 'a conv -> 'a option conv
val some' : ?none:'a -> 'a conv -> 'a option conv

(* Arguments *)

type 'a t = 'a Cmdliner_term.t

type info
val info :
  ?deprecated:string -> ?absent:string -> ?docs:string ->
  ?doc_envs:Cmdliner_def.Env.info list -> ?docv:string -> ?doc:string ->
  ?env:Cmdliner_def.Env.info -> string list -> info

val ( & ) : ('a -> 'b) -> 'a -> 'b

val flag : info -> bool t
val flag_all : info -> bool list t
val vflag : 'a -> ('a * info) list -> 'a t
val vflag_all : 'a list -> ('a * info) list -> 'a list t
val opt : ?vopt:'a -> 'a conv -> 'a -> info -> 'a t
val opt_all : ?vopt:'a -> 'a conv -> 'a list -> info -> 'a list t

val pos : ?rev:bool -> int -> 'a conv -> 'a -> info -> 'a t
val pos_all : 'a conv -> 'a list -> info -> 'a list t
val pos_left : ?rev:bool -> int -> 'a conv -> 'a list -> info -> 'a list t
val pos_right : ?rev:bool -> int -> 'a conv -> 'a list -> info -> 'a list t

(* As terms *)

val value : 'a t -> 'a Cmdliner_term.t
val required : 'a option t -> 'a Cmdliner_term.t
val non_empty : 'a list t -> 'a list Cmdliner_term.t
val last : 'a list t -> 'a Cmdliner_term.t

(* Predefined arguments *)

val man_format : Cmdliner_manpage.format Cmdliner_term.t
val stdopt_version : docs:string -> bool Cmdliner_term.t
val stdopt_help : docs:string -> Cmdliner_manpage.format option Cmdliner_term.t

(* Predifined converters *)

val bool : bool conv
val char : char conv
val int : int conv
val nativeint : nativeint conv
val int32 : int32 conv
val int64 : int64 conv
val float : float conv
val string : string conv
val enum : ?docv:string -> (string * 'a) list -> 'a conv
val path : string conv
val filepath : string conv
val dirpath : string conv
val file : string conv
val dir : string conv
val non_dir_file : string conv
val list : ?sep:char -> 'a conv -> 'a list conv
val array : ?sep:char -> 'a conv -> 'a array conv
val pair : ?sep:char -> 'a conv -> 'b conv -> ('a * 'b) conv
val t2 : ?sep:char -> 'a conv -> 'b conv -> ('a * 'b) conv
val t3 : ?sep:char -> 'a conv ->'b conv -> 'c conv -> ('a * 'b * 'c) conv
val t4 :
  ?sep:char -> 'a conv ->'b conv -> 'c conv -> 'd conv ->
  ('a * 'b * 'c * 'd) conv

val doc_quote : string -> string
val doc_alts : ?quoted:bool -> string list -> string
val doc_alts_enum : ?quoted:bool -> (string * 'a) list -> string

(* Deprecated *)

type 'a printer = Format.formatter -> 'a -> unit
val conv' : ?docv:string -> 'a Conv.parser * 'a Conv.fmt -> 'a conv
val conv :
  ?docv:string -> (string -> ('a, [`Msg of string]) result) * 'a Conv.fmt ->
  'a conv

val conv_parser : 'a conv -> (string -> ('a, [`Msg of string]) result)
val conv_printer : 'a conv -> 'a printer
val conv_docv : 'a conv -> string
val parser_of_kind_of_string :
  kind:string -> (string -> 'a option) ->
  (string -> ('a, [`Msg of string]) result)
