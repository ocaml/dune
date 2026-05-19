(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Core definitions. *)

(** Exit codes. *)
module Exit : sig
  type code = int
  val ok : code
  val some_error : code
  val cli_error : code
  val internal_error : code

  type info
  val info : ?docs:string -> ?doc:string -> ?max:code -> code -> info
  val info_code : info -> code
  val info_codes : info -> code * code
  val info_doc : info -> string
  val info_docs : info  -> string
  val info_order : info -> info -> int
  val defaults : info list
  val doclang_subst :
    subst:Cmdliner_manpage.subst -> info -> Cmdliner_manpage.subst
  (** [doclang_subst ~subst info] adds the substitution of [info] to
      [subst]. *)
end

(** Environment variables. *)
module Env : sig
  type var = string
  type info
  val info : ?deprecated:string -> ?docs:string -> ?doc:string -> var -> info
  val info_var : info -> string
  val info_doc : info -> string
  val info_docs : info -> string
  val info_deprecated : info -> string option
  val doclang_subst :
    subst:Cmdliner_manpage.subst -> info -> Cmdliner_manpage.subst
  (** [doclang_subst ~subst info] adds the substitution of [info] to
      [subst]. *)

  val styled_deprecated :
    errs:Format.formatter -> subst:Cmdliner_manpage.subst -> info -> string

  val styled_doc :
    errs:Format.formatter -> subst:Cmdliner_manpage.subst -> info -> string

  module Set : Set.S with type elt = info
end

(** Argument information. *)
module Arg_info : sig
  type absence =
  | Err  (** an error is reported. *)
  | Val of string Lazy.t (** if <> "", takes the given default value. *)
  | Doc of string
    (** if <> "", a doc string interpreted in the doc markup language. *)
  (** The type for what happens if the argument is absent from the cli. *)

  type opt_kind =
  | Flag (** without value, just a flag. *)
  | Opt  (** with required value. *)
  | Opt_vopt of string (** with optional value, takes given default. *)
  (** The type for optional argument kinds. *)

  type pos_kind
  val pos : rev:bool -> start:int -> len:int option -> pos_kind
  val pos_rev : pos_kind -> bool
  val pos_start : pos_kind -> int
  val pos_len : pos_kind -> int option

  type t
  val make :
    ?deprecated:string -> ?absent:string -> ?docs:string ->
    ?doc_envs:Env.info list -> ?docv:string -> ?doc:string ->
    ?env:Env.info -> string list -> t

  val id : t -> int
  val deprecated : t -> string option
  val absent : t -> absence
  val env : t -> Env.info option
  val doc : t -> string
  val docv : t -> string
  val doc_envs : t -> Env.info list
  val docs : t -> string
  val opt_names : t -> string list (* has dashes *)
  val opt_name_sample : t -> string (* warning must be an opt arg *)
  val opt_kind : t -> opt_kind
  val pos_kind : t -> pos_kind

  val make_req : t -> t
  val make_all_opts : t -> t
  val make_opt : docv:string -> absent:absence -> kind:opt_kind -> t -> t
  val make_opt_all : docv:string -> absent:absence -> kind:opt_kind -> t -> t
  val make_pos : docv:string -> pos:pos_kind -> t -> t
  val make_pos_abs : docv:string -> absent:absence -> pos:pos_kind -> t -> t

  val is_opt : t -> bool
  val is_pos : t -> bool
  val is_req : t -> bool

  val pos_cli_order : t -> t -> int
  val rev_pos_cli_order : t -> t -> int

  val compare : t -> t -> int

  val doclang_subst :
    subst:Cmdliner_manpage.subst -> t -> Cmdliner_manpage.subst
  (** [doclang_subst ~subst info] adds the substitution of [info] to
      [subst]. Note this includes the substitutions for [env] if present. *)

  val styled_deprecated :
    errs:Format.formatter -> subst:Cmdliner_manpage.subst -> t -> string

  val styled_doc :
    errs:Format.formatter -> subst:Cmdliner_manpage.subst -> t -> string

  type 'a conv
  type e_conv = Conv : 'a conv -> e_conv

  module Set : sig
    type arg := t
    type t
    val is_empty : t -> bool
    val empty : t
    val add : arg -> e_conv -> t -> t
    val choose : t -> arg * e_conv
    val partition : (arg -> e_conv -> bool) -> t -> t * t
    val filter : (arg -> e_conv -> bool) -> t -> t
    val iter : (arg -> e_conv -> unit) -> t -> unit
    val singleton : arg -> e_conv -> t
    val fold : (arg -> e_conv -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val elements : t -> arg list
    val union : t -> t -> t
    val find_opt : arg -> t -> e_conv option
  end
end

(** Command information. *)
module Cmd_info : sig
  type t
  val make :
    ?deprecated:string -> ?man_xrefs:Cmdliner_manpage.xref list ->
    ?man:Cmdliner_manpage.block list -> ?envs:Env.info list ->
    ?exits:Exit.info list -> ?sdocs:string -> ?docs:string -> ?doc:string ->
    ?version:string -> string -> t

  val name : t -> string
  val version : t -> string option
  val deprecated : t -> string option
  val doc : t -> string
  val docs : t -> string
  val stdopts_docs : t -> string
  val exits : t -> Exit.info list
  val envs : t -> Env.info list
  val man : t -> Cmdliner_manpage.block list
  val man_xrefs : t -> Cmdliner_manpage.xref list
  val args : t -> Arg_info.Set.t
  val has_args : t -> bool
  val children : t -> t list
  val add_args : t -> Arg_info.Set.t -> t
  val with_children : t -> args:Arg_info.Set.t option -> children:t list -> t
  val styled_deprecated :
    errs:Format.formatter -> subst:Cmdliner_manpage.subst -> t -> string

  val styled_doc :
    errs:Format.formatter -> subst:Cmdliner_manpage.subst -> t -> string
end

(** Untyped command line parses. *)
module Cline : sig
  type arg =
  | O of (int * string * (string option)) list (* (pos, name, value) of opt. *)
  | P of string list (** *)
  (** Unconverted argument data as found on the command line. *)

  type t (* command line, maps arg_infos to arg value. *)
  val empty : t
  val add : Arg_info.t -> arg -> t -> t
  val get_arg : t -> Arg_info.t -> arg
  val get_opt_arg : t -> Arg_info.t -> (int * string * (string option)) list
  val get_pos_arg : t -> Arg_info.t -> string list
  val actual_args : t -> Arg_info.t -> string list
  (** Actual command line arguments from the command line *)

  val fold : (Arg_info.t -> arg -> 'b -> 'b) -> t -> 'b -> 'b

  (** {1:deprecations Deprecations} *)

  type deprecated
  (** The type for deprecation invocations. This include both environment
      variable deprecations and argument deprecations. *)

  val deprecated :
    env:(string -> string option) -> t -> deprecated list
  (** [deprecated ~env cli] are the deprecated invocations that occur
      when parsing [cli]. *)

  val pp_deprecated :
    subst:Cmdliner_manpage.subst -> deprecated Cmdliner_base.Fmt.t
    (** [pp_deprecated] formats deprecations. *)
end

(** Evaluation. *)
module Eval : sig
  type t
  val make :
    ancestors:Cmd_info.t list -> cmd:Cmd_info.t -> subcmds:Cmd_info.t list ->
    env:(string -> string option) -> err_ppf:Format.formatter -> t

  val cmd : t -> Cmd_info.t
  val main : t -> Cmd_info.t
  val ancestors : t -> Cmd_info.t list (* root is last *)
  val subcmds : t -> Cmd_info.t list
  val env_var : t -> string -> string option
  val err_ppf : t -> Format.formatter
  val with_cmd : t -> Cmd_info.t -> t
  val doclang_subst : t -> Cmdliner_manpage.subst
end

(** Terms, typed cli fragment definitions. *)
module Term : sig
  type escape =
  [ `Error of bool * string
  | `Help of Cmdliner_manpage.format * string option ]

  type 'a parser =
    Eval.t -> Cline.t -> ('a, [ `Parse of string | escape ]) result

  type 'a t = Arg_info.Set.t * 'a parser
end

(** Completion strategies *)
module Arg_completion : sig
  type 'a directive =
  | Message of string | String of string * string | Value of 'a * string
  | Files | Dirs | Restart | Raw of string

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
  | Complete : 'ctx Term.t option * ('ctx, 'a) func -> 'a complete

  type 'a t

  val make : ?context:'ctx Term.t -> ('ctx, 'a) func -> 'a t
  val complete : 'a t -> 'a complete
  val complete_none : 'a t
  val complete_files : 'a t
  val complete_dirs : 'a t
  val complete_paths : 'a t
  val complete_restart : 'a t
end

(** Textual OCaml value converters *)
module Arg_conv : sig
  type 'a parser = string -> ('a, string) result
  type 'a fmt = 'a Cmdliner_base.Fmt.t
  type 'a t = 'a Arg_info.conv
  val make :
    ?completion:'a Arg_completion.t -> docv:string -> parser:'a parser ->
    pp:'a fmt -> unit -> 'a t

  val of_conv :
    ?completion:'a Arg_completion.t -> ?docv:string ->
    ?parser:'a parser -> ?pp:'a fmt -> 'a t -> 'a t

  val docv : 'a t -> string
  val parser : 'a t -> 'a parser
  val pp : 'a t -> 'a fmt
  val completion : 'a t -> 'a Arg_completion.t

  val some : ?none:string -> 'a t -> 'a option t
  val some' : ?none:'a -> 'a t -> 'a option t

  val none : 'a t
end

(** Complete instruction. *)
module Complete : sig
  type kind =
  | Opt_value of Arg_info.t
  | Opt_name_or_pos_value of Arg_info.t
  | Opt_name

  type t
  val make : ?after_dashdash:bool -> ?subcmds:bool -> token:string -> kind -> t
  val token : t -> string
  val after_dashdash : t -> bool
  val subcmds : t -> bool
  val kind : t -> kind
  val add_subcmds : t -> t
end
