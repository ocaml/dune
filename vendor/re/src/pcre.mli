(** NOTE: Only a subset of the PCRE spec is supported *)

exception Parse_error
exception Not_supported

type regexp = Core.re

type flag =
  [ `CASELESS
  | `MULTILINE
  | `ANCHORED
  | `DOTALL
  ]

type groups = Core.Group.t

(** Result of a {!Pcre.full_split} *)
type split_result =
  | Text of string (** Text part of splitted string *)
  | Delim of string (** Delimiter part of splitted string *)
  | Group of int * string (** Subgroup of matched delimiter (subgroup_nr, subgroup_str) *)
  | NoGroup (** Unmatched subgroup *)

(** [re ~flags s] creates the regexp [s] using the pcre syntax. *)
val re : ?flags:flag list -> string -> Core.t

(** [re ~flags s] compiles the regexp [s] using the pcre syntax. *)
val regexp : ?flags:flag list -> string -> regexp

(** [extract ~rex s] executes [rex] on [s] and returns the matching groups. *)
val extract : rex:regexp -> string -> string array

(** Equivalent to {!Core.exec}. *)
val exec : rex:regexp -> ?pos:int -> string -> groups

(** Equivalent to {!Core.Group.get}. *)
val get_substring : groups -> int -> string

(** Return the names of named groups. *)
val names : regexp -> string array

(** Return the first matched named group, or raise [Not_found]. *)
val get_named_substring : regexp -> string -> groups -> string

(** Equivalent to {!Core.Group.offset}. *)
val get_substring_ofs : groups -> int -> int * int

(** Equivalent to {!Core.execp}. *)
val pmatch : rex:regexp -> string -> bool

val substitute : rex:Core.re -> subst:(string -> string) -> string -> string
val full_split : ?max:int -> rex:regexp -> string -> split_result list
val split : rex:regexp -> string -> string list
val quote : string -> string

(** {2 Deprecated} *)

type substrings = Group.t
