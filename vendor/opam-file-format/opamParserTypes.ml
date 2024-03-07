(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Defines the types for the opam format lexer and parser *)

(** Type definitions used by the legacy and the new full position modules *)

(** Relational operators *)
type relop = [ `Eq  (** [=] *)
             | `Neq (** [!=] *)
             | `Geq (** [>=] *)
             | `Gt  (** [>] *)
             | `Leq (** [<=] *)
             | `Lt  (** [<] *)
             ]

(** Logical operators *)
type logop = [ `And (** [&] *) | `Or (** [|] *) ]

(** Prefix operators *)
type pfxop = [ `Not (** [!] *) | `Defined (** [?] *) ]

type file_name = string

(** Source file positions: [(filename, line, column)] *)
type pos = file_name * int * int

(** Environment variable update operators *)
type env_update_op = Eq       (** [=] *)
                   | PlusEq   (** [+=] *)
                   | EqPlus   (** [=+] *)
                   | ColonEq  (** [:=] *)
                   | EqColon  (** [=:] *)
                   | EqPlusEq (** [=+=] *)

(** [OpamParserTypes] transitional module with full position types *)
module FullPos = struct

  (** Source file positions *)
  type file_name = string

  (** Full position *)
  type pos = {
    filename: file_name;
    start: int * int; (* line, column *)
    stop: int * int;  (* line, column *)
  }

  (** [with_pos] type, used for all units, embedding the element [pelem] ans
      its position [pos] *)
  type 'a with_pos = {
    pelem : 'a;
    pos : pos
  }

  type relop_kind = relop
  type relop = relop_kind with_pos

  type logop_kind = logop
  type logop = logop_kind with_pos

  type pfxop_kind = pfxop
  type pfxop = pfxop_kind with_pos

  type env_update_op_kind = env_update_op
  type env_update_op = env_update_op_kind with_pos

  (** Base values *)
  type value_kind =
    | Bool of bool
    (** [bool] atoms *)
    | Int of int
    (** [int] atoms *)
    | String of string
    (** [string] atoms *)
    | Relop of relop * value * value
    (** Relational operators with two values (e.g. [os != "win32"]) *)
    | Prefix_relop of relop * value
    (** Relational operators in prefix position (e.g. [< "4.07.0"]) *)
    | Logop of logop * value * value
    (** Logical operators *)
    | Pfxop of pfxop * value
    (** Prefix operators *)
    | Ident of string
    (** Identifiers *)
    | List of value list with_pos
    (** Lists of values ([[x1 x2 ... x3]]) *)
    | Group of value list with_pos
    (** Groups of values ([(x1 x2 ... x3)]) *)
    | Option of value * value list with_pos
    (** Value with optional list ([x1 {x2 x3 x4}]) *)
    | Env_binding of value * env_update_op * value
    (** Environment variable binding ([FOO += "bar"]) *)
  and value = value_kind with_pos

  (** An opamfile section *)
  type opamfile_section =
    { section_kind  : string with_pos;             (** Section kind
                                                       (e.g. [extra-source]) *)
      section_name  : string with_pos option;      (** Section name
                                                       (e.g. ["myfork.patch"]) *)
      section_items : opamfile_item list with_pos; (** Content of the section *)
    }

  (** An opamfile is composed of sections and variable definitions *)
  and opamfile_item_kind =
    | Section of opamfile_section         (** e.g. [kind ["name"] { ... }] *)
    | Variable of string with_pos * value (** e.g. [opam-version: "2.0"] *)
  and opamfile_item = opamfile_item_kind with_pos

  (** A file is a list of items and the filename *)
  type opamfile = {
    file_contents: opamfile_item list; (** Content of the file *)
    file_name    : file_name;          (** Name of the disk file this record was
                                           loaded from *)
  }

end

type value =
  | Bool of pos * bool
  | Int of pos * int
  | String of pos * string
  | Relop of pos * relop * value * value
  | Prefix_relop of pos * relop * value
  | Logop of pos * logop * value * value
  | Pfxop of pos * pfxop * value
  | Ident of pos * string
  | List of pos * value list
  | Group of pos * value list
  | Option of pos * value * value list
  | Env_binding of pos * value * env_update_op * value

type opamfile_section = {
  section_kind  : string;
  section_name  : string option;
  section_items : opamfile_item list
}
and opamfile_item =
  | Section of pos * opamfile_section
  | Variable of pos * string * value

type opamfile = {
  file_contents: opamfile_item list;
  file_name    : file_name;
}
