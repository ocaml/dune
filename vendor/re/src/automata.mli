(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

(* Regular expressions *)

module Mark : sig
  type t [@@immediate]

  val compare : t -> t -> int
  val start : t
  val prev : t -> t
  val next : t -> t
  val next2 : t -> t
  val group_count : t -> int
end

module Sem : sig
  type t =
    [ `Longest
    | `Shortest
    | `First
    ]

  val pp : t Fmt.t
end

module Rep_kind : sig
  type t =
    [ `Greedy
    | `Non_greedy
    ]

  val pp : t Fmt.t
end

type expr

val is_eps : expr -> bool
val pp : expr Fmt.t

module Ids : sig
  type t

  val create : unit -> t
end

val cst : Ids.t -> Cset.t -> expr
val empty : Ids.t -> expr
val alt : Ids.t -> expr list -> expr
val seq : Ids.t -> Sem.t -> expr -> expr -> expr
val eps : Ids.t -> expr
val rep : Ids.t -> Rep_kind.t -> Sem.t -> expr -> expr
val mark : Ids.t -> Mark.t -> expr
val pmark : Ids.t -> Pmark.t -> expr
val erase : Ids.t -> Mark.t -> Mark.t -> expr
val before : Ids.t -> Category.t -> expr
val after : Ids.t -> Category.t -> expr
val rename : Ids.t -> expr -> expr

(****)

(* States of the automata *)

module Idx : sig
  type t

  val to_int : t -> int
end

module Status : sig
  type t =
    | Failed
    | Match of Mark_infos.t * Pmark.Set.t
    | Running
end

module State : sig
  type t

  val dummy : t
  val create : Category.t -> expr -> t
  val idx : t -> Idx.t
  val status : t -> Status.t

  module Table : Hashtbl.S with type key = t
end

(****)

(* Computation of the states following a given state *)

module Working_area : sig
  type t

  val create : unit -> t
  val index_count : t -> int
end

val delta : Working_area.t -> Category.t -> Cset.c -> State.t -> State.t
