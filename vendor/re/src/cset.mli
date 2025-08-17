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

(* Character sets, represented as sorted list of intervals *)

type c [@@immediate]

val equal_c : c -> c -> bool
val to_int : c -> int
val of_int : int -> c
val to_char : c -> char
val of_char : char -> c

type t

(** special characters which isn't present in any set (not even in [cany]) *)
val null_char : c

val equal : t -> t -> bool
val iter : t -> f:(c -> c -> unit) -> unit
val union : t -> t -> t
val union_all : t list -> t
val intersect_all : t list -> t
val inter : t -> t -> t
val diff : t -> t -> t
val empty : t
val single : c -> t
val add : c -> t -> t
val mem : c -> t -> bool
val case_insens : t -> t
val cdigit : t
val calpha : t
val cword : t
val notnl : t
val ascii : t
val nl : t
val cseq : char -> char -> t
val set : string -> t
val blank : t
val space : t
val xdigit : t
val lower : t
val upper : t
val alpha : t
val alnum : t
val wordc : t
val cntrl : t
val graph : t
val print : t
val punct : t
val pp : t Fmt.t
val one_char : t -> c option
val fold_right : t -> init:'acc -> f:(c -> c -> 'acc -> 'acc) -> 'acc
val hash : t -> int
val compare : t -> t -> int

module CSetMap : Map.S with type key = int * t

val cany : t
val csingle : char -> t
val is_empty : t -> bool
val prepend : t -> 'a list -> (t * 'a list) list -> (t * 'a list) list
val pick : t -> c

val offset : int -> t -> t
