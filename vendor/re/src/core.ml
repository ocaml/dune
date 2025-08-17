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

open Import

include struct
  let cset = Ast.cset
  let rg c c' = cset (Cset.cseq c c')
  let notnl = cset Cset.notnl
  let lower = cset Cset.lower
  let upper = cset Cset.upper
  let alpha = cset Cset.alpha
  let digit = cset Cset.cdigit
  let alnum = cset Cset.alnum
  let wordc = cset Cset.wordc
  let ascii = cset Cset.ascii
  let blank = cset Cset.blank
  let cntrl = cset Cset.cntrl
  let graph = cset Cset.graph
  let print = cset Cset.print
  let punct = cset Cset.punct
  let space = cset Cset.space
  let xdigit = cset Cset.xdigit
end

include Ast.Export

let exec_internal ?(pos = 0) ?(len = -1) ~partial ~groups re s =
  Compile.match_str ~groups ~partial re s ~pos ~len
;;

let exec ?pos ?len re s =
  match exec_internal ?pos ?len ~groups:true ~partial:false re s with
  | Match substr -> substr
  | _ -> raise Not_found
;;

let exec_opt ?pos ?len re s =
  match exec_internal ?pos ?len ~groups:true ~partial:false re s with
  | Match substr -> Some substr
  | _ -> None
;;

let execp ?(pos = 0) ?(len = -1) re s = Compile.match_str_p ~pos ~len re s

let exec_partial ?pos ?len re s =
  match exec_internal ~groups:false ~partial:true ?pos ?len re s with
  | Match _ -> `Full
  | Running _ -> `Partial
  | Failed -> `Mismatch
;;

let exec_partial_detailed ?pos ?len re s =
  match exec_internal ~groups:true ~partial:true ?pos ?len re s with
  | Match group -> `Full group
  | Running { no_match_starts_before } -> `Partial no_match_starts_before
  | Failed -> `Mismatch
;;

module Mark = struct
  type t = Pmark.t

  let test (g : Group.t) p = Pmark.Set.mem p (Group.pmarks g)
  let all (g : Group.t) = Group.pmarks g

  module Set = Pmark.Set

  let equal = Pmark.equal
  let compare = Pmark.compare
end

type split_token =
  [ `Text of string
  | `Delim of Group.t
  ]

module Gen = struct
  type 'a gen = unit -> 'a option

  let gen_of_seq (s : 'a Seq.t) : 'a gen =
    let r = ref s in
    fun () ->
      match !r () with
      | Seq.Nil -> None
      | Seq.Cons (x, tl) ->
        r := tl;
        Some x
  ;;

  let split ?pos ?len re s : _ gen = Search.split ?pos ?len re s |> gen_of_seq
  let split_full ?pos ?len re s : _ gen = Search.split_full ?pos ?len re s |> gen_of_seq
  let all ?pos ?len re s = Search.all ?pos ?len re s |> gen_of_seq
  let matches ?pos ?len re s = Search.matches ?pos ?len re s |> gen_of_seq
end

module Group = Group

(** {2 Deprecated functions} *)

let split_full_seq = Search.split_full
let split_seq = Search.split
let matches_seq = Search.matches
let all_seq = Search.all

type 'a gen = 'a Gen.gen

let all_gen = Gen.all
let matches_gen = Gen.matches
let split_gen = Gen.split
let split_full_gen = Gen.split_full

type substrings = Group.t

let get = Group.get
let get_ofs = Group.offset
let get_all = Group.all
let get_all_ofs = Group.all_offset
let test = Group.test

type markid = Mark.t

let marked = Mark.test
let mark_set = Mark.all

type groups = Group.t

module List = struct
  let list_of_seq (s : 'a Seq.t) : 'a list =
    Seq.fold_left (fun l x -> x :: l) [] s |> List.rev
  ;;

  let all ?pos ?len re s = Search.all ?pos ?len re s |> list_of_seq
  let matches ?pos ?len re s = Search.matches ?pos ?len re s |> list_of_seq
  let split_full ?pos ?len re s = Search.split_full ?pos ?len re s |> list_of_seq
  let split ?pos ?len re s = Search.split ?pos ?len re s |> list_of_seq
  let split_delim ?pos ?len re s = Search.split_delim ?pos ?len re s |> list_of_seq
end

include List

include struct
  open Compile

  type nonrec re = re

  let compile = compile
  let pp_re = pp_re
  let print_re = pp_re
  let group_names = group_names
  let group_count = group_count
end

module Seq = Search
