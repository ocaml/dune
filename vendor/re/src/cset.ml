module List = struct end
open Import

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

type c = int

let equal_c = Int.equal
let to_int x = x
let of_int x = x
let to_char t = Char.chr t
let of_char c = Char.code c
let null_char = -1

type t = (c * c) list

let compare_pair (x, y) (x', y') =
  match Int.compare x x' with
  | 0 -> Int.compare y y'
  | x -> x
;;

let equal_pair (x, y) (x', y') = Int.equal x x' && Int.equal y y'
let equal = List.equal ~eq:equal_pair
let compare : t -> t -> int = List.compare ~cmp:compare_pair

let rec union l l' =
  match l, l' with
  | _, [] -> l
  | [], _ -> l'
  | (c1, c2) :: r, (c1', c2') :: r' ->
    if c2 + 1 < c1'
    then (c1, c2) :: union r l'
    else if c2' + 1 < c1
    then (c1', c2') :: union l r'
    else if c2 < c2'
    then union r ((min c1 c1', c2') :: r')
    else union ((min c1 c1', c2) :: r) r'
;;

let rec inter l l' =
  match l, l' with
  | _, [] -> []
  | [], _ -> []
  | (c1, c2) :: r, (c1', c2') :: r' ->
    if c2 < c1'
    then inter r l'
    else if c2' < c1
    then inter l r'
    else if c2 < c2'
    then (max c1 c1', c2) :: inter r l'
    else (max c1 c1', c2') :: inter l r'
;;

let rec diff l l' =
  match l, l' with
  | _, [] -> l
  | [], _ -> []
  | (c1, c2) :: r, (c1', c2') :: r' ->
    if c2 < c1'
    then (c1, c2) :: diff r l'
    else if c2' < c1
    then diff l r'
    else (
      let r'' = if c2' < c2 then (c2' + 1, c2) :: r else r in
      if c1 < c1' then (c1, c1' - 1) :: diff r'' r' else diff r'' r')
;;

let single c = [ c, c ]
let add c l = union (single c) l
let seq c c' = if c <= c' then [ c, c' ] else [ c', c ]

let rec offset o l =
  match l with
  | [] -> []
  | (c1, c2) :: r -> (c1 + o, c2 + o) :: offset o r
;;

let empty = []
let cany = [ 0, 255 ]
let union_all : t list -> t = List.fold_left ~init:empty ~f:union
let intersect_all : t list -> t = List.fold_left ~init:cany ~f:inter

let rec mem (c : int) s =
  match s with
  | [] -> false
  | (c1, c2) :: rem -> if c <= c2 then c >= c1 else mem c rem
;;

(****)

let rec hash_rec = function
  | [] -> 0
  | (i, j) :: r -> i + (13 * j) + (257 * hash_rec r)
;;

let hash l = hash_rec l land 0x3FFFFFFF

(****)

let print_one ch (c1, c2) =
  if Int.equal c1 c2 then Format.fprintf ch "%d" c1 else Format.fprintf ch "%d-%d" c1 c2
;;

let pp = Fmt.list ~pp_sep:(Fmt.lit ", ") print_one

let rec iter t ~f =
  match t with
  | [] -> ()
  | (x, y) :: xs ->
    f x y;
    iter xs ~f
;;

let one_char = function
  | [ (i, j) ] when Int.equal i j -> Some i
  | _ -> None
;;

module CSetMap = Map.Make (struct
    type t = int * (int * int) list

    let compare (i, u) (j, v) =
      let c = Int.compare i j in
      if c <> 0 then c else compare u v
    ;;
  end)

let fold_right t ~init ~f = List.fold_right ~f:(fun (x, y) acc -> f x y acc) t ~init
let csingle c = single (Char.code c)

let is_empty = function
  | [] -> true
  | _ -> false
;;

let rec prepend s x l =
  match s, l with
  | [], _ -> l
  | _r, [] -> []
  | (_c, c') :: r, ([ (d, _d') ], _x') :: _r' when c' < d -> prepend r x l
  | (c, c') :: r, ([ (d, d') ], x') :: r' ->
    if c <= d
    then
      if c' < d'
      then ([ d, c' ], x @ x') :: prepend r x (([ c' + 1, d' ], x') :: r')
      else ([ d, d' ], x @ x') :: prepend s x r'
    else if c > d'
    then ([ d, d' ], x') :: prepend s x r'
    else ([ d, c - 1 ], x') :: prepend s x (([ c, d' ], x') :: r')
  | _ -> assert false
;;

let pick = function
  | [] -> invalid_arg "Re_cset.pick"
  | (x, _) :: _ -> x
;;

let cseq c c' = seq (of_char c) (of_char c')
let rg = cseq
let char = csingle
let upper = union_all [ cseq 'A' 'Z'; cseq '\192' '\214'; cseq '\216' '\222' ]
let clower = offset 32 upper
let cdigit = cseq '0' '9'
let ascii = cseq '\000' '\127'
let cadd c s = add (of_char c) s
let space = add (of_char ' ') (cseq '\009' '\013')
let xdigit = union_all [ cdigit; cseq 'a' 'f'; cseq 'A' 'F' ]

let calpha =
  List.fold_right
    ~f:cadd
    [ '\170'; '\181'; '\186'; '\223'; '\255' ]
    ~init:(union clower upper)
;;

let calnum = union calpha cdigit

let case_insens s =
  union_all [ s; offset 32 (inter s upper); offset (-32) (inter s clower) ]
;;

let cword = cadd '_' calnum
let notnl = diff cany (csingle '\n')
let nl = csingle '\n'

let set str =
  let s = ref empty in
  for i = 0 to String.length str - 1 do
    s := union (csingle str.[i]) !s
  done;
  !s
;;

let blank = set "\t "

(* CR-someday rgrinberg: this [lower] doesn't match [clower] *)
let lower = union_all [ rg 'a' 'z'; char '\181'; rg '\223' '\246'; rg '\248' '\255' ]
let alpha = union_all [ lower; upper; char '\170'; char '\186' ]
let alnum = union_all [ alpha; cdigit ]
let wordc = union_all [ alnum; char '_' ]
let cntrl = union_all [ rg '\000' '\031'; rg '\127' '\159' ]
let graph = union_all [ rg '\033' '\126'; rg '\160' '\255' ]
let print = union_all [ rg '\032' '\126'; rg '\160' '\255' ]

let punct =
  union_all
    [ rg '\033' '\047'
    ; rg '\058' '\064'
    ; rg '\091' '\096'
    ; rg '\123' '\126'
    ; rg '\160' '\169'
    ; rg '\171' '\180'
    ; rg '\182' '\185'
    ; rg '\187' '\191'
    ; char '\215'
    ; char '\247'
    ]
;;
