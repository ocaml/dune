(*
   This file is distributed under LGPL-2.1-or-later. It is extracted from the
   0install library. See src/sat/COPYING.md for the full license.

   Copyright (C) 2013, Thomas Leonard
   See the README file for details, or visit http://0install.net.
*)

module List = Stdlib.ListLabels

let ( = ) = Int.equal
let ( < ) x y = Int.compare x y = -1
let ( > ) x y = Int.compare x y = 1

module Int = struct
  let[@warning "-32"] hash (x : int) = Hashtbl.hash x

  include Stdlib.Int
end

module Array = struct
  type nonrec t = Bytes.t

  let words = 8
  let[@inline] length t = Bytes.length t / words
  let[@inline] unsafe_get t i = Int64.to_int (Bytes.get_int64_ne t (i * words))
  let[@inline] unsafe_set t i x = Bytes.set_int64_ne t (i * words) (Int64.of_int x)

  let[@inline] make len x =
    let t = Bytes.create (len * words) in
    for i = 0 to length t - 1 do
      unsafe_set t i x
    done;
    t
  ;;

  let[@inline] make_absent len = Bytes.make (len * words) '\255'
  let clear t = Bytes.fill t 0 (Bytes.length t) '\255'
end

(* A specialized hash table that makes the following trade-offs:
   - Open addresing. Bucketing is quite memory intensive and dune is already
     a memory hog.
   - No boxing for empty slots. We make use of the fact that id's are never
     negative to achieve this.
   - No saving of the hash. Recomputing the hash for id's is a no-op.
*)

type nonrec table =
  { mutable table : Array.t
  ; mutable size : int
  }

type t = table Option.t ref

let init t =
  if Option.is_none !t then t := Option.some { size = 0; table = Array.make 0 (-1) };
  Option.get !t
;;

let[@inline] should_grow t =
  let slots = Array.length t.table in
  slots = 0 || (t.size > 0 && slots / t.size < 2)
;;

let absent = -1

let () =
  let x = Array.make_absent 1 in
  assert (Array.unsafe_get x 0 = absent)
;;

let create () = ref Option.none

let[@inline] index_of_offset slots index i =
  let i = index + !i in
  if i >= slots then i - slots else i
;;

let clear t =
  match !t with
  | None -> ()
  | Some t ->
    t.size <- 0;
    Array.clear t.table
;;

let add t x =
  let hash = Int.hash x in
  let slots = Array.length t.table in
  let index = hash land (slots - 1) in
  let inserting = ref true in
  let i = ref 0 in
  while !inserting do
    let idx = index_of_offset slots index i in
    let elem = Array.unsafe_get t.table idx in
    if elem = absent
    then (
      Array.unsafe_set t.table idx x;
      inserting := false)
    else incr i
  done;
  t.size <- t.size + 1
;;

let resize t =
  let old_table = t.table in
  let slots = Array.length old_table in
  let table = Array.make_absent (if slots = 0 then 1 else slots lsl 1) in
  t.table <- table;
  for i = 0 to slots - 1 do
    let elem = Array.unsafe_get old_table i in
    if elem <> absent then add t elem
  done
;;

let add t x =
  let t = init t in
  if should_grow t then resize t;
  add t x
;;

let[@inline] is_empty t =
  let t = !t in
  if Option.is_none t
  then true
  else (
    let t = Option.get t in
    t.size = 0)
;;

let mem t x =
  let t = !t in
  if Option.is_none t || (Option.get t).size = 0
  then false
  else (
    let t = Option.get t in
    let hash = Int.hash x in
    let slots = Array.length t.table in
    let index = hash land (slots - 1) in
    let i = ref 0 in
    let found = ref false in
    while (not !found) && !i < slots do
      let idx = index_of_offset slots index i in
      let elem = Array.unsafe_get t.table idx in
      if Int.equal elem x
      then found := true
      else if Int.equal elem absent
      then i := slots
      else incr i
    done;
    !found)
;;
