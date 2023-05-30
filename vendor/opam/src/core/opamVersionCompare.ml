(******************************************************************************)
(*  This file is part of the Dose library http://www.irill.org/software/dose  *)
(*                                                                            *)
(*  Copyright (C) 2011 Ralf Treinen <ralf.treinen@pps.jussieu.fr>             *)
(*                                                                            *)
(*  All rights reserved. This file is distributed under the terms of the      *)
(*  GNU Lesser General Public License version 2.1, with the special           *)
(*  exception on linking described in the file LICENSE.                       *)
(*                                                                            *)
(*  Work developed with the support of the Mancoosi Project                   *)
(*  http://www.mancoosi.org                                                   *)
(*                                                                            *)
(******************************************************************************)

let is_digit = function
  | '0'..'9' -> true
  | _ -> false
;;

(* [skip_while_from i f w m] yields the index of the leftmost character
 * in the string [s], starting from [i], and ending at [m], that does
 * not satisfy the predicate [f], or [length w] if no such index exists.  *)
let skip_while_from i f w m =
  let rec loop i =
    if i = m then i
    else if f w.[i] then loop (i + 1) else i
  in loop i
;;

(* splits a version into (epoch,rest), without the separating ':'. The
 * epoch is delimited by the leftmost occurrence of ':' in x, and is ""
 * in case there is no ':' in x.  *)
let extract_epoch x =
  try
    let ci = String.index x ':' in
    let epoch = String.sub x 0 ci
    and rest = String.sub x (ci + 1) (String.length x - ci - 1)
    in (epoch,rest)
  with
    | Not_found -> ("",x)
;;

(* splits a version into (prefix,revision). The revision starts on the
 * right-most occurrence of '-', or is empty in case the version does
 * not contain '-'.  *)
let extract_revision x =
  try
    let di = String.rindex x '-' in
    let before = String.sub x 0 di in
    let after = String.sub x (di+1) (String.length x - di -1) in
    (before,after)
  with
    | Not_found -> (x,"")
;;

(* character comparison uses a modified character ordering: '~' first,
   then letters, then anything else *)
let compare_chars c1 c2 = match c1 with
  | '~' -> (match c2 with
      | '~' -> 0
      | _ -> -1)
  | 'a'..'z'|'A'..'Z' -> (match c2 with
      | '~' -> 1
      | 'a'..'z'|'A'..'Z' -> Char.compare c1 c2
      | _ -> -1)
  | _ -> (match c2 with
      | '~'|'a'..'z'|'A'..'Z' -> 1
      | _ -> Char.compare c1 c2)
;;

(* return the first index of x, starting from xi, of a nun-null
 * character in x.  or (length x) in case x contains only 0's starting
 * from xi on.  *)
let skip_zeros x xi xl = skip_while_from xi (fun c -> c = '0') x xl;;

(* compare versions chunks, that is parts of version strings that are
 * epoch, upstream version, or revisision. Alternates string comparison
 * and numerical comparison.  *)
let compare_chunks x y =
  (* x and y may be empty *)
  let xl = String.length x
  and yl = String.length y
  in
  let rec loop_lexical xi yi =
    assert (xi <= xl && yi <= yl);
    match (xi=xl,yi=yl) with (* which of x and y is exhausted? *)
      | true,true -> 0
      | true,false ->
        (* if y continues numerically than we have to continue by
         * comparing numerically. In this case the x part is
         * interpreted as 0 (since empty). If the y part consists
         * only of 0's then both parts are equal, otherwise the y
         * part is larger. If y continues non-numerically then y is
         * larger anyway, so we only have to skip 0's in the y part
         * and check whether this exhausts the y part.  *)
        let ys = skip_zeros y yi yl in
        if ys = yl then 0 else if y.[ys]='~' then 1 else -1
      | false,true -> (* symmetric to the preceding case *)
        let xs = skip_zeros x xi xl in
        if xs = xl then 0 else if x.[xs]='~' then -1 else 1
      | false,false -> (* which of x and y continues numerically? *)
        match (is_digit x.[xi], is_digit y.[yi]) with
          | true,true ->
            (* both continue numerically. Skip leading zeros in the
             * remaining parts, and then continue by
             * comparing numerically. *)
            compare_numerical (skip_zeros x xi xl) (skip_zeros y yi yl)
          | true,false -> (* '~' is smaller than any numeric part *)
            if y.[yi]='~' then 1 else -1
          | false,true -> (* '~' is smaller than any numeric part *)
            if x.[xi]='~' then -1 else 1
          | false,false -> (* continue comparing lexically *)
            let comp = compare_chars x.[xi] y.[yi]
            in if comp = 0 then loop_lexical (xi+1) (yi+1) else comp
  and compare_numerical xi yi =
    assert (xi = xl || (xi < xl && x.[xi] <> '0'));
    (* leading zeros have been stripped *)
    assert (yi = yl || (yi < yl && y.[yi] <> '0'));
    (* leading zeros have been stripped *)
    let xn = skip_while_from xi is_digit x xl (* length of numerical part *)
    and yn = skip_while_from yi is_digit y yl (* length of numerical part *)
    in
    let comp = compare (xn-xi) (yn-yi)
    in if comp = 0
      then (* both numerical parts have same length: compare digit by digit *)
        loop_numerical xi yi yn
      else
        (* if one numerical part is longer than the other we have found the
         * answer since leading 0 have been striped when switching
         * to numerical comparison.  *)
        comp
  and loop_numerical xi yi yn =
    assert (xi <= xl && yi <= yn && yn <= yl);
    (* invariant: the two numerical parts that remain to compare are
       of the same length *)
    if yi=yn
    then
      (* both numerical parts are exhausted, we switch to lexical
         comparison *)
      loop_lexical xi yi
    else
      (* both numerical parts are not exhausted, we continue comparing
         digit by digit *)
      let comp = Char.compare x.[xi] y.[yi]
      in if comp = 0 then loop_numerical (xi+1) (yi+1) yn else comp
  in loop_lexical 0 0
;;

let compare (x : string) (y : string) =
  let normalize_comp_result x = if x=0 then 0 else if x < 0 then -1 else 1
  in
  if x = y then 0
  else
    let (e1,rest1) = extract_epoch x
    and (e2,rest2) = extract_epoch y in
    let e_comp = compare_chunks e1 e2 in
    if e_comp <> 0 then normalize_comp_result e_comp
    else
      let (u1,r1) = extract_revision rest1
      and (u2,r2) = extract_revision rest2 in
      let u_comp = compare_chunks u1 u2 in
      if u_comp <> 0 then normalize_comp_result u_comp
      else normalize_comp_result (compare_chunks r1 r2)
;;

let equal (x : string) (y : string) =
  if x = y then true else (compare x y) = 0
;;
