(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* These are the rules as found in [1], with property values aliases [2]
   substituted.

   GB1.               sot ÷ Any
   GB2.               Any ÷ eot
   GB3.                CR × LF
   GB4.        (CN|CR|LF) ÷
   GB5.                   ÷ (CN|CR|LF)
   GB6.                 L × (L|V|LV|LVT)
   GB7.            (LV|V) × (V|T)
   GB8.           (LVT|T) × T
   GB9.                   × (EX|ZWJ)
   GB9a.                  × SM
   GB9b.               PP ×
   GB10. (v10.0.0) (EB|EBG) EX* × EM
   GB11. (v10.0.0)          ZWJ × (GAZ|EBG)
   GB12.  sot (RI RI)* RI × RI
   GB13.   [^RI] (RI RI)* × RI
   GB999.             Any ÷ Any

   [1]: http://www.unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries
   [2]: http://www.unicode.org/Public/7.0.0/ucd/PropertyValueAliases.txt
   [3]: http://www.unicode.org/Public/7.0.0/ucd/auxiliary/GraphemeBreakTest.html

   By the structure of the rules we see that grapheme clusters
   boundaries can *mostly* be determined by simply looking at the
   grapheme cluster break property value of the character on the left
   and on the right of a boundary. The exceptions are GB10 and GB12-13
   which are handled specially by enriching the segmenter state in
   a horribly ad-hoc fashion. *)

type ret = [ `Await | `Boundary | `End | `Uchar of Uchar.t ]

type gcb =
  | CN | CR | EX | EB | EBG | EM | GAZ | L | LF | LV | LVT | PP | RI
  | SM | T | V | XX | ZWJ | Sot

(* WARNING. The indexes used here need to be synchronized with those
   assigned by uucp for Uucp.Break.Low.grapheme_cluster. *)

let byte_to_gcb =
  [| CN; CR; EX; EB; EBG; EM; GAZ; L; LF; LV; LVT; PP; RI;
     SM; T; V; XX; ZWJ; |]

let gcb u = byte_to_gcb.(Notty_uucp.grapheme_cluster_boundary u)

type state =
| Fill  (* get next uchar to decide boundary. *)
| Flush (* an uchar is buffered, client needs to get it out with `Await. *)
| End   (* `End was added. *)

type t =
  { mutable state : state;                                 (* current state. *)
    mutable left : gcb;            (* break property value left of boundary. *)
    mutable odd_ri : bool;                  (* odd number of RI on the left. *)
    mutable emoji_seq : bool;               (* (EB|EBG) Extend* on the left. *)
    mutable buf : [ `Uchar of Uchar.t ] }                 (* bufferized add. *)

let nul_buf = `Uchar (Uchar.unsafe_of_int 0x0000)

let create () =
  { state = Fill; left = Sot;
    odd_ri = false; emoji_seq = false;
    buf = nul_buf (* overwritten *); }

let break s right = match s.left, right with
| (* GB1 *)   Sot, _ -> true
  (* GB2 is handled by `End *)
| (* GB3 *)   CR, LF -> false
| (* GB4 *)   (CN|CR|LF), _ -> true
| (* GB5 *)   _, (CN|CR|LF) -> true
| (* GB6 *)   L, (L|V|LV|LVT) -> false
| (* GB7 *)   (LV|V), (V|T) -> false
| (* GB8 *)   (LVT|T), T -> false
| (* GB9+a *) _, (EX|ZWJ|SM) -> false
| (* GB9b *)  PP, _ -> false
| (* GB10 *)  _, EM when s.emoji_seq -> false
| (* GB11 *)  ZWJ, (GAZ|EBG) -> false
| (* GB12+13 *) RI, RI when s.odd_ri -> false
| (* GB999 *) _, _ -> true

let update_left s right =
  s.left <- right;
  match s.left with
  | EX -> (* keep s.emoji_seq as is *) s.odd_ri <- false
  | EB | EBG -> s.emoji_seq <- true; s.odd_ri <- false
  | RI -> s.emoji_seq <- false; s.odd_ri <- not s.odd_ri
  | _ -> s.emoji_seq <- false; s.odd_ri <- false

let add s = function
| `Uchar u as add ->
    begin match s.state with
    | Fill ->
        let right = gcb u in
        let break = break s right in
        update_left s right;
        if not break then add else
        (s.state <- Flush; s.buf <- add; `Boundary)
    | Flush | End -> assert false
    end
| `Await ->
    begin match s.state with
    | Flush -> s.state <- Fill; (s.buf :> ret)
    | End -> `End
    | Fill -> `Await
    end
| `End ->
    begin match s.state with
    | Fill -> s.state <- End; if s.left = Sot then `End else `Boundary
    | Flush | End -> assert false
    end

(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
