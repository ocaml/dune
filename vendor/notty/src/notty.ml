(* Copyright (c) 2016-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let (&.) f g x = f (g x)

let btw (x : int) a b = a <= x && x <= b
let bit n b = b land (1 lsl n) > 0

let max (a : int) b = if a > b then a else b
let min (a : int) b = if a < b then a else b

let is_C0 x = x < 0x20 || x = 0x7f
and is_C1 x = 0x80 <= x && x < 0xa0
let is_ctrl x = is_C0 x || is_C1 x
and is_ascii x = x < 0x80

let rec concatm z (@) xs =
  let rec accum (@) = function
    | []|[_] as xs -> xs
    | a::b::xs -> (a @ b) :: accum (@) xs in
  match xs with [] -> z | [x] -> x | xs -> concatm z (@) (accum (@) xs)

let rec linspcm z (@) x n f = match n with
  | 0 -> z
  | 1 -> f x
  | _ -> let m = n / 2 in linspcm z (@) x m f @ linspcm z (@) (x + m) (n - m) f

let memo (type a) ?(hash=Hashtbl.hash) ?(eq=(=)) ~size f =
  let module H = Ephemeron.K1.Make
    (struct type t = a let (hash, equal) = (hash, eq) end) in
  let t = H.create size in fun x ->
    try H.find t x with Not_found -> let y = f x in H.add t x y; y

module Buffer = struct
  include Buffer
  let buf = Buffer.create 1024
  let mkstring f = f buf; let res = contents buf in reset buf; res
  let add_decimal b = function
    | x when btw x 0 999 ->
        let d1 = x / 100 and d2 = (x mod 100) / 10 and d3 = x mod 10 in
        if d1 > 0 then 0x30 + d1 |> Char.unsafe_chr |> add_char b;
        if (d1 + d2) > 0 then 0x30 + d2 |> Char.unsafe_chr |> add_char b;
        0x30 + d3 |> Char.unsafe_chr |> add_char b
    | x -> string_of_int x |> add_string b
  let add_chars b c n = for _ = 1 to n do add_char b c done
end

module String = struct
  include String
  let sub0cp s i len = if i > 0 || len < length s then sub s i len else s
  let of_chars_rev = function
    | []  -> ""
    | [c] -> String.make 1 c
    | cs  ->
        let n = List.length cs in
        let rec go bs i = Bytes.(function
          | []    -> unsafe_to_string bs
          | x::xs -> unsafe_set bs i x; go bs (pred i) xs
        ) in go (Bytes.create n) (n - 1) cs
end

module Option = struct

  let map f = function Some x -> Some (f x) | _ -> None
  let get def = function Some x -> x | _ -> def
  let to_list = function Some x -> [x] | _ -> []
  let (>>|) a f = map f a
  let (>>=) a f = match a with Some x -> f x | _ -> None
end

module Text = struct

  let err_ctrl u = invalid_arg "Notty: control char: U+%02X, %S" (Char.code u)
  let err_malformed = invalid_arg "Notty: malformed UTF-8: %s, %S"

  type t =
    | Ascii of string * int * int
    | Utf8  of string * int array * int * int

  let equal t1 t2 = match (t1, t2) with
    | (Utf8 (s1, _, i1, n1), Utf8 (s2, _, i2, n2))
    | (Ascii (s1, i1, n1), Ascii (s2, i2, n2)) -> i1 = i2 && n1 = n2 && s1 = s2
    | _ -> false

  let width = function Utf8 (_, _, _, w) -> w | Ascii (_, _, w)   -> w

  let empty = Ascii ("", 0, 0)

  let is_empty t = width t = 0

  let graphemes str =
    let module Uuseg = Notty_grapheme_cluster in
    let seg = Uuseg.create () in
    let rec f (is, w as acc) i evt =
      match Uuseg.add seg evt with
      | `Await | `End -> acc
      | `Uchar u      -> f (is, w + Notty_uucp.tty_width_hint u) i `Await
      | `Boundary     ->
          let is = match w with 0 -> is | 1 -> i::is | _ -> i::(-1)::is in
          f (is, 0) i `Await in
    let acc = Uutf.String.fold_utf_8 (fun acc i -> function
      | `Malformed err -> err_malformed err str
      | `Uchar _ as u  -> f acc i u
      ) ([0], 0) str in
    f acc (String.length str) `End |> fst |> List.rev |> Array.of_list (*XXX*)

  let dead = ' '

  let to_buffer buf = function
    | Ascii (s, off, w)    -> Buffer.add_substring buf s off w
    | Utf8 (s, ix, off, w) ->
        let x1 = match ix.(off) with
          | -1 -> Buffer.add_char buf dead; ix.(off + 1) | x -> x
        and x2 = ix.(off + w) in
        Buffer.add_substring buf s x1 @@
          (if x2 = -1 then ix.(off + w - 1) else x2) - x1;
        if x2 = -1 then Buffer.add_char buf dead

  let sub t x w =
    let w1 = width t in
    if w = 0 || x >= w1 then empty else
      let w = min w (w1 - x) in
      if w = w1 then t else match t with
        Ascii (s, off, _) -> Ascii (s, off + x, w)
      | Utf8 (s, ix, off, _) -> Utf8 (s, ix, off + x, w)

  let is_ascii_or_raise_ctrl s =
    let (@!) s i = String.unsafe_get s i |> Char.code in
    let rec go s acc i n =
      if n = 0 then acc else
        let x = s @! i in
        if is_C0 x then
          err_ctrl s.[i] s
        else if x = 0xc2 && n > 1 && is_C1 (s @! (i + 1)) then
          err_ctrl s.[i + 1] s
        else go s (acc && is_ascii x) (i + 1) (n - 1) in
    go s true 0 (String.length s)

  let of_ascii s = Ascii (s, 0, String.length s)
  and of_unicode s = let x = graphemes s in Utf8 (s, x, 0, Array.length x - 1)
  let of_unicode = memo ~eq:String.equal ~size:128 of_unicode

  let of_string = function
    | "" -> empty
    | s  -> if is_ascii_or_raise_ctrl s then of_ascii s else of_unicode s

  let of_uchars ucs = of_string @@ Buffer.mkstring @@ fun buf ->
    Array.iter (Buffer.add_utf_8_uchar buf) ucs

  let replicateu w u =
    if is_ctrl (Uchar.to_int u) then
      err_ctrl (Uchar.unsafe_to_char u) "<repeated character>"
    else if w < 1 then empty
    else if is_ascii (Uchar.to_int u) then
      of_ascii (String.make w (Uchar.unsafe_to_char u))
    else of_unicode @@ Buffer.mkstring @@ fun buf ->
      for _ = 1 to w do Buffer.add_utf_8_uchar buf u done

  let replicatec w c = replicateu w (Uchar.of_char c)
end

module A = struct

  type color = int
  type style = int
  type t = { fg : color; bg : color; st : style }

  let equal t1 t2 = t1.fg = t2.fg && t1.bg = t2.bg && t1.st = t2.st

  let black        = 0x01000000
  and red          = 0x01000001
  and green        = 0x01000002
  and yellow       = 0x01000003
  and blue         = 0x01000004
  and magenta      = 0x01000005
  and cyan         = 0x01000006
  and white        = 0x01000007
  and lightblack   = 0x01000008
  and lightred     = 0x01000009
  and lightgreen   = 0x0100000a
  and lightyellow  = 0x0100000b
  and lightblue    = 0x0100000c
  and lightmagenta = 0x0100000d
  and lightcyan    = 0x0100000e
  and lightwhite   = 0x0100000f

  let tag c = (c land 0x03000000) lsr 24

  let rgb ~r ~g ~b =
    if r < 0 || g < 0 || b < 0 || r > 5 || g > 5 || b > 5 then
      invalid_arg "Notty.A.rgb %d %d %d: channel out of range" r g b
    else 0x01000000 lor (r * 36 + g * 6 + b + 16)

  let gray level =
    if level < 0 || level > 23 then
      invalid_arg "Notty.A.gray %d: level out of range" level
    else 0x01000000 lor (level + 232)

  let rgb_888 ~r ~g ~b =
    if r < 0 || g < 0 || b < 0 || r > 255 || g > 255 || b > 255 then
      invalid_arg "Notty.A.rgb_888 %d %d %d: channel out of range" r g b
    else 0x02000000 lor ((r lsl 16) lor (g lsl 8) lor b)

  let i x = x land 0xff
  and r x = x lsr 16 land 0xff
  and g x = x lsr 8 land 0xff
  and b x = x land 0xff

  let bold      = 1
  and italic    = 2
  and dim       = 3
  and underline = 4
  and blink     = 8
  and reverse   = 16

  let empty = { fg = 0; bg = 0; st = 0 }

 let (++) a1 a2 =
   if a1 == empty then a2 else if a2 == empty then a1 else
     { fg = (match a2.fg with 0 -> a1.fg | x -> x)
     ; bg = (match a2.bg with 0 -> a1.bg | x -> x)
     ; st = a1.st lor a2.st }

  let fg fg = { empty with fg }
  let bg bg = { empty with bg }
  let st st = { empty with st }
end

module I = struct

  type dim = int * int

  type t =
    | Empty
    | Segment  of A.t * Text.t
    | Hcompose of (t * t) * dim
    | Vcompose of (t * t) * dim
    | Zcompose of (t * t) * dim
    | Hcrop    of (t * int * int) * dim
    | Vcrop    of (t * int * int) * dim
    | Void     of dim

  let width = function
    | Empty -> 0
    | Segment (_, text) -> Text.width text
    | Hcompose (_, (w, _)) -> w
    | Vcompose (_, (w, _)) -> w
    | Zcompose (_, (w, _)) -> w
    | Hcrop    (_, (w, _)) -> w
    | Vcrop    (_, (w, _)) -> w
    | Void         (w, _)  -> w [@@inline]

  let height = function
    | Empty -> 0
    | Segment _ -> 1
    | Hcompose (_, (_, h)) -> h
    | Vcompose (_, (_, h)) -> h
    | Zcompose (_, (_, h)) -> h
    | Hcrop    (_, (_, h)) -> h
    | Vcrop    (_, (_, h)) -> h
    | Void         (_, h)  -> h [@@inline]

  let equal t1 t2 =
    let rec eq t1 t2 = match (t1, t2) with
      | (Empty, Empty) -> true
      | (Segment (a1, t1), Segment (a2, t2)) ->
          A.equal a1 a2 && Text.equal t1 t2
      | (Hcompose ((a, b), _), Hcompose ((c, d), _))
      | (Vcompose ((a, b), _), Vcompose ((c, d), _))
      | (Zcompose ((a, b), _), Zcompose ((c, d), _)) -> eq a c && eq b d
      | (Hcrop ((a, i1, n1), _), Hcrop ((b, i2, n2), _))
      | (Vcrop ((a, i1, n1), _), Vcrop ((b, i2, n2), _)) ->
          i1 = i2 && n1 = n2 && eq a b
      | (Void (a, b), Void (c, d)) -> a = c && b = d
      | _ -> false in
    width t1 = width t2 && height t1 = height t2 && eq t1 t2

  let empty = Empty

  let (<|>) t1 t2 = match (t1, t2) with
    | (_, Empty) -> t1
    | (Empty, _) -> t2
    | _          ->
        let w = width t1 + width t2
        and h = max (height t1) (height t2) in
        Hcompose ((t1, t2), (w, h))

  let (<->) t1 t2 = match (t1, t2) with
    | (_, Empty) -> t1
    | (Empty, _) -> t2
    | _          ->
        let w = max (width t1) (width t2)
        and h = height t1 + height t2 in
        Vcompose ((t1, t2), (w, h))

  let (</>) t1 t2 = match (t1, t2) with
    | (_, Empty) -> t1
    | (Empty, _) -> t2
    | _          ->
        let w = max (width t1) (width t2)
        and h = max (height t1) (height t2) in
        Zcompose ((t1, t2), (w, h))

  let void w h =
    if w < 1 && h < 1 then Empty else Void (max 0 w, max 0 h)

  let lincropinv crop void (++) init fini img =
    match (init >= 0, fini >= 0) with
    | (true, true) -> crop init fini img
    | (true, _   ) -> crop init 0 img ++ void (-fini)
    | (_   , true) -> void (-init) ++ crop 0 fini img
    | _            -> void (-init) ++ img ++ void (-fini)

  let hcrop =
    let ctor left right img =
      let h = height img and w = width img - left - right in
      if w > 0 then Hcrop ((img, left, right), (w, h)) else void w h
    in lincropinv ctor (fun w -> void w 0) (<|>)

  let vcrop =
    let ctor top bottom img =
      let w = width img and h = height img - top - bottom in
      if h > 0 then Vcrop ((img, top, bottom), (w, h)) else void w h
    in lincropinv ctor (void 0) (<->)

  let crop ?(l=0) ?(r=0) ?(t=0) ?(b=0) img =
    let img = if l <> 0 || r <> 0 then hcrop l r img else img in
    if t <> 0 || b <> 0 then vcrop t b img else img

  let hpad left right img = hcrop (-left) (-right) img

  let vpad top bottom img = vcrop (-top) (-bottom) img

  let pad ?(l=0) ?(r=0) ?(t=0) ?(b=0) img =
    crop ~l:(-l) ~r:(-r) ~t:(-t) ~b:(-b) img

  let hcat = concatm empty (<|>)

  let vcat = concatm empty (<->)

  let zcat xs = List.fold_right (</>) xs empty

  let text attr tx =
    if Text.is_empty tx then void 0 1 else Segment (attr, tx)

  let string attr s = text attr (Text.of_string s)

  let uchars attr a = text attr (Text.of_uchars a)

  let tabulate m n f =
    let m = max m 0 and n = max n 0 in
    linspcm empty (<->) 0 n (fun y -> linspcm empty (<|>) 0 m (fun x -> f x y))

  let chars ctor attr c w h =
    if w < 1 || h < 1 then void w h else
      let line = text attr (ctor w c) in tabulate 1 h (fun _ _ -> line)

  let char  = chars Text.replicatec
  let uchar = chars Text.replicateu

  let hsnap ?(align=`Middle) w img =
    let off = width img - w in match align with
      | `Left   -> hcrop 0 off img
      | `Right  -> hcrop off 0 img
      | `Middle -> let w1 = off / 2 in hcrop w1 (off - w1) img

  let vsnap ?(align=`Middle) h img =
    let off = height img - h in match align with
      | `Top    -> vcrop 0 off img
      | `Bottom -> vcrop off 0 img
      | `Middle -> let h1 = off / 2 in vcrop h1 (off - h1) img

  module Fmt = struct

    open Format

    type stag += Attr of A.t

    let push r x = r := x :: !r
    let pop r = r := (match !r with _::xs -> xs | _ -> [])
    let top_a r = match !r with a::_ -> a | _ -> A.empty

    let create () =
      let img, line, attr = ref empty, ref empty, ref [] in
      let fmt = formatter_of_out_functions {
          out_flush = (fun () ->
            img := !img <-> !line; line := empty; attr := [])
        ; out_newline = (fun () ->
            img := !img <-> !line; line := void 0 1)
        ; out_string = (fun s i n ->
            line := !line <|> string (top_a attr) String.(sub0cp s i n))
        (* Not entirely clear; either or both could be void: *)
        ; out_spaces = (fun w -> line := !line <|> char (top_a attr) ' ' w 1)
        ; out_indent = (fun w -> line := !line <|> char (top_a attr) ' ' w 1)
      } in
      pp_set_formatter_stag_functions fmt {
        (pp_get_formatter_stag_functions fmt ()) with
            mark_open_stag =
              (function Attr a -> push attr A.(top_a attr ++ a); "" | _ -> "")
          ; mark_close_stag = (fun _ -> pop attr; "") };
      pp_set_mark_tags fmt true;
      fmt, fun () -> let i = !img in img := empty; line := empty; attr := []; i

    let ppf, reset = create ()

    let kstrf ?(attr = A.empty) ?(w = 1000000) k format =
      let m = ref 0 in
      let f1 _ () =
        m := pp_get_margin ppf ();
        pp_set_margin ppf w;
        pp_open_stag ppf (Attr attr)
      and k _ =
        pp_print_flush ppf ();
        pp_set_margin ppf !m;
        reset () |> k
      in kfprintf k ppf ("%a" ^^ format) f1 ()

    let strf ?attr ?w format = kstrf ?attr ?w (fun i -> i) format

    let attr attr f fmt x =
      pp_open_stag fmt (Attr attr); f fmt x; pp_close_stag fmt ()
  end

  let kstrf, strf, pp_attr = Fmt.(kstrf, strf, attr)
end

module Operation = struct

  type t =
    End
  | Skip of int * t
  | Text of A.t * Text.t * t

  let skip n k = if n = 0 then k else match k with
      End         -> End
    | Skip (m, k) -> Skip (m + n, k)
    | _           -> Skip (n, k) [@@inline]

  let rec scan x w row i k =
    let open I in match i with

    | Empty | Void _ -> skip w k

    | Segment _ when row > 0 -> skip w k
    | Segment (attr, text) ->
        let t  = Text.sub text x w in
        let w1 = Text.width t in
        let p  = if w > w1 then skip (w - w1) k else k in
        if w1 > 0 then Text (attr, t, p) else p

    | Hcompose ((i1, i2), _) ->
        let w1 = width i1
        and w2 = width i2 in
        if x >= w1 + w2 then skip w k else
        if x >= w1 then scan (x - w1) w row i2 k else
        if x + w <= w1 then scan x w row i1 k else
          scan x (w1 - x) row i1 @@ scan 0 (w - w1 + x) row i2 @@ k

    | Vcompose ((i1, i2), _) ->
        let h1 = height i1
        and h2 = height i2 in
        if row >= h1 + h2 then skip w k else
        if row >= h1 then scan x w (row - h1) i2 k else scan x w row i1 k

    | Zcompose ((i1, i2), _) ->
        let rec stitch x w row i = function
          | End -> scan x w row i End
          | Text (a, t, ops) as opss ->
              let w1 = Text.width t in
              if w1 >= w then opss else
                Text (a, t, stitch (x + w1) (w - w1) row i ops)
          | Skip (w1, ops) ->
              scan x w1 row i @@
                if w1 >= w then ops else stitch (x + w1) (w - w1) row i ops
        in stitch x w row i2 @@ scan x w row i1 @@ k

    | Hcrop ((i, left, _), (w1, _)) ->
        if x >= w1 then skip w k else
        if x + w <= w1 then scan (x + left) w row i k else
          scan (x + left) (w1 - x) row i @@ skip (w - w1 + x) k

    | Vcrop ((i, top, _), (_, h1)) ->
        if row < h1 then scan x w (top + row) i k else skip w k

  let of_image (x, y) (w, h) i =
    List.init h (fun off -> scan x (x + w) (y + off) i End)
end

module Cap = struct

  type op = Buffer.t -> unit

  let (&) op1 op2 buf = op1 buf; op2 buf

  type t = {
    skip    : int -> op
  ; sgr     : A.t -> op
  ; newline : op
  ; clreol  : op
  ; cursvis : bool -> op
  ; cursat  : int -> int -> op
  ; cubcuf  : int -> op
  ; cuucud  : int -> op
  ; cr      : op
  ; altscr  : bool -> op
  ; mouse   : bool -> op
  ; bpaste  : bool -> op
  }

  let ((<|), (<.), (<!)) = Buffer.(add_string, add_char, add_decimal)

  let sts = [ ";1"; ";3"; ";4"; ";5"; ";7" ]

  let sgr { A.fg; bg; st } buf =
    buf <| "\x1b[0";
    let rgb888 buf x =
      buf <! A.r x; buf <. ';'; buf <! A.g x; buf <. ';'; buf <! A.b x in
    ( match A.tag fg with
        0 -> ()
      | 1 -> let c = A.i fg in
             if c < 8  then ( buf <. ';'; buf <! (c + 30) )
             else if c < 16 then ( buf <. ';'; buf <! (c + 82) )
             else ( buf <| ";38;5;"; buf <! c )
      | _ -> buf <| ";38;2;"; rgb888 buf fg );
    ( match A.tag bg with
        0 -> ()
      | 1 -> let c = A.i bg in
             if c < 8  then ( buf <. ';'; buf <! (c + 40) )
             else if c < 16 then ( buf <. ';'; buf <! (c + 92) )
             else ( buf <| ";48;5;"; buf <! c )
      | _ -> buf <| ";48;2;"; rgb888 buf bg );
    if st <> 0 then
      ( let rec go f xs = match (f, xs) with
          | (0, _) | (_, []) -> ()
          | (_, x::xs) -> if f land 1 > 0 then buf <| x; go (f lsr 1) xs in
        go st sts );
    buf <. 'm'

  let ansi = {
      skip    = (fun n b -> b <| "\x1b[0m"; Buffer.add_chars b ' ' n)
    ; newline = (fun b -> b <| "\x1bE")
    ; altscr  = (fun x b -> b <| if x then "\x1b[?1049h" else "\x1b[?1049l")
    ; cursat  = (fun w h b -> b <| "\x1b["; b <! h; b <. ';'; b <! w; b <. 'H')
    ; cubcuf  = (fun x b -> b <| "\x1b["; b <! abs x; b <. if x < 0 then 'D' else 'C')
    ; cuucud  = (fun y b -> b <| "\x1b["; b <! abs y; b <. if y < 0 then 'A' else 'B')
    ; cr      = (fun b -> b <| "\x1b[1G")
    ; clreol  = (fun b -> b <| "\x1b[K")
    ; cursvis = (fun x b -> b <| if x then "\x1b[34h\x1b[?25h" else "\x1b[?25l")
    ; mouse   = (fun x b -> b <| if x then "\x1b[?1000;1002;1005;1015;1006h"
                                      else "\x1b[?1000;1002;1005;1015;1006l")
    ; bpaste  = (fun x b -> b <| if x then "\x1b[?2004h" else "\x1b[?2004l")
    ; sgr }

  let no0 _     = ()
  and no1 _ _   = ()
  and no2 _ _ _ = ()

  let dumb = {
      skip    = (fun n b -> Buffer.add_chars b ' ' n)
    ; newline = (fun b -> b <| "\n")
    ; altscr  = no1
    ; cursat  = no2
    ; cubcuf  = no1
    ; cuucud  = no1
    ; cr      = no0
    ; clreol  = no0
    ; cursvis = no1
    ; sgr     = no1
    ; mouse   = no1
    ; bpaste  = no1
    }

  let erase cap buf = cap.sgr A.empty buf; cap.clreol buf (* KEEP ETA-LONG. *)
  let cursat0 cap w h = cap.cursat (max w 0 + 1) (max h 0 + 1)
end

module Render = struct

  open Cap
  open Operation

  let skip_op cap buf n = cap.skip n buf
  let text_op cap buf a x = cap.sgr a buf; Text.to_buffer buf x

  let rec line cap buf = function
    End              -> erase cap buf
  | Skip (n,    End) -> erase cap buf; skip_op cap buf n
  | Text (a, x, End) -> erase cap buf; text_op cap buf a x
  | Skip (n,    ops) -> skip_op cap buf n; line cap buf ops
  | Text (a, x, ops) -> text_op cap buf a x; line cap buf ops

  let rec lines cap buf = function
    []      -> ()
  | [ln]    -> line cap buf ln; cap.sgr A.empty buf
  | ln::lns -> line cap buf ln; cap.newline buf; lines cap buf lns

  let to_buffer buf cap off dim img =
    Operation.of_image off dim img |> lines cap buf

  let pp cap ppf img =
    let open Format in
    let buf = Buffer.create (I.width img * 2) in
    let h, w = I.(height img, width img |> min (pp_get_margin ppf ())) in
    let img = I.(img </> vpad (h - 1) 0 (char A.empty ' ' w 1)) in
    pp_open_vbox ppf 0;
    for y = 0 to h - 1 do
      Buffer.clear buf; to_buffer buf cap (0, y) (w, 1) img;
      pp_print_as ppf w (Buffer.contents buf);
      if y < h - 1 then pp_print_cut ppf ()
    done;
    pp_close_box ppf ()

  let pp_image = pp Cap.ansi
  let pp_attr ppf a =
    let string_ = I.string A.empty in
    pp_image ppf I.(string_ "<" <|> string a "ATTR" <|> string_ ">")
end

module Unescape = struct

  type special = [
    `Escape
  | `Enter
  | `Tab
  | `Backspace
  | `Insert
  | `Delete
  | `Home | `End
  | `Arrow of [ `Up | `Down | `Left | `Right ]
  | `Page of [ `Up | `Down ]
  | `Function of int
  ]

  type button = [ `Left | `Middle | `Right | `Scroll of [ `Up | `Down ] ]

  type mods = [ `Meta | `Ctrl | `Shift ] list

  type key = [ special | `Uchar of Uchar.t  | `ASCII of char ] * mods

  type mouse = [ `Press of button | `Drag | `Release ] * (int * int) * mods

  type paste = [ `Start | `End ]

  type event = [ `Key of key | `Mouse of mouse | `Paste of paste ]

  type esc =
    C0    of char
  | C1    of char
  | SS2   of char
  | CSI   of string * int list * char
  | Esc_M of int * int * int
  | Uchar of Uchar.t

  let uchar = function `Uchar u -> u | `ASCII c -> Uchar.of_char c

  let csi =
    let open Option in
    let rec priv acc = function
      | x::xs when btw x 0x3c 0x3f -> priv (Char.unsafe_chr x::acc) xs
      | xs                         -> param (String.of_chars_rev acc) None [] xs
    and param prv p ps = function
      | x::xs when btw x 0x30 0x39 -> param prv (Some (get 0 p * 10 + x - 0x30)) ps xs
      | 0x3b::xs                   -> param prv None (get 0 p :: ps) xs
      | xs                         -> code prv (List.rev (to_list p @ ps)) xs
    and code prv ps = function (* Conflate two classes because urxvt... *)
      | x::xs when btw x 0x20 0x2f || btw x 0x40 0x7e ->
          Some (CSI (prv, ps, (Char.chr x)), xs)
      | _ -> None in
    priv []

  let rec demux =
    let chr = Char.chr in function
    | 0x1b::0x5b::0x4d::a::b::c::xs -> Esc_M (a, b, c) :: demux xs
    | 0x1b::0x5b::xs | 0x9b::xs ->
        let (r, xs) = csi xs |> Option.get (C1 '\x5b', xs) in r :: demux xs
    | 0x1b::0x4f::x::xs | 0x8f::x::xs
        when is_ascii x                 -> SS2 (chr x) :: demux xs
    | 0x1b::x::xs when is_C1 (x + 0x40) -> C1 (chr x) :: demux xs
    | x::xs when is_C1 x                -> C1 (chr (x - 0x40)) :: demux xs
    | x::xs when is_C0 x                -> C0 (chr x) :: demux xs
    | x::xs -> Uchar (Uchar.unsafe_of_int x) :: demux xs
    | [] -> []

  let xtrm_mod_flags = function
    | 2 -> Some [`Shift]
    | 3 -> Some [`Meta]
    | 4 -> Some [`Shift; `Meta]
    | 5 -> Some [`Ctrl]
    | 6 -> Some [`Shift; `Ctrl]
    | 7 -> Some [`Meta; `Ctrl]
    | 8 -> Some [`Shift; `Meta; `Ctrl]
    | _ -> None

  let mods_xtrm = function
    | [1;p] -> xtrm_mod_flags p
    | []    -> Some []
    | _     -> None

  let mods_rxvt = function
    | '~' -> Some []
    | '$' -> Some [`Shift]
    | '^' -> Some [`Ctrl]
    | '@' -> Some [`Ctrl; `Shift]
    | _ -> None

  let mods_common ps code = match (ps, code) with
    | ([], '~')  -> Some []
    | ([], c)    -> mods_rxvt c
    | ([p], '~') -> xtrm_mod_flags p
    | _          -> None

  let mouse_p p =
    let btn = match p land 3 with
      | 0 when bit 6 p -> `Scroll `Up
      | 0              -> `Left
      | 1 when bit 6 p -> `Scroll `Down
      | 1              -> `Middle
      | 2 when bit 6 p -> `ALL (* `Scroll `Left *)
      | 2              -> `Right
      | 3 when bit 6 p -> `ALL (* `Scroll `Right *)
      | _              -> `ALL
    and drag = bit 5 p
    and mods =
      (if bit 3 p then [`Meta] else []) @
      (if bit 4 p then [`Ctrl] else [])
    in (btn, drag, mods)

  let key k mods = Some (`Key (k, mods))

  let event_of_control_code =
    let open Option in function
    | Uchar u when Uchar.to_int u |> is_ascii ->
        Some (`Key (`ASCII (Uchar.unsafe_to_char u), []))
    | Uchar u -> Some (`Key (`Uchar u, []))

    | C0 '\x1b'        -> key `Escape []
    | C0 ('\b'|'\x7f') -> key `Backspace []
    | C0 '\n'          -> key `Enter []
    | C0 '\t'          -> key `Tab []

    | C0 x -> key (`ASCII Char.(code x + 0x40 |> unsafe_chr)) [`Ctrl]
    | C1 x -> key (`ASCII x) [`Meta]

    | CSI ("",[],'Z') -> key `Tab [`Shift]

    | CSI ("",p,'A') -> mods_xtrm p >>= key (`Arrow `Up)
    | CSI ("",p,'B') -> mods_xtrm p >>= key (`Arrow `Down)
    | CSI ("",p,'C') -> mods_xtrm p >>= key (`Arrow `Right)
    | CSI ("",p,'D') -> mods_xtrm p >>= key (`Arrow `Left)

    | CSI ("",[],'a') -> key (`Arrow `Up) [`Shift]
    | CSI ("",[],'b') -> key (`Arrow `Down) [`Shift]
    | CSI ("",[],'c') -> key (`Arrow `Right) [`Shift]
    | CSI ("",[],'d') -> key (`Arrow `Left) [`Shift]
    | SS2 ('A'|'a') -> key (`Arrow `Up) [`Ctrl]
    | SS2 ('B'|'b') -> key (`Arrow `Down) [`Ctrl]
    | SS2 ('C'|'c') -> key (`Arrow `Right) [`Ctrl]
    | SS2 ('D'|'d') -> key (`Arrow `Left) [`Ctrl]

    | CSI ("",5::p,c) -> mods_common p c >>= key (`Page `Up)
    | CSI ("",6::p,c) -> mods_common p c >>= key (`Page `Down)

    | CSI ("",2::p,c) -> mods_common p c >>= key `Insert
    | CSI ("",3::p,c) -> mods_common p c >>= key `Delete

    | CSI ("",[4],'h') -> key `Insert []
    | CSI ("",[],'L')  -> key `Insert [`Ctrl]
    | CSI ("",[],'P')  -> key `Delete []
    | CSI ("",[],'M')  -> key `Delete [`Ctrl]

    | CSI ("",p,'H')   -> mods_xtrm p >>= key `Home
    | CSI ("",[7|1],c) -> mods_rxvt c >>= key `Home

    | CSI ("",p,'F')   -> mods_xtrm p >>= key `End
    | CSI ("",[8|4],c) -> mods_rxvt c >>= key `End
    | CSI ("",[],'J')  -> key `End [`Ctrl]

    | SS2 ('P'..'S' as c) -> key (`Function (Char.code c - 0x4f)) []

    | CSI ("",p,('P'..'S' as c)) ->
        mods_xtrm p >>= key (`Function (Char.code c - 0x4f))

    | CSI ("",k::p,c) when btw k 11 15 || btw k 17 21 || btw k 23 26 ->
        mods_common p c >>= key (`Function ((k - 10) - (k - 10) / 6))

    | CSI ("<",[p;x;y],('M'|'m' as c)) ->
        let (btn, drag, mods) = mouse_p p in
        ( match (c, btn, drag) with
          | ('M', (#button as b), false) -> Some (`Press b)
          | ('M', #button, true)         -> Some `Drag
          | ('m', #button, false)        -> Some `Release
          (* | ('M', `ALL   , true)         -> Some `Move *)
          | _                            -> None
        ) >>| fun e -> `Mouse (e, (x - 1, y - 1), mods)

    | CSI ("",[p;x;y],'M') | Esc_M (p,x,y) as evt ->
        let (x, y) = match evt with Esc_M _ -> x - 32, y - 32 | _ -> x, y
        and (btn, drag, mods) = mouse_p (p - 32) in
        ( match (btn, drag) with
          | (#button as b, false) -> Some (`Press b)
          | (#button     , true ) -> Some `Drag
          | (`ALL        , false) -> Some `Release
          (* | (`ALL        , true)  -> Some `Move *)
          | _                     -> None
        ) >>| fun e -> `Mouse (e, (x - 1, y - 1), mods)

    | CSI ("",[200],'~') -> Some (`Paste `Start)
    | CSI ("",[201],'~') -> Some (`Paste `End)

    | CSI _ | SS2 _ -> None

  let rec events = function
    | C0 '\x1b' :: cc :: ccs ->
      ( match event_of_control_code cc with
        | Some (`Key (k, mods)) -> `Key (k, `Meta :: mods) :: events ccs
        | Some _                -> `Key (`Escape, []) :: events (cc::ccs)
        | None                  -> events ccs )
    | cc::ccs -> (event_of_control_code cc |> Option.to_list) @ events ccs
    | [] -> []

  let decode = events &. demux &. List.map Uchar.to_int

  type t = (event list * bool) ref

  let create () = ref ([], false)

  let next t = match !t with
    | (#event as e::es, eof) -> t := (es, eof) ; e
    | ([], false) -> `Await
    | _           -> `End

  let list_of_utf8 buf i l =
    let f cs _ = function `Uchar c -> c::cs | _ -> cs in
    String.sub0cp (Bytes.unsafe_to_string buf) i l
    |> Uutf.String.fold_utf_8 f [] |> List.rev

  let input t buf i l = t := match !t with
    | (es, false) when l > 0 -> (es @ (list_of_utf8 buf i l |> decode), false)
    | (es, _)                -> (es, true)

  let pending t = match !t with ([], false) -> false | _ -> true
end

module Tmachine = struct

  open Cap
  (* XXX This is sad. This should be a composable, stateless transducer. *)

  type t = {
    cap           : Cap.t
  ; mutable write : Buffer.t -> unit
  ; mutable curs  : (int * int) option
  ; mutable dim   : (int * int)
  ; mutable image : I.t
  ; mutable dead  : bool
  }

  let emit t op =
    if t.dead then
      invalid_arg "Notty: use of released terminal"
    else t.write <- t.write & op

  let cursor cap = function
    | None        -> cap.cursvis false
    | Some (w, h) -> cap.cursvis true & cursat0 cap w h

  let create ~mouse ~bpaste cap = {
      cap
    ; curs  = None
    ; dim   = (0, 0)
    ; image = I.empty
    ; dead  = false
    ; write =
        cap.altscr true & cursor cap None & cap.mouse mouse & cap.bpaste bpaste
    }

  let release t =
    if t.dead then false else
      ( emit t ( t.cap.altscr false & t.cap.cursvis true &
                 t.cap.mouse false & t.cap.bpaste false );
        t.dead <- true; true )

  let output t buf = t.write buf; t.write <- ignore

  let refresh ({ dim; image; _ } as t) =
    emit t ( cursor t.cap None & cursat0 t.cap 0 0 &
             (fun buf -> Render.to_buffer buf t.cap (0, 0) dim image) &
             cursor t.cap t.curs )

  let set_size t dim = t.dim <- dim
  let image t image = t.image <- image; refresh t
  let cursor t curs = t.curs <- curs; emit t (cursor t.cap curs)

  let size t = t.dim
  let dead t = t.dead
end

module Direct = struct
  let show_cursor buf cap x = cap.Cap.cursvis x buf
  and move_cursor buf cap cmd = match cmd with
    | `To (w, h) -> Cap.cursat0 cap w h buf
    | `Home      -> cap.Cap.cr buf
    | `By (x, y) ->
        Cap.(if x <> 0 then cap.cubcuf x buf; if y <> 0 then cap.cuucud y buf)
end

type attr  = A.t
type image = I.t

module Infix = struct
  let ((<->), (<|>), (</>)) = I.((<->), (<|>), (</>))
  let (++) = A.(++)
end
