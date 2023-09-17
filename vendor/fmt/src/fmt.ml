(*---------------------------------------------------------------------------
   Copyright (c) 2014 The fmt programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let invalid_arg' = invalid_arg

(* Errors *)

let err_str_formatter = "Format.str_formatter can't be set."

(* Standard outputs *)

let stdout = Format.std_formatter
let stderr = Format.err_formatter

(* Formatting *)

let pf = Format.fprintf
let pr = Format.printf
let epr = Format.eprintf
let str = Format.asprintf
let kpf = Format.kfprintf
let kstr = Format.kasprintf
let failwith fmt = kstr failwith fmt
let failwith_notrace fmt = kstr (fun s -> raise_notrace (Failure s)) fmt
let invalid_arg fmt = kstr invalid_arg fmt
let error fmt = kstr (fun s -> Error s) fmt
let error_msg fmt = kstr (fun s -> Error (`Msg s)) fmt

(* Formatters *)

type 'a t = Format.formatter -> 'a -> unit

let flush ppf _ = Format.pp_print_flush ppf ()
let nop fmt ppf = ()
let any fmt ppf _ = pf ppf fmt
let using f pp ppf v = pp ppf (f v)
let const pp_v v ppf _ = pp_v ppf v
let fmt fmt ppf = pf ppf fmt

(* Separators *)

let cut ppf _ = Format.pp_print_cut ppf ()
let sp ppf _ = Format.pp_print_space ppf ()
let sps n ppf _ = Format.pp_print_break ppf n 0
let comma ppf _ = Format.pp_print_string ppf ","; sp ppf ()
let semi ppf _ = Format.pp_print_string ppf ";"; sp ppf ()

(* Sequencing *)

let iter ?sep:(pp_sep = cut) iter pp_elt ppf v =
  let is_first = ref true in
  let pp_elt v =
    if !is_first then (is_first := false) else pp_sep ppf ();
    pp_elt ppf v
  in
  iter pp_elt v

let iter_bindings ?sep:(pp_sep = cut) iter pp_binding ppf v =
  let is_first = ref true in
  let pp_binding k v =
    if !is_first then (is_first := false) else pp_sep ppf ();
    pp_binding ppf (k, v)
  in
  iter pp_binding v

let append pp_v0 pp_v1 ppf v = pp_v0 ppf v; pp_v1 ppf v
let ( ++ ) = append
let concat ?sep pps ppf v = iter ?sep List.iter (fun ppf pp -> pp ppf v) ppf pps

(* Boxes *)

let box ?(indent = 0) pp_v ppf v =
  Format.(pp_open_box ppf indent; pp_v ppf v; pp_close_box ppf ())

let hbox pp_v ppf v =
  Format.(pp_open_hbox ppf (); pp_v ppf v; pp_close_box ppf ())

let vbox ?(indent = 0) pp_v ppf v =
  Format.(pp_open_vbox ppf indent; pp_v ppf v; pp_close_box ppf ())

let hvbox ?(indent = 0) pp_v ppf v =
  Format.(pp_open_hvbox ppf indent; pp_v ppf v; pp_close_box ppf ())

let hovbox ?(indent = 0) pp_v ppf v =
  Format.(pp_open_hovbox ppf indent; pp_v ppf v; pp_close_box ppf ())

(* Brackets *)

let surround s1 s2 pp_v ppf v =
  Format.(pp_print_string ppf s1; pp_v ppf v; pp_print_string ppf s2)

let parens pp_v = box ~indent:1 (surround "(" ")" pp_v)
let brackets pp_v = box ~indent:1 (surround "[" "]" pp_v)
let oxford_brackets pp_v = box ~indent:2 (surround "[|" "|]" pp_v)
let braces pp_v = box ~indent:1 (surround "{" "}" pp_v)
let quote ?(mark = "\"") pp_v =
  let pp_mark ppf _ = Format.pp_print_as ppf 1 mark in
  box ~indent:1 (pp_mark ++ pp_v ++ pp_mark)

(* Stdlib types formatters *)

let bool = Format.pp_print_bool
let int = Format.pp_print_int
let nativeint ppf v = pf ppf "%nd" v
let int32 ppf v = pf ppf "%ld" v
let int64 ppf v = pf ppf "%Ld" v
let uint ppf v = pf ppf "%u" v
let uint32 ppf v = pf ppf "%lu" v
let uint64 ppf v = pf ppf "%Lu" v
let unativeint ppf v = pf ppf "%nu" v
let char = Format.pp_print_char
let string = Format.pp_print_string
let buffer ppf b = string ppf (Buffer.contents b)
let exn ppf e = string ppf (Printexc.to_string e)
let exn_backtrace ppf (e, bt) =
  let pp_backtrace_str ppf s =
    let stop = String.length s - 1 (* there's a newline at the end *) in
    let rec loop left right =
      if right = stop then string ppf (String.sub s left (right - left)) else
      if s.[right] <> '\n' then loop left (right + 1) else
      begin
        string ppf (String.sub s left (right - left));
        cut ppf ();
        loop (right + 1) (right + 1)
      end
    in
    if s = "" then (string ppf "No backtrace available.") else
    loop 0 0
  in
  pf ppf "@[<v>Exception: %a@,%a@]"
    exn e pp_backtrace_str (Printexc.raw_backtrace_to_string bt)

let float ppf v = pf ppf "%g" v
let round x = floor (x +. 0.5)
let round_dfrac d x =
  if x -. (round x) = 0. then x else                   (* x is an integer. *)
  let m = 10. ** (float_of_int d) in                (* m moves 10^-d to 1. *)
  (floor ((x *. m) +. 0.5)) /. m

let round_dsig d x =
  if x = 0. then 0. else
  let m = 10. ** (floor (log10 (abs_float x))) in       (* to normalize x. *)
  (round_dfrac d (x /. m)) *. m

let float_dfrac d ppf f = pf ppf "%g" (round_dfrac d f)
let float_dsig d ppf f = pf ppf "%g" (round_dsig d f)

let pair ?sep:(pp_sep = cut) pp_fst pp_snd ppf (fst, snd) =
  pp_fst ppf fst; pp_sep ppf (); pp_snd ppf snd

let option ?none:(pp_none = nop) pp_v ppf = function
| None -> pp_none ppf ()
| Some v -> pp_v ppf v

let result ~ok ~error ppf = function
| Ok v -> ok ppf v
| Error e -> error ppf e

let list ?sep pp_elt = iter ?sep List.iter pp_elt
let array ?sep pp_elt = iter ?sep Array.iter pp_elt
let seq ?sep pp_elt = iter ?sep Seq.iter pp_elt
let hashtbl ?sep pp_binding = iter_bindings ?sep Hashtbl.iter pp_binding
let queue ?sep pp_elt = iter Queue.iter pp_elt
let stack ?sep pp_elt = iter Stack.iter pp_elt

(* Stdlib type dumpers *)

module Dump = struct

  (* Stdlib types *)

  let sig_names =
    Sys.[ sigabrt, "SIGABRT"; sigalrm, "SIGALRM"; sigfpe, "SIGFPE";
          sighup, "SIGHUP"; sigill, "SIGILL"; sigint, "SIGINT";
          sigkill, "SIGKILL"; sigpipe, "SIGPIPE"; sigquit, "SIGQUIT";
          sigsegv, "SIGSEGV"; sigterm, "SIGTERM"; sigusr1, "SIGUSR1";
          sigusr2, "SIGUSR2"; sigchld, "SIGCHLD"; sigcont, "SIGCONT";
          sigstop, "SIGSTOP"; sigtstp, "SIGTSTP"; sigttin, "SIGTTIN";
          sigttou, "SIGTTOU"; sigvtalrm, "SIGVTALRM"; sigprof, "SIGPROF";
          sigbus, "SIGBUS"; sigpoll, "SIGPOLL"; sigsys, "SIGSYS";
          sigtrap, "SIGTRAP"; sigurg, "SIGURG"; sigxcpu, "SIGXCPU";
          sigxfsz, "SIGXFSZ"; ]

  let signal ppf s = match List.assq_opt s sig_names with
  | Some name -> string ppf name
  | None -> pf ppf "SIG(%d)" s

  let uchar ppf u = pf ppf "U+%04X" (Uchar.to_int u)
  let string ppf s = pf ppf "%S" s
  let pair pp_fst pp_snd =
    parens (using fst (box pp_fst) ++ comma ++ using snd (box pp_snd))

  let option pp_v ppf = function
  | None -> pf ppf "None"
  | Some v -> pf ppf "@[<2>Some@ @[%a@]@]" pp_v v

  let result ~ok ~error ppf = function
  | Ok v -> pf ppf "@[<2>Ok@ @[%a@]@]" ok v
  | Error e -> pf ppf "@[<2>Error@ @[%a@]@]" error e

  (* Sequencing *)

  let iter iter_f pp_name pp_elt =
    let pp_v = iter ~sep:sp iter_f (box pp_elt) in
    parens (pp_name ++ sp ++ pp_v)

  let iter_bindings iter_f pp_name pp_k pp_v =
    let pp_v = iter_bindings ~sep:sp iter_f (pair pp_k pp_v) in
    parens (pp_name ++ sp ++ pp_v)

  (* Stdlib data structures *)

  let list pp_elt = brackets (list ~sep:semi (box pp_elt))
  let array pp_elt = oxford_brackets (array ~sep:semi (box pp_elt))
  let seq pp_elt = brackets (seq ~sep:semi (box pp_elt))

  let hashtbl pp_k pp_v =
    iter_bindings Hashtbl.iter (any "hashtbl") pp_k pp_v

  let stack pp_elt = iter Stack.iter (any "stack") pp_elt
  let queue pp_elt = iter Queue.iter (any "queue") pp_elt

  (* Records *)

  let field ?(label = string) l prj pp_v ppf v =
    pf ppf "@[<1>%a =@ %a@]" label l pp_v (prj v)

  let record pps =
    box ~indent:2 (surround "{ " " }" @@ vbox (concat ~sep:(any ";@,") pps))
end

(* Magnitudes *)

let ilog10 x =
  let rec loop p x = if x = 0 then p else loop (p + 1) (x / 10) in
  loop (-1) x

let ipow10 n =
  let rec loop acc n = if n = 0 then acc else loop (acc * 10) (n - 1) in
  loop 1 n

let si_symb_max = 16
let si_symb =
  [| "y"; "z"; "a"; "f"; "p"; "n"; "u"; "m"; ""; "k"; "M"; "G"; "T"; "P";
     "E"; "Z"; "Y"|]

let rec pp_at_factor ~scale u symb factor ppf s =
  let m = s / factor in
  let n = s mod factor in
  match m with
  | m when m >= 100 -> (* No fractional digit *)
      let m_up = if n > 0 then m + 1 else m in
      if m_up >= 1000 then si_size ~scale u ppf (m_up * factor) else
      pf ppf "%d%s%s" m_up symb u
  | m when m >= 10 -> (* One fractional digit w.o. trailing 0 *)
      let f_factor = factor / 10 in
      let f_m = n / f_factor in
      let f_n = n mod f_factor in
      let f_m_up = if f_n > 0 then f_m + 1 else f_m in
      begin match f_m_up with
      | 0 -> pf ppf "%d%s%s" m symb u
      | f when f >= 10 -> si_size ~scale u ppf (m * factor + f * f_factor)
      | f -> pf ppf "%d.%d%s%s" m f symb u
      end
  | m -> (* Two or zero fractional digits w.o. trailing 0 *)
      let f_factor = factor / 100 in
      let f_m = n / f_factor in
      let f_n = n mod f_factor in
      let f_m_up = if f_n > 0 then f_m + 1 else f_m in
      match f_m_up with
      | 0 -> pf ppf "%d%s%s" m symb u
      | f when f >= 100 -> si_size ~scale u ppf (m * factor + f * f_factor)
      | f when f mod 10 = 0 -> pf ppf "%d.%d%s%s" m (f / 10) symb u
      | f -> pf ppf "%d.%02d%s%s" m f symb u

and si_size ~scale u ppf s = match scale < -8 || scale > 8 with
| true -> invalid_arg "~scale is %d, must be in [-8;8]" scale
| false ->
    let pow_div_3 = if s = 0 then 0 else (ilog10 s / 3) in
    let symb = (scale + 8) + pow_div_3 in
    let symb, factor = match symb > si_symb_max with
    | true -> si_symb_max, ipow10 ((8 - scale) * 3)
    | false -> symb, ipow10 (pow_div_3 * 3)
    in
    if factor = 1
    then pf ppf "%d%s%s" s si_symb.(symb) u
    else pp_at_factor ~scale u si_symb.(symb) factor ppf s

let byte_size ppf s = si_size ~scale:0 "B" ppf s

let bi_byte_size ppf s =
  (* XXX we should get rid of this. *)
  let _pp_byte_size k i ppf s =
    let pp_frac = float_dfrac 1 in
    let div_round_up m n = (m + n - 1) / n in
    let float = float_of_int in
    if s < k then pf ppf "%dB" s else
    let m = k * k in
    if s < m then begin
      let kstr = if i = "" then "k" (* SI *) else "K" (* IEC *) in
      let sk = s / k in
      if sk < 10
      then pf ppf "%a%s%sB" pp_frac (float s /. float k) kstr i
      else pf ppf "%d%s%sB" (div_round_up s k) kstr i
    end else
    let g = k * m in
    if s < g then begin
      let sm = s / m in
      if sm < 10
      then pf ppf "%aM%sB" pp_frac (float s /. float m) i
      else pf ppf "%dM%sB" (div_round_up s m) i
    end else
    let t = k * g in
    if s < t then begin
      let sg = s / g in
      if sg < 10
      then pf ppf "%aG%sB" pp_frac (float s /. float g) i
      else pf ppf "%dG%sB" (div_round_up s g) i
    end else
    let p = k * t in
    if s < p then begin
      let st = s / t in
      if st < 10
      then pf ppf "%aT%sB" pp_frac (float s /. float t) i
      else pf ppf "%dT%sB" (div_round_up s t) i
    end else begin
      let sp = s / p in
      if sp < 10
      then pf ppf "%aP%sB" pp_frac (float s /. float p) i
      else pf ppf "%dP%sB" (div_round_up s p) i
    end
  in
  _pp_byte_size 1024 "i" ppf s

(* XXX From 4.08 on use Int64.unsigned_*

   See Hacker's Delight for the implementation of these unsigned_* funs *)

let unsigned_compare x0 x1 = Int64.(compare (sub x0 min_int) (sub x1 min_int))
let unsigned_div n d = match d < Int64.zero with
| true -> if unsigned_compare n d < 0 then Int64.zero else Int64.one
| false ->
    let q = Int64.(shift_left (div (shift_right_logical n 1) d) 1) in
    let r = Int64.(sub n (mul q d)) in
    if unsigned_compare r d >= 0 then Int64.succ q else q

let unsigned_rem n d = Int64.(sub n (mul (unsigned_div n d) d))

let us_span   =                  1_000L
let ms_span   =              1_000_000L
let sec_span  =          1_000_000_000L
let min_span  =         60_000_000_000L
let hour_span =       3600_000_000_000L
let day_span  =     86_400_000_000_000L
let year_span = 31_557_600_000_000_000L

let rec pp_si_span unit_str si_unit si_higher_unit ppf span =
  let geq x y = unsigned_compare x y >= 0 in
  let m = unsigned_div span si_unit in
  let n = unsigned_rem span si_unit in
  match m with
  | m when geq m 100L -> (* No fractional digit *)
      let m_up = if Int64.equal n 0L then m else Int64.succ m in
      let span' = Int64.mul m_up si_unit in
      if geq span' si_higher_unit then uint64_ns_span ppf span' else
      pf ppf "%Ld%s" m_up unit_str
  | m when geq m 10L -> (* One fractional digit w.o. trailing zero *)
      let f_factor = unsigned_div si_unit 10L in
      let f_m = unsigned_div n f_factor in
      let f_n = unsigned_rem n f_factor in
      let f_m_up = if Int64.equal f_n 0L then f_m else Int64.succ f_m in
      begin match f_m_up with
      | 0L -> pf ppf "%Ld%s" m unit_str
      | f when geq f 10L ->
          uint64_ns_span ppf Int64.(add (mul m si_unit) (mul f f_factor))
      | f -> pf ppf "%Ld.%Ld%s" m f unit_str
      end
  | m -> (* Two or zero fractional digits w.o. trailing zero *)
      let f_factor = unsigned_div si_unit 100L in
      let f_m = unsigned_div n f_factor in
      let f_n = unsigned_rem n f_factor in
      let f_m_up = if Int64.equal f_n 0L then f_m else Int64.succ f_m in
      match f_m_up with
      | 0L -> pf ppf "%Ld%s" m unit_str
      | f when geq f 100L ->
          uint64_ns_span ppf Int64.(add (mul m si_unit) (mul f f_factor))
      | f when Int64.equal (Int64.rem f 10L) 0L ->
          pf ppf "%Ld.%Ld%s" m (Int64.div f 10L) unit_str
      | f ->
          pf ppf "%Ld.%02Ld%s" m f unit_str

and pp_non_si unit_str unit unit_lo_str unit_lo unit_lo_size ppf span =
  let geq x y = unsigned_compare x y >= 0 in
  let m = unsigned_div span unit in
  let n = unsigned_rem span unit in
  if Int64.equal n 0L then pf ppf "%Ld%s" m unit_str else
  let f_m = unsigned_div n unit_lo in
  let f_n = unsigned_rem n unit_lo in
  let f_m_up = if Int64.equal f_n 0L then f_m else Int64.succ f_m in
  match f_m_up with
  | f when geq f unit_lo_size ->
      uint64_ns_span ppf Int64.(add (mul m unit) (mul f unit_lo))
  | f ->
      pf ppf "%Ld%s%Ld%s" m unit_str f unit_lo_str

and uint64_ns_span ppf span =
  let geq x y = unsigned_compare x y >= 0 in
  let lt x y = unsigned_compare x y = -1 in
  match span with
  | s when lt s us_span -> pf ppf "%Ldns" s
  | s when lt s ms_span -> pp_si_span "us" us_span ms_span ppf s
  | s when lt s sec_span -> pp_si_span "ms" ms_span sec_span ppf s
  | s when lt s min_span -> pp_si_span "s" sec_span min_span ppf s
  | s when lt s hour_span -> pp_non_si "min" min_span "s" sec_span 60L ppf s
  | s when lt s day_span -> pp_non_si "h" hour_span "min" min_span 60L ppf s
  | s when lt s year_span -> pp_non_si "d" day_span "h" hour_span 24L ppf s
  | s ->
      let m = unsigned_div s year_span in
      let n = unsigned_rem s year_span in
      if Int64.equal n 0L then pf ppf "%Lda" m else
      let f_m = unsigned_div n day_span in
      let f_n = unsigned_rem n day_span in
      let f_m_up = if Int64.equal f_n 0L then f_m else Int64.succ f_m in
      match f_m_up with
      | f when geq f 366L -> pf ppf "%Lda" (Int64.succ m)
      | f -> pf ppf "%Lda%Ldd" m f

(* Binary formatting *)

type 'a vec = int * (int -> 'a)

let iter_vec f (n, get) = for i = 0 to n - 1 do f i (get i) done
let vec ?sep = iter_bindings ?sep iter_vec

let on_string = using String.(fun s -> length s, get s)
let on_bytes = using Bytes.(fun b -> length b, get b)

let sub_vecs w (n, get) =
  (n - 1) / w + 1,
  fun j ->
    let off = w * j in
    min w (n - off), fun i -> get (i + off)

let prefix0x = [
  0xf       , fmt "%01x";
  0xff      , fmt "%02x";
  0xfff     , fmt "%03x";
  0xffff    , fmt "%04x";
  0xfffff   , fmt "%05x";
  0xffffff  , fmt "%06x";
  0xfffffff , fmt "%07x"; ]

let padded0x ~max = match List.find_opt (fun (x, _) -> max <= x) prefix0x with
| Some (_, pp) -> pp
| None -> fmt "%08x"

let ascii ?(w = 0) ?(subst = const char '.') () ppf (n, _ as v) =
  let pp_char ppf (_, c) =
    if '\x20' <= c && c < '\x7f' then char ppf c else subst ppf ()
  in
  vec pp_char ppf v;
  if n < w then sps (w - n) ppf ()

let octets ?(w = 0) ?(sep = sp) () ppf (n, _ as v) =
  let pp_sep ppf i = if i > 0 && i mod 2 = 0 then sep ppf () in
  let pp_char ppf (i, c) = pp_sep ppf i; pf ppf "%02x" (Char.code c) in
  vec ~sep:nop pp_char ppf v;
  for i = n to w - 1 do pp_sep ppf i; sps 2 ppf () done

let addresses ?addr ?(w = 16) pp_vec ppf (n, _ as v) =
  let addr = match addr with
  | Some pp -> pp
  | _ -> padded0x ~max:(((n - 1) / w) * w) ++ const string ": "
  in
  let pp_sub ppf (i, sub) = addr ppf (i * w); box pp_vec ppf sub in
  vbox (vec pp_sub) ppf (sub_vecs w v)

let hex ?(w = 16) () =
  addresses ~w ((octets ~w () |> box) ++ sps 2 ++ (ascii ~w () |> box))

(* Text and lines *)

let is_nl c = c = '\n'
let is_nl_or_sp c = is_nl c || c = ' '
let is_white = function ' ' | '\t' .. '\r'  -> true | _ -> false
let not_white c = not (is_white c)
let not_white_or_nl c = is_nl c || not_white c

let rec stop_at sat ~start ~max s =
  if start > max then start else
  if sat s.[start] then start else
  stop_at sat ~start:(start + 1) ~max s

let sub s start stop ~max =
  if start = stop then "" else
  if start = 0 && stop > max then s else
  String.sub s start (stop - start)

let words ppf s =
  let max = String.length s - 1 in
  let rec loop start s = match stop_at is_white ~start ~max s with
  | stop when stop > max -> Format.pp_print_string ppf (sub s start stop ~max)
  | stop ->
      Format.pp_print_string ppf (sub s start stop ~max);
      match stop_at not_white ~start:stop ~max s with
      | stop when stop > max -> ()
      | stop -> Format.pp_print_space ppf (); loop stop s
  in
  let start = stop_at not_white ~start:0 ~max s in
  if start > max then () else loop start s

let paragraphs ppf s =
  let max = String.length s - 1 in
  let rec loop start s = match stop_at is_white ~start ~max s with
  | stop when stop > max -> Format.pp_print_string ppf (sub s start stop ~max)
  | stop ->
      Format.pp_print_string ppf (sub s start stop ~max);
      match stop_at not_white_or_nl ~start:stop ~max s with
      | stop when stop > max -> ()
      | stop ->
          if s.[stop] <> '\n'
          then (Format.pp_print_space ppf (); loop stop s) else
          match stop_at not_white_or_nl ~start:(stop + 1) ~max s with
          | stop when stop > max -> ()
          | stop ->
              if s.[stop] <> '\n'
              then (Format.pp_print_space ppf (); loop stop s) else
              match stop_at not_white ~start:(stop + 1) ~max s with
              | stop when stop > max -> ()
              | stop ->
                  Format.pp_force_newline ppf ();
                  Format.pp_force_newline ppf ();
                  loop stop s
  in
  let start = stop_at not_white ~start:0 ~max s in
  if start > max then () else loop start s

let text ppf s =
  let max = String.length s - 1 in
  let rec loop start s = match stop_at is_nl_or_sp ~start ~max s with
  | stop when stop > max -> Format.pp_print_string ppf (sub s start stop ~max)
  | stop ->
      Format.pp_print_string ppf (sub s start stop ~max);
      begin match s.[stop] with
      | ' ' -> Format.pp_print_space ppf ()
      | '\n' -> Format.pp_force_newline ppf ()
      | _ -> assert false
      end;
      loop (stop + 1) s
  in
  loop 0 s

let lines ppf s =
  let max = String.length s - 1 in
  let rec loop start s = match stop_at is_nl ~start ~max s with
  | stop when stop > max -> Format.pp_print_string ppf (sub s start stop ~max)
  | stop ->
      Format.pp_print_string ppf (sub s start stop ~max);
      Format.pp_force_newline ppf ();
      loop (stop + 1) s
  in
  loop 0 s

let truncated ~max ppf s = match String.length s <= max with
| true -> Format.pp_print_string ppf s
| false ->
    for i = 0 to max - 4 do Format.pp_print_char ppf s.[i] done;
    Format.pp_print_string ppf "..."

let text_loc ppf ((l0, c0), (l1, c1)) =
  if (l0 : int) == (l1 : int) && (c0 : int) == (c1 : int)
  then pf ppf "%d.%d" l0 c0
  else pf ppf "%d.%d-%d.%d" l0 c0 l1 c1

(* HCI fragments *)

let one_of ?(empty = nop) pp_v ppf = function
| [] -> empty ppf ()
| [v] -> pp_v ppf v
| [v0; v1] -> pf ppf "@[either %a or@ %a@]" pp_v v0 pp_v v1
| _ :: _ as vs ->
    let rec loop ppf = function
    | [v] -> pf ppf "or@ %a" pp_v v
    | v :: vs -> pf ppf "%a,@ " pp_v v; loop ppf vs
    | [] -> assert false
    in
    pf ppf "@[one@ of@ %a@]" loop vs

let did_you_mean
    ?(pre = any "Unknown") ?(post = nop) ~kind pp_v ppf (v, hints)
  =
  match hints with
  | [] -> pf ppf "@[%a %s %a%a.@]" pre () kind pp_v v post ()
  | hints ->
      pf ppf "@[%a %s %a%a.@ Did you mean %a ?@]"
        pre () kind pp_v v post () (one_of pp_v) hints

(* Conditional UTF-8 and styled formatting. *)

module Imap = Map.Make (Int)

type 'a attr = int * ('a -> string) * (string -> 'a)
let id = ref 0
let attr (type a) enc dec = incr id; (!id, enc, dec)

type Format.stag +=
| Fmt_store_get : 'a attr -> Format.stag
| Fmt_store_set : 'a attr * 'a -> Format.stag

let store () =
  let s = ref Imap.empty in
  fun ~other -> function
  | Fmt_store_get (id, _, _) -> Option.value ~default:"" (Imap.find_opt id !s)
  | Fmt_store_set ((id, enc, _), v) -> s := Imap.add id (enc v) !s; "ok"
  | stag -> other stag

let setup_store ppf =
  let funs = Format.pp_get_formatter_stag_functions ppf () in
  let mark_open_stag = store () ~other:funs.mark_open_stag in
  Format.pp_set_formatter_stag_functions ppf { funs with mark_open_stag }

let store_op op ppf =
  let funs = Format.pp_get_formatter_stag_functions ppf () in
  funs.mark_open_stag op

let get (_, _, dec as attr) ppf = match store_op (Fmt_store_get attr) ppf with
| "" -> None | s -> Some (dec s)

let rec set attr v ppf = match store_op (Fmt_store_set (attr, v)) ppf with
| "ok" -> () | _ -> setup_store ppf; set attr v ppf

let def x = function Some y -> y | _ -> x

let utf_8_attr =
  let enc = function true -> "t" | false -> "f" in
  let dec = function "t" -> true | "f" -> false | _ -> assert false in
  attr enc dec

let utf_8 ppf = get utf_8_attr ppf |> def true
let set_utf_8 ppf x = set utf_8_attr x ppf

type style_renderer = [ `Ansi_tty | `None ]
let style_renderer_attr =
  let enc = function `Ansi_tty -> "A" | `None -> "N" in
  let dec = function "A" -> `Ansi_tty | "N" -> `None | _ -> assert false in
  attr enc dec

let style_renderer ppf = get style_renderer_attr ppf |> def `None
let set_style_renderer ppf x = set style_renderer_attr x ppf

let with_buffer ?like buf =
  let ppf = Format.formatter_of_buffer buf in
  (* N.B. this does slighty more it also makes buf use other installed
     semantic tag actions. *)
  match like with
  | None -> ppf
  | Some like ->
      let funs = Format.pp_get_formatter_stag_functions like () in
      Format.pp_set_formatter_stag_functions ppf funs;
      ppf

let str_like ppf fmt =
  let buf = Buffer.create 64 in
  let bppf = with_buffer ~like:ppf buf in
  let flush ppf =
    Format.pp_print_flush ppf ();
    let s = Buffer.contents buf in
    Buffer.reset buf; s
  in
  Format.kfprintf flush bppf fmt

(* Conditional UTF-8 formatting *)

let if_utf_8 pp_u pp = fun ppf v -> (if utf_8 ppf then pp_u else pp) ppf v

(* Styled formatting *)

type color =
  [ `Black | `Blue | `Cyan | `Green | `Magenta | `Red | `White | `Yellow ]

type style =
  [ `None |  `Bold | `Faint | `Italic | `Underline | `Reverse
  | `Fg of [ color | `Hi of color ]
  | `Bg of [ color | `Hi of color ]
  | color (** deprecated *) ]

let ansi_style_code = function
| `Bold -> "1"
| `Faint -> "2"
| `Italic -> "3"
| `Underline -> "4"
| `Reverse -> "7"
| `Fg `Black -> "30"
| `Fg `Red -> "31"
| `Fg `Green -> "32"
| `Fg `Yellow -> "33"
| `Fg `Blue -> "34"
| `Fg `Magenta -> "35"
| `Fg `Cyan -> "36"
| `Fg `White -> "37"
| `Bg `Black -> "40"
| `Bg `Red -> "41"
| `Bg `Green -> "42"
| `Bg `Yellow -> "43"
| `Bg `Blue -> "44"
| `Bg `Magenta -> "45"
| `Bg `Cyan -> "46"
| `Bg `White -> "47"
| `Fg (`Hi `Black) -> "90"
| `Fg (`Hi `Red) -> "91"
| `Fg (`Hi `Green) -> "92"
| `Fg (`Hi `Yellow) -> "93"
| `Fg (`Hi `Blue) -> "94"
| `Fg (`Hi `Magenta) -> "95"
| `Fg (`Hi `Cyan) -> "96"
| `Fg (`Hi `White) -> "97"
| `Bg (`Hi `Black) -> "100"
| `Bg (`Hi `Red) -> "101"
| `Bg (`Hi `Green) -> "102"
| `Bg (`Hi `Yellow) -> "103"
| `Bg (`Hi `Blue) -> "104"
| `Bg (`Hi `Magenta) -> "105"
| `Bg (`Hi `Cyan) -> "106"
| `Bg (`Hi `White) -> "107"
| `None -> "0"
(* deprecated *)
| `Black -> "30"
| `Red -> "31"
| `Green -> "32"
| `Yellow -> "33"
| `Blue -> "34"
| `Magenta -> "35"
| `Cyan -> "36"
| `White -> "37"

let pp_sgr ppf style =
  Format.pp_print_as ppf 0 "\027[";
  Format.pp_print_as ppf 0 style;
  Format.pp_print_as ppf 0 "m"

let curr_style = attr Fun.id Fun.id

let styled style pp_v ppf v = match style_renderer ppf with
| `None -> pp_v ppf v
| `Ansi_tty ->
    let prev = match get curr_style ppf with
    | None -> let zero = "0" in set curr_style zero ppf; zero
    | Some s -> s
    in
    let here = ansi_style_code style in
    let curr = match style with
    | `None -> here
    | _ -> String.concat ";" [prev; here]
    in
    let finally () = set curr_style prev ppf in
    set curr_style curr ppf;
    Fun.protect ~finally @@ fun () ->
    pp_sgr ppf here; pp_v ppf v; pp_sgr ppf prev

(* Records *)

let id = Fun.id
let label = styled (`Fg `Yellow) string
let field ?(label = label) ?(sep = any ":@ ") l prj pp_v ppf v =
  pf ppf "@[<1>%a%a%a@]" label l sep () pp_v (prj v)

let record ?(sep = cut) pps = vbox (concat ~sep pps)

(* Converting with string converters. *)

let of_to_string f ppf v = string ppf (f v)
let to_to_string pp_v v = str "%a" pp_v v

(* Deprecated *)

let strf = str
let kstrf = kstr
let strf_like = str_like
let always = any
let unit = any
let prefix pp_p pp_v ppf v = pp_p ppf (); pp_v ppf v
let suffix pp_s pp_v ppf v = pp_v ppf v; pp_s ppf ()
let styled_unit style fmt = styled style (any fmt)

(*---------------------------------------------------------------------------
   Copyright (c) 2014 The fmt programmers

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
