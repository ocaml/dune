(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf

(* Unique ids *)

let uid =
  (* Thread-safe UIDs, Oo.id (object end) was used before.
     Note this won't be thread-safe in multicore, we should use
     Atomic but this is >= 4.12 and we have 4.08 for now. *)
  let c = ref 0 in
  fun () ->
    let id = !c in
    incr c; if id > !c then assert false (* too many ids *) else id

(* Edit distance

   The stdlib has much better in but this will be only >= 5.4, maybe
   in twenty years. *)

let edit_distance s0 s1 =
  let minimum (a : int) (b : int) (c : int) : int = min a (min b c) in
  let s0,s1 = if String.length s0 <= String.length s1 then s0,s1 else s1,s0 in
  let m = String.length s0 and n = String.length s1 in
  let rec rows row0 row i = match i > n with
  | true -> row0.(m)
  | false ->
      row.(0) <- i;
      for j = 1 to m do
        if s0.[j - 1] = s1.[i - 1] then row.(j) <- row0.(j - 1) else
        row.(j) <- minimum (row0.(j - 1) + 1) (row0.(j) + 1) (row.(j - 1) + 1)
      done;
      rows row row0 (i + 1)
  in
  rows (Array.init (m + 1) (fun x -> x)) (Array.make (m + 1) 0) 1

let suggest s candidates =
  let add (min, acc) name =
    let d = edit_distance s name in
    if d = min then min, (name :: acc) else
    if d < min then d, [name] else
    min, acc
  in
  let dist, suggs = List.fold_left add (max_int, []) candidates in
  if dist < 3 (* suggest only if not too far *) then suggs else []

(* Stdlib compatibility *)

let is_space = function ' ' | '\n' | '\r' | '\t' -> true | _ -> false

let string_starts_with ~prefix s = (* available in 4.13 *)
  let prefix_len = String.length prefix in
  let s_len = String.length s in
  if prefix_len > s_len then false else
  let rec loop i =
    if i = prefix_len then true
    else if String.get prefix i = String.get s i then loop (i + 1)
    else false
  in
  loop 0

let string_drop_first n s =
  if n <= 0 then s else
  if n >= String.length s then "" else
  String.sub s n (String.length s - n)

(* Invalid argument strings *)

let err_empty_list = "empty list"

(* Formatting tools *)

module Fmt = struct
  type 'a t = Format.formatter -> 'a -> unit
  let str = Format.asprintf
  let pf = Format.fprintf
  let nop ppf _ = ()
  let sp = Format.pp_print_space
  let cut = Format.pp_print_cut
  let string = Format.pp_print_string
  let char = Format.pp_print_char
  let comma ppf () = char ppf ','; sp ppf ()
  let indent ppf c = for i = 1 to c do char ppf ' ' done
  let list ?sep pp_v ppf l = Format.pp_print_list ?pp_sep:sep pp_v ppf l
  let text = Format.pp_print_text
  let lines ppf s =
    let rec stop_at sat ~start ~max s =
      if start > max then start else
      if sat s.[start] then start else
      stop_at sat ~start:(start + 1) ~max s
    in
    let sub s start stop ~max =
      if start = stop then "" else
      if start = 0 && stop > max then s else
      String.sub s start (stop - start)
    in
    let is_nl c = c = '\n' in
    let max = String.length s - 1 in
    let rec loop start s = match stop_at is_nl ~start ~max s with
    | stop when stop > max -> Format.pp_print_string ppf (sub s start stop ~max)
    | stop ->
        Format.pp_print_string ppf (sub s start stop ~max);
        Format.pp_force_newline ppf ();
        loop (stop + 1) s
    in
    loop 0 s

  let tokens ~spaces ppf s = (* collapse white and hint spaces (maybe) *)
    let i_max = String.length s - 1 in
    let flush start stop = string ppf (String.sub s start (stop - start + 1)) in
    let rec skip_white i =
      if i > i_max then i else
      if is_space s.[i] then skip_white (i + 1) else i
    in
    let rec loop start i =
      if i > i_max then flush start i_max else
      if not (is_space s.[i]) then loop start (i + 1) else
      let next_start = skip_white i in
      (flush start (i - 1); if spaces then sp ppf () else char ppf ' ';
       if next_start > i_max then () else loop next_start next_start)
    in
    loop 0 0

  (* Text styling *)

  type styler = Ansi | Plain
  let styler' =
    ref begin match Sys.getenv_opt "NO_COLOR" with
    | Some s when s <> "" -> Plain
    | _ ->
        match Sys.getenv_opt "TERM" with
        | Some "dumb" -> Plain
        | None when Sys.backend_type <> Other "js_of_ocaml" -> Plain
        | _ -> Ansi
    end

  let set_styler styler = styler' := styler
  let styler () = !styler'

  let sgr_of_style = function
  | `Bold -> "01"
  | `Underline -> "04"
  | `Fg `Red -> string_of_int (30 + 1)
  | `Fg `Yellow -> string_of_int (30 + 3)

  let sgrs_of_styles styles = String.concat ";" (List.map sgr_of_style styles)
  let ansi_esc = "\x1B["
  let sgr_reset = "\x1B[m"

  let ansi styles ppf s =
    let sgrs = String.concat "" [ansi_esc; sgrs_of_styles styles; "m"] in
    Format.pp_print_as ppf 0 sgrs;
    string ppf s;
    Format.pp_print_as ppf 0 sgr_reset

  let st styles ppf s = match !styler' with
  | Plain -> string ppf s
  | Ansi -> ansi styles ppf s

  let code ppf v = st [`Bold] ppf v
  let code_var ppf v = st [`Underline] ppf v
  let code_or_quote ppf v = match !styler' with
  | Plain -> char ppf '\''; string ppf v; char ppf '\''
  | Ansi -> ansi [`Bold] ppf v

  let ereason ppf s = match !styler' with
  | Plain -> string ppf s
  | Ansi -> ansi [`Fg `Red] ppf s

  let wreason ppf s = match !styler' with
  | Plain -> string ppf s
  | Ansi -> ansi [`Fg `Yellow] ppf s

  let missing ppf () = ereason ppf "missing"
  let invalid ppf () = ereason ppf "invalid"
  let unknown ppf () = ereason ppf "unknown"
  let deprecated ppf () = wreason ppf "deprecated"

  let puterr ppf () = st [`Bold; `Fg `Red] ppf "Error"; char ppf ':'

  let styled_text ppf s =
    (* Detects ANSI escapes and prints them as 0 width. Collapses spaces
       and newlines to single space except for blank lines which are
       preserved. *)
    let rec loop ppf s i max =
      if i > max then () else
      let ansi = s.[i] = '\x1B' && i + 1 < max && s.[i+1] = '[' in
      if not ansi then match s.[i] with
      | ' ' when i = max || s.[i+1] = ' ' || s.[i+1] = '\n' ->
          loop ppf s (i + 1) max
      | ' ' -> sp ppf (); loop ppf s (i + 1) max
      | '\n' when i = max || s.[i+1] = ' ' -> loop ppf s (i + 1) max
      | '\n' when s.[i+1] = '\n' ->
          Format.pp_force_newline ppf ();
          if i > 0 && s.[i-1] <> '\n' then Format.pp_force_newline ppf ();
          loop ppf s (i + 1) max
      | '\n' -> sp ppf (); loop ppf s (i + 1) max
      | c -> char ppf s.[i]; loop ppf s (i + 1) max
      else begin
        let k = ref (i + 2) in
        while (!k <= max && s.[!k] <> 'm') do incr k done;
        let esc = String.sub s i (!k - i + 1) in
        Format.pp_print_as ppf 0 esc;
        loop ppf s (!k + 1) max
      end
    in
    loop ppf s 0 (String.length s - 1)
end

(* Converter (end-user) error messages *)

let err_multi_def ~kind name doc v v' = (* programming error *)
  strf "%s %s defined twice (doc strings are '%s' and '%s')"
    kind name (doc v) (doc v')

let quote s = strf "'%s'" s (* Exposed in the API do not change *)
let _alts_str ~styled ?quoted ppf alts =
  let quote = match quoted with
  | None -> fun ppf s -> Fmt.pf ppf "$(b,%s)" s
  | Some quoted ->
      if not quoted then Fmt.string else
      if styled then Fmt.code_or_quote else
      fun ppf s -> Fmt.pf ppf "'%s'" s
  in
  match alts with
  | [] -> invalid_arg err_empty_list
  | [a] -> quote ppf a
  | [a; b] -> Fmt.pf ppf "either@ %a@ or@ %a" quote a quote b
  | alts ->
      let rev_alts = List.rev alts in
      Fmt.pf ppf "one@ of@ %a@ or@ %a"
        Fmt.(list ~sep:comma quote) (List.rev (List.tl rev_alts))
        quote (List.hd rev_alts)

let alts_str ?quoted alts = (* Exposed in the API do not change *)
  Fmt.str "@[%a@]" (_alts_str ~styled:false ?quoted) alts

let pp_alts ppf alts =
  _alts_str ~styled:true ~quoted:true ppf alts

let err_ambiguous ~kind s ~ambs =
  Fmt.str "@[%s %a %a@ and@ could@ be@ %a@]"
    kind Fmt.code_or_quote s Fmt.ereason "ambiguous" pp_alts ambs

let err_unknown ?(dom = []) ?(hints = []) ~kind v =
  let hints ppf () = match hints, dom with
  | [], [] -> ()
  | [], dom -> Fmt.pf ppf ". Must@ be@ %a" pp_alts dom
  | hints, _ -> Fmt.pf ppf ". Did@ you@ mean@ %a?" pp_alts hints
  in
  Fmt.str "@[%a %s@ %a%a@]" Fmt.unknown () kind Fmt.code_or_quote v hints ()
