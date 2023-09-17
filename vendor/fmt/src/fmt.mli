(*---------------------------------------------------------------------------
   Copyright (c) 2014 The fmt programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** {!Format} pretty-printer combinators.

    Consult {{!nameconv}naming conventions} for your pretty-printers.

    {b References}
    {ul
    {- The {!Format} module documentation.}
    {- The required reading {!Format} module
       {{:https://ocaml.org/learn/tutorials/format.html}tutorial}.}} *)

(** {1:stdos Standard outputs} *)

val stdout : Format.formatter
(** [stdout] is the standard output formatter. *)

val stderr : Format.formatter
(** [stderr] is the standard error formatter. *)

(** {1:formatting Formatting} *)

val pf : Format.formatter -> ('a, Format.formatter, unit) Stdlib.format -> 'a
(** [pf] is {!Format.fprintf}. *)

val pr : ('a, Format.formatter, unit) format -> 'a
(** [pr] is [pf stdout]. *)

val epr : ('a, Format.formatter, unit) format -> 'a
(** [epr] is [pf stderr]. *)

val str : ('a, Format.formatter, unit, string) format4 -> 'a
(** [str] is {!Format.asprintf}.

    {b Note.} When using [str] {!utf_8} and {!val-style_renderer} are
    always respectively set to [true] and [`None]. See also
    {!str_like}. *)

val kpf : (Format.formatter -> 'a) -> Format.formatter ->
  ('b, Format.formatter, unit, 'a) Stdlib.format4 -> 'b
(** [kpf] is {!Format.kfprintf}. *)

val kstr :
  (string -> 'a) -> ('b, Format.formatter, unit, 'a) format4 -> 'b
(** [kstr] is like {!str} but continuation based. *)

val str_like :
  Format.formatter -> ('a, Format.formatter, unit, string) format4 -> 'a
(** [str_like ppf] is like {!str} except its {!utf_8} and {!val-style_renderer}
    settings are those of [ppf]. *)

val with_buffer : ?like:Format.formatter -> Buffer.t -> Format.formatter
(** [with_buffer ~like b] is a formatter whose {!utf_8} and
    {!val-style_renderer} settings are copied from those of [like]
    (if provided). *)

val failwith : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [failwith] is [kstr failwith], raises {!Stdlib.Failure} with
    a pretty-printed string argument. *)

val failwith_notrace : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [failwith_notrace] is like {!failwith} but raises with {!raise_notrace}. *)

val invalid_arg : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [invalid_arg] is [kstr invalid_arg], raises
    {!Stdlib.Invalid_argument} with a pretty-printed string argument. *)

val error : ('b, Format.formatter , unit, ('a, string) result) format4 -> 'b
(** [error fmt ...] is [kstr (fun s -> Error s) fmt ...] *)

val error_msg :
  ('b, Format.formatter , unit, ('a, [> `Msg of string]) result) format4 -> 'b
(** [error_msg fmt ...] is [kstr (fun s -> Error (`Msg s)) fmt ...] *)

(** {1 Formatters} *)

type 'a t = Format.formatter -> 'a -> unit
(** The type for formatters of values of type ['a]. *)

val flush : 'a t
(** [flush] has the effect of {!Format.pp_print_flush} *)

val nop : 'a t
(** [nop] formats nothing. *)

val any : (unit, Format.formatter, unit) Stdlib.format -> 'a t
(** [any fmt ppf v] formats any value with the constant format [fmt]. *)

val using : ('a -> 'b) -> 'b t -> 'a t
(** [using f pp ppf v] ppf ppf [(f v)]. *)

val const : 'a t -> 'a -> 'b t
(** [const pp_v v] always formats [v] using [pp_v]. *)

val fmt : ('a, Format.formatter, unit) Stdlib.format -> Format.formatter -> 'a
(** [fmt fmt ppf] is [pf ppf fmt]. If [fmt] is used with a single
    non-constant formatting directive, generates a value of type
    {!t}. *)

(** {1:seps Separators} *)

val cut : 'a t
(** [cut] has the effect of {!Format.pp_print_cut}. *)

val sp : 'a t
(** [sp] has the effect of {!Format.pp_print_space}. *)

val sps : int -> 'a t
(** [sps n] has the effect of {!Format.pp_print_break}[ n 0]. *)

val comma : 'a t
(** [comma] is {!Fmt.any}[ ",@ "]. *)

val semi : 'a t
(** [semi] is {!Fmt.any}[ ";@ "]. *)

(** {1:seq Sequencing} *)

val append : 'a t -> 'a t -> 'a t
(** [append pp_v0 pp_v1 ppf v] is [pp_v0 ppf v; pp_v1 ppf v]. *)

val ( ++ ) : 'a t -> 'a t -> 'a t
(** [( ++ )] is {!append}. *)

val concat : ?sep:unit t -> 'a t list -> 'a t
(** [concat ~sep pps] formats a value using the formaters [pps]
    and separting each format with [sep] (defaults to {!cut}). *)

val iter : ?sep:unit t -> (('a -> unit) -> 'b -> unit) -> 'a t -> 'b t
(** [iter ~sep iter pp_elt] formats the iterations of [iter] over a
    value using [pp_elt]. Iterations are separated by [sep] (defaults to
    {!cut}). *)

val iter_bindings : ?sep:unit t -> (('a -> 'b -> unit) -> 'c -> unit) ->
  ('a * 'b) t -> 'c t
(** [iter_bindings ~sep iter pp_binding] formats the iterations of
    [iter] over a value using [pp_binding]. Iterations are separated
    by [sep] (defaults to {!cut}). *)

(** {1:boxes Boxes} *)

val box : ?indent:int -> 'a t -> 'a t
(** [box ~indent pp ppf] wraps [pp] in a pretty-printing box. The box tries to
    print as much as possible on every line, while emphasizing the box structure
    (see {!Format.pp_open_box}). Break hints that lead to a new line add
    [indent] to the current indentation (defaults to [0]). *)

val hbox : 'a t -> 'a t
(** [hbox] is like {!box} but is a horizontal box: the line is not split
    in this box (but may be in sub-boxes). See {!Format.pp_open_hbox}. *)

val vbox : ?indent:int -> 'a t -> 'a t
(** [vbox] is like {!box} but is a vertical box: every break hint leads
    to a new line which adds [indent] to the current indentation
    (defaults to [0]). See {!Format.pp_open_vbox}. *)

val hvbox : ?indent:int -> 'a t -> 'a t
(** [hvbox] is like {!hbox} if it fits on a single line, or like {!vbox}
    otherwise. See {!Format.pp_open_hvbox}. *)

val hovbox : ?indent:int -> 'a t -> 'a t
(** [hovbox] is a condensed {!box}. See {!Format.pp_open_hovbox}. *)

(** {1:bracks Brackets} *)

val parens : 'a t -> 'a t
(** [parens pp_v ppf] is [pf "@[<1>(%a)@]" pp_v]. *)

val brackets : 'a t -> 'a t
(** [brackets pp_v ppf] is [pf "@[<1>[%a]@]" pp_v]. *)

val braces : 'a t -> 'a t
(** [braces pp_v ppf] is [pf "@[<1>{%a}@]" pp_v]. *)

val quote : ?mark:string -> 'a t -> 'a t
(** [quote ~mark pp_v ppf] is [pf "@[<1>@<1>%s%a@<1>%s@]" mark pp_v mark],
    [mark] defaults to ["\""], it is always counted as spanning as single
    column (this allows for UTF-8 encoded marks). *)

(** {1:records Records} *)

val id : 'a -> 'a
(** [id] is {!Fun.id}. *)

val field :
  ?label:string t -> ?sep:unit t -> string -> ('b -> 'a) -> 'a t -> 'b t
(** [field ~label ~sep l prj pp_v] pretty prints a labelled field value as
    [pf "@[<1>%a%a%a@]" label l sep () (using prj pp_v)]. [label] defaults
    to [styled `Yellow string] and [sep] to [any ":@ "]. *)

val record : ?sep:unit t -> 'a t list -> 'a t
(** [record ~sep fields] pretty-prints a value using the concatenation of
    [fields], separated by [sep] (defaults to [cut]) and framed in a vertical
    box. *)

(** {1:stdlib Stdlib types}

    Formatters for structures give full control to the client over the
    formatting process and do not wrap the formatted structures with
    boxes. Use the {!Dump} module to quickly format values for
    inspection.  *)

val bool : bool t
(** [bool] is {!Format.pp_print_bool}. *)

val int : int t
(** [int ppf] is [pf ppf "%d"]. *)

val nativeint : nativeint t
(** [nativeint ppf] is [pf ppf "%nd"]. *)

val int32 : int32 t
(** [int32 ppf] is [pf ppf "%ld"]. *)

val int64 : int64 t
(** [int64 ppf] is [pf ppf "%Ld"]. *)

val uint : int t
(** [uint ppf] is [pf ppf "%u"]. *)

val unativeint : nativeint t
(** [unativeint ppf] is [pf ppf "%nu"]. *)

val uint32 : int32 t
(** [uint32 ppf] is [pf ppf "%lu"]. *)

val uint64 : int64 t
(** [uint64 ppf] is [pf ppf "%Lu"]. *)

val float : float t
(** [float ppf] is [pf ppf "%g".] *)

val float_dfrac : int -> float t
(** [float_dfrac d] rounds the float to the [d]th {e decimal}
    fractional digit and formats the result with ["%g"]. Ties are
    rounded towards positive infinity. The result is only defined
    for [0 <= d <= 16]. *)

val float_dsig : int -> float t
(** [float_dsig d] rounds the normalized {e decimal} significand
    of the float to the [d]th decimal fractional digit and formats
    the result with ["%g"]. Ties are rounded towards positive
    infinity. The result is NaN on infinities and only defined for
    [0 <= d <= 16].

    {b Warning.} The current implementation overflows on large [d]
    and floats. *)

val char : char t
(** [char] is {!Format.pp_print_char}. *)

val string : string t
(** [string] is {!Format.pp_print_string}. *)

val buffer : Buffer.t t
(** [buffer] formats a {!Buffer.t} value's current contents. *)

val exn : exn t
(** [exn] formats an exception. *)

val exn_backtrace : (exn * Printexc.raw_backtrace) t
(** [exn_backtrace] formats an exception backtrace. *)

val pair : ?sep:unit t -> 'a t -> 'b t -> ('a * 'b) t
(** [pair ~sep pp_fst pp_snd] formats a pair. The first and second
    projection are formatted using [pp_fst] and [pp_snd] and are
    separated by [sep] (defaults to {!cut}). *)

val option : ?none:unit t -> 'a t -> 'a option t
(** [option ~none pp_v] formats an optional value. The [Some] case
    uses [pp_v] and [None] uses [none] (defaults to {!nop}). *)

val result : ok:'a t -> error:'b t -> ('a, 'b) result t
(** [result ~ok ~error] formats a result value using [ok] for the [Ok]
    case and [error] for the [Error] case. *)

val list : ?sep:unit t -> 'a t -> 'a list t
(** [list sep pp_v] formats list elements. Each element of the list is
    formatted in order with [pp_v]. Elements are separated by [sep]
    (defaults to {!cut}). If the list is empty, this is {!nop}. *)

val array : ?sep:unit t -> 'a t -> 'a array t
(** [array sep pp_v] formats array elements. Each element of the array
    is formatted in order with [pp_v]. Elements are separated by [sep]
    (defaults to {!cut}). If the array is empty, this is {!nop}. *)

val seq : ?sep:unit t -> 'a t -> 'a Seq.t t
(** [seq sep pp_v] formats sequence elements. Each element of the sequence
    is formatted in order with [pp_v]. Elements are separated by [sep]
    (defaults to {!cut}). If the sequence is empty, this is {!nop}. *)

val hashtbl : ?sep:unit t -> ('a * 'b) t -> ('a, 'b) Hashtbl.t t
(** [hashtbl ~sep pp_binding] formats the bindings of a hash
    table. Each binding is formatted with [pp_binding] and bindings
    are separated by [sep] (defaults to {!cut}). If the hash table has
    multiple bindings for a given key, all bindings are formatted,
    with the most recent binding first. If the hash table is empty,
    this is {!nop}. *)

val queue : ?sep:unit t -> 'a t -> 'a Queue.t t
(** [queue ~sep pp_v] formats queue elements. Each element of the
    queue is formatted in least recently added order with
    [pp_v]. Elements are separated by [sep] (defaults to {!cut}). If
    the queue is empty, this is {!nop}. *)

val stack : ?sep:unit t -> 'a t -> 'a Stack.t t
(** [stack ~sep pp_v] formats stack elements. Each element of the
    stack is formatted from top to bottom order with [pp_v].  Elements
    are separated by [sep] (defaults to {!cut}). If the stack is
    empty, this is {!nop}. *)

(** Formatters for inspecting OCaml values.

    Formatters of this module dump OCaml value with little control
    over the representation but with good default box structures and,
    whenever possible, using OCaml syntax. *)
module Dump : sig

  (** {1:stdlib Stdlib types} *)

  val signal : int t
  (** [signal] formats an OCaml {{!Sys.sigabrt}signal number} as a C
      POSIX
      {{:http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/signal.h.html}
      constant} or ["SIG(%d)"] the signal number is unknown. *)

  val uchar : Uchar.t t
  (** [uchar] formats an OCaml {!Uchar.t} value using only US-ASCII
      encoded characters according to the Unicode
      {{:http://www.unicode.org/versions/latest/appA.pdf}notational
      convention} for code points. *)

  val string : string t
  (** [string] is [pf ppf "%S"]. *)

  val pair : 'a t -> 'b t -> ('a * 'b) t
  (** [pair pp_fst pp_snd] formats an OCaml pair using [pp_fst] and [pp_snd]
      for the first and second projection. *)

  val option : 'a t -> 'a option t
  (** [option pp_v] formats an OCaml option using [pp_v] for the [Some]
      case. No parentheses are added. *)

  val result : ok:'a t -> error:'b t -> ('a, 'b) result t
  (** [result ~ok ~error] formats an OCaml result using [ok] for the [Ok]
      case value and [error] for the [Error] case value. No parentheses
      are added. *)

  val list : 'a t -> 'a list t
  (** [list pp_v] formats an OCaml list using [pp_v] for the list
      elements. *)

  val array : 'a t -> 'a array t
  (** [array pp_v] formats an OCaml array using [pp_v] for the array
      elements. *)

  val seq : 'a t -> 'a Seq.t t
  (** [seq pp_v] formats an OCaml sequence using [pp_v] for the sequence
      elements. *)

  val hashtbl : 'a t -> 'b t -> ('a, 'b) Hashtbl.t t
  (** [hashtbl pp_k pp_v] formats an unspecified representation of the
      bindings of a hash table using [pp_k] for the keys and [pp_v]
      for the values. If the hash table has multiple bindings for a
      given key, all bindings are formatted, with the most recent
      binding first. *)

  val queue : 'a t -> 'a Queue.t t
  (** [queue pp_v] formats an unspecified representation of an OCaml
      queue using [pp_v] to format its elements, in least recently added
      order. *)

  val stack : 'a t -> 'a Stack.t t
  (** [stack pp_v] formats an unspecified representation of an OCaml
      stack using [pp_v] to format its elements in top to bottom order. *)

  (** {1:record Records} *)

  val field : ?label:string t -> string -> ('b -> 'a) -> 'a t -> 'b t
  (** [field ~label l prj pp_v] pretty prints a named field using [label]
      (defaults to [styled `Yellow string]) for the label, and [using prj pp_v]
      for the field value. *)

  val record : 'a t list -> 'a t
  (** [record fields] pretty-prints a value using the concatenation of
      [fields], separated by [";@,"], framed in a vertical
      box and surrounded by {!braces}. *)

  (** {1:seq Sequencing}

      These are akin to {!iter} and {!iter_bindings} but
      delimit the sequences with {!parens}. *)

  val iter : (('a -> unit) -> 'b -> unit) -> 'b t -> 'a t -> 'b t
  (** [iter iter pp_name pp_elt] formats an unspecified representation
      of the iterations of [iter] over a value using [pp_elt]. The
      iteration is named by [pp_name]. *)

  val iter_bindings : (('a -> 'b -> unit) -> 'c -> unit) -> 'c t -> 'a t
    -> 'b t -> 'c t
  (** [iter_bindings ~sep iter pp_name pp_k pp_v] formats an
      unspecified representation of the iterations of [iter] over a
      value using [pp_k] and [pp_v]. The iteration is named by
      [pp_name]. *)
end

(** {1:mgs Magnitudes} *)

val si_size : scale:int -> string -> int t
(** [si_size ~scale unit] formats a non negative integer
    representing unit [unit] at scale 10{^scale * 3}, depending on
    its magnitude, using power of 3
    {{:https://www.bipm.org/en/publications/si-brochure/chapter3.html}
    SI prefixes} (i.e. all of them except deca, hector, deci and
    centi). Only US-ASCII characters are used, [µ] (10{^-6}) is
    written using [u].

    [scale] indicates the scale 10{^scale * 3} an integer
    represents, for example [-1] for m[unit] (10{^-3}), [0] for
    [unit] (10{^0}), [1] for [kunit] (10{^3}); it must be in the
    range \[[-8];[8]\] or [Invalid_argument] is raised.

    Except at the maximal yotta scale always tries to show three
    digits of data with trailing fractional zeros omited. Rounds
    towards positive infinity (over approximates).  *)

val byte_size : int t
(** [byte_size] is [si_size ~scale:0 "B"]. *)

val bi_byte_size : int t
(** [bi_byte_size] formats a byte size according to its magnitude
    using {{:https://en.wikipedia.org/wiki/Binary_prefix}binary prefixes}
    up to pebi bytes (2{^15}). *)

val uint64_ns_span : int64 t
(** [uint64_ns_span] formats an {e unsigned} nanosecond time span
    according to its magnitude using
    {{:http://www.bipm.org/en/publications/si-brochure/chapter3.html}SI
    prefixes} on seconds and
    {{:http://www.bipm.org/en/publications/si-brochure/table6.html}accepted
    non-SI units}. Years are counted in Julian years (365.25 SI-accepted days)
    as {{:http://www.iau.org/publications/proceedings_rules/units/}defined}
    by the International Astronomical Union (IAU). Only US-ASCII characters
    are used ([us] is used for [µs]). *)

(** {1:binary Binary data} *)

type 'a vec = int * (int -> 'a)
(** The type for random addressable, sized sequences. Each [(n, f)]
    represents the sequence [f 0, ..., f (n - 1)]. *)

val on_bytes : char vec t -> bytes t
(** [on_bytes pp] is [pp] adapted to format (entire) [bytes]. *)

val on_string : char vec t -> string t
(** [on_string pp] is [pp] adapted to format (entire) [string]s. *)

val ascii : ?w:int -> ?subst:unit t -> unit -> char vec t
(** [ascii ~w ~subst ()] formats character sequences by printing
    characters in the {e printable US-ASCII range} ([[0x20];[0x7E]])
    as is, and replacing the rest with [subst] (defaults to [fmt "."]).
    [w] causes the output to be right padded to the size of formatting
    at least [w] sequence elements (defaults to [0]). *)

val octets : ?w:int -> ?sep:unit t -> unit -> char vec t
(** [octets ~w ~sep ()] formats character sequences as hexadecimal
    digits.  It prints groups of successive characters of unspecified
    length together, separated by [sep] (defaults to {!sp}). [w]
    causes the output to be right padded to the size of formatting at
    least [w] sequence elements (defaults to [0]). *)

val addresses : ?addr:int t -> ?w:int -> 'a vec t -> 'a vec t
(** [addresses pp] formats sequences by applying [pp] to consecutive
    subsequences of length [w] (defaults to 16). [addr] formats
    subsequence offsets (defaults to an unspecified hexadecimal
    format).  *)

val hex : ?w:int -> unit -> char vec t
(** [hex ~w ()] formats character sequences as traditional hex dumps,
    matching the output of {e xxd} and forcing line breaks after every
    [w] characters (defaults to 16). *)

(** {1:text Words, paragraphs, text and lines}

    {b Note.} These functions only work on US-ASCII strings and/or
    with newlines (['\n']). If you are dealing with UTF-8 strings or
    different kinds of line endings you should use the pretty-printers
    from {!Uuseg_string}.

    {b White space.} White space is one of the following US-ASCII
    characters: space [' '] ([0x20]), tab ['\t'] ([0x09]), newline
    ['\n'] ([0x0A]), vertical tab ([0x0B]), form feed ([0x0C]),
    carriage return ['\r'] ([0x0D]). *)

val words : string t
(** [words] formats words by suppressing initial and trailing
    white space and replacing consecutive white space with
    a single {!Format.pp_print_space}. *)

val paragraphs : string t
(** [paragraphs] formats paragraphs by suppressing initial and trailing
    spaces and newlines, replacing blank lines (a line made only
    of white space) by a two {!Format.pp_force_newline} and remaining
    consecutive white space with a single {!Format.pp_print_space}. *)

val text : string t
(** [text] formats text by respectively replacing spaces and newlines in
    the string with {!Format.pp_print_space} and {!Format.pp_force_newline}. *)

val lines : string t
(** [lines] formats lines by replacing newlines (['\n']) in the string
    with calls to {!Format.pp_force_newline}. *)

val truncated : max:int -> string t
(** [truncated ~max] formats a string using at most [max]
    characters. If the string doesn't fit, it is truncated and ended
    with three consecutive dots which do count towards [max]. *)

val text_loc : ((int * int) * (int * int)) t
(** [text_loc] formats a line-column text range according to
    {{:http://www.gnu.org/prep/standards/standards.html#Errors}
    GNU conventions}. *)

(** {1:hci HCI fragments} *)

val one_of : ?empty:unit t -> 'a t -> 'a list t
(** [one_of ~empty pp_v ppf l] formats according to the length of [l]
    {ul
    {- [0], formats [empty] (defaults to {!nop}).}
    {- [1], formats the element with [pp_v].}
    {- [2], formats ["either %a or %a"] with the list elements}
    {- [n], formats ["one of %a, ... or %a"] with the list elements}} *)

val did_you_mean :
  ?pre:unit t -> ?post:unit t -> kind:string -> 'a t -> ('a * 'a list) t
(** [did_you_mean ~pre kind ~post pp_v] formats a faulty value [v] of
    kind [kind] and a list of [hints] that [v] could have been
    mistaken for.

    [pre] defaults to [unit "Unknown"], [post] to {!nop} they surround
    the faulty value before the "did you mean" part as follows ["%a %s
    %a%a." pre () kind pp_v v post ()]. If [hints] is empty no "did
    you mean" part is printed. *)

(** {1:utf8_cond Conditional UTF-8 formatting}

    {b Note.} Since {!Format} is not UTF-8 aware using UTF-8 output
    may derail the pretty printing process. Use the pretty-printers
    from {!Uuseg_string} if you are serious about UTF-8 formatting. *)

val if_utf_8 : 'a t -> 'a t -> 'a t
(** [if_utf_8 pp_u pp ppf v] is:
    {ul
    {- [pp_u ppf v] if [utf_8 ppf] is [true].}
    {- [pp ppf v] otherwise.}} *)

val utf_8 : Format.formatter -> bool
(** [utf_8 ppf] is [true] if UTF-8 output is enabled on [ppf]. If
    {!set_utf_8} hasn't been called on [ppf] this is [true]. *)

val set_utf_8 : Format.formatter -> bool -> unit
(** [set_utf_8 ppf b] enables or disables conditional UTF-8 formatting
    on [ppf].

    @raise Invalid_argument if [ppf] is {!Format.str_formatter}: it is
    is always UTF-8 enabled. *)

(** {1:styled Styled formatting} *)

type color =
  [ `Black | `Blue | `Cyan | `Green | `Magenta | `Red | `White | `Yellow ]
(** The type for colors. *)

type style =
  [ `None |  `Bold | `Faint | `Italic | `Underline | `Reverse
  | `Fg of [ color | `Hi of color ]
  | `Bg of [ color | `Hi of color ]
  | color (** deprecated *) ]
(** The type for styles:
    {ul
    {- [`None] resets the styling.}
    {- [`Bold], [`Faint], [`Italic], [`Underline] and [`Reverse] are
       display attributes.}
    {- [`Fg _] is the foreground color or high-intensity color on [`Hi _].}
    {- [`Bg _] is the foreground color or high-intensity color on [`Hi _].}
    {- [#color] is the foreground colour, {b deprecated} use [`Fg
       #color] instead.}} *)

val styled : style -> 'a t -> 'a t
(** [styled s pp] formats like [pp] but styled with [s]. *)

(** {2 Style rendering control} *)

type style_renderer = [ `Ansi_tty | `None ]
(** The type for style renderers.
    {ul
    {- [`Ansi_tty], renders styles using
       {{:http://www.ecma-international.org/publications/standards/Ecma-048.htm}
       ANSI escape sequences}.}
    {- [`None], styled rendering has no effect.}} *)

val style_renderer : Format.formatter  -> style_renderer
(** [style_renderer ppf] is the style renderer used by [ppf].  If
    {!set_style_renderer} has never been called on [ppf] this is
    [`None]. *)

val set_style_renderer : Format.formatter -> style_renderer -> unit
(** [set_style_renderer ppf r] sets the style renderer of [ppf] to [r].

    @raise Invalid_argument if [ppf] is {!Format.str_formatter}: its
    renderer is always [`None]. *)

(** {1:stringconverters Converting with string value converters} *)

val of_to_string : ('a -> string) -> 'a t
(** [of_to_string f ppf v] is [string ppf (f v)]. *)

val to_to_string : 'a t -> 'a -> string
(** [to_to_string pp_v v] is [strf "%a" pp_v v]. *)

(** {1:deprecated Deprecated} *)

val strf : ('a, Format.formatter, unit, string) format4 -> 'a
[@@ocaml.deprecated "use Fmt.str instead."]
(** @deprecated use {!str} instead. *)

val kstrf : (string -> 'a) -> ('b, Format.formatter, unit, 'a) format4 -> 'b
[@@ocaml.deprecated "use Fmt.kstr instead."]
(** @deprecated use {!kstr} instead. *)

val strf_like :
  Format.formatter -> ('a, Format.formatter, unit, string) format4 -> 'a
[@@ocaml.deprecated "use Fmt.str_like instead."]
(** @deprecated use {!str_like} instead. *)

val always : (unit, Format.formatter, unit) Stdlib.format -> 'a t
[@@ocaml.deprecated "use Fmt.any instead."]
(** @deprecated use {!any} instead. *)

val unit : (unit, Format.formatter, unit) Stdlib.format -> unit t
[@@ocaml.deprecated "use Fmt.any instead."]
(** @deprecated use {!any}. *)

val prefix : unit t -> 'a t -> 'a t
[@@ocaml.deprecated "use Fmt.(++) instead."]
(** @deprecated use {!( ++ )}. *)

val suffix : unit t -> 'a t -> 'a t
[@@ocaml.deprecated "use Fmt.(++) instead."]
(** @deprecated use {!( ++ )}. *)

val styled_unit :
  style -> (unit, Format.formatter, unit) Stdlib.format -> unit t
[@@ocaml.deprecated "use Fmt.(styled s (any fmt)) instead."]
(** @deprecated use [styled s (any fmt)] instead *)

(** {1:nameconv Naming conventions}

    Given a type [ty] use:

    {ul
    {- [pp_ty] for a pretty printer that provides full control to the
       client and does not wrap the formatted value in an enclosing
       box. See {{!stdlib}these examples}.}
    {- [pp_dump_ty] for a pretty printer that provides little control
       over the pretty-printing process, wraps the rendering in an
       enclosing box and tries as much as possible to respect the
       OCaml syntax. These pretty-printers should make it easy to
       inspect and understand values of the given type, they are
       mainly used for quick printf debugging and/or toplevel interaction.
       See {{!Fmt.Dump.stdlib}these examples}.}}

    If you are in a situation where making a difference between [dump_ty]
    and [pp_ty] doesn't make sense then use [pp_ty].

    For a type [ty] that is the main type of the module (the "[M.t]"
    convention) drop the suffix, that is simply use [M.pp] and
    [M.pp_dump]. *)

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
