(* Copyright (c) 2016-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(** Declaring terminals.

    Notty is a terminal library that revolves around construction and
    composition of displayable images.

    This module provides the core {{!I}[image]} abstraction, standalone
    {{!Render}rendering}, and escape sequence {{!Unescape}parsing}. It does not
    depend on any platform code, and does not interact with the environment.
    Input and output are provided by {!Notty_unix} and {!Notty_lwt}.

    Consult the {{!basics}basics}, {{!examples}examples} and
    {{!limitations}limitations}.

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Interface} *)

type attr
(** Visual characteristics of displayed text. *)

type image
(** Rectangles of styled characters. *)

(** [A] is for attribute.

    Construction and composition of styling characteristics of text.

    Consult the {{!basics}basics} for an overview. *)
module A : sig

  (** {1 Colors} *)

  type color
  (** An ineffable quality of light.

      There are three kinds of colors:
      {ul
      {- {e Core 16 colors.}

         ANSI defines 8 color {e names}, with the actual display colors
         considered an implementation detail. Historically, this palette was
         extended with their light (sometimes {e bright} or {e high-intensity})
         counterparts. Their presentation is undefined too, but typically
         produces a brighter shade. These colors - often called the {e ANSI
         colors} - tend to be unpredictable, but ubiquitously supported.

         }
      {- {e Extended 256-color palette.}

         This common feature extends the palette by further 240 colors. They
         come in two groups:

         {ul
         {- The {e color cube}, a 6*6*6 approximation to the usual 24-bit RGB
            color cube; and}
         {- the {e grayscale ramp}, containing (merely) 24 shades of gray.}}

         XTerm was the first to support this extension. Many terminals have
         since cloned it, so the support is wide, but not universal.

         As the extended colors are still palette-driven they do not have a
         fixed presentation, and the presentation can be changed in some
         terminals. Default palette tends to match {{:
         https://upload.wikimedia.org/wikipedia/commons/1/15/Xterm_256color_chart.svg}
         XTerm's}.

         }
      {- {e True color}

         A recently established convention allows directly sending 24-bit colors
         to the terminal. This has been adopted by a growing minority of
         terminals. A reasonably up-to-date status document maintained by the
         community can be found {{:https://gist.github.com/XVilka/8346728}here}.}}

      Some of the technical and historical background can be found in {{:
      http://invisible-island.net/xterm/xterm.faq.html#problems_colors}
      XTerm's FAQ}.

      {b Note} No attempt is made to remap colors depending on the terminal.
      Terminals might ignore, remap, or completely misinterpret unsupported
      colors. *)

  (** {2:corecolors Core 16 colors}

      The first 8 have their standard ANSI names. *)

  val black        : color
  val red          : color
  val green        : color
  val yellow       : color
  val blue         : color
  val magenta      : color
  val cyan         : color
  val white        : color
  val lightblack   : color
  val lightred     : color
  val lightgreen   : color
  val lightyellow  : color
  val lightblue    : color
  val lightmagenta : color
  val lightcyan    : color
  val lightwhite   : color

  (** {2 Extended 256-color palette} *)

  val rgb : r:int -> g:int -> b:int -> color
  (** [rgb ~r:red ~g:green ~b:blue] is an extended-palette color from the color cube.

      All three channels must be in the range [0 - 5]. XTerm default palette maps
      this to [0x00], [0x5f], [0x87], [0xaf], [0xd7], and [0xff] independently
      per channel.

      @raise Invalid_argument if a channel is outside the range. *)

  val gray : int -> color
  (** [gray level] is an extended-palette color from the grayscale ramp.

      [level] must be in the range [0 - 23]. XTerm default palette maps this to
      [8 + level * 10] on all three channels.

      @raise Invalid_argument if the [level] is outside the range. *)

  (** {2 True Color} *)

  val rgb_888 : r:int -> g:int -> b:int -> color
  (** [rgb_888 ~r:red ~g:green ~b:blue] is a 24-bit color.

      All three channels must be in the range [0 - 255].

      @raise Invalid_argument if a channel is outside the range. *)

  (** {1 Text styles} *)

  type style
  (** Additional text properties. *)

  val bold      : style
  val dim       : style
  val faint     : style
  val italic    : style
  val underline : style
  val blink     : style
  val reverse   : style

  (** {1 Attribute construction and composition} *)

  type t = attr

  val equal : t -> t -> bool

  val empty : attr
  (** [empty] is the attribute with the default foreground and background color
      and empty style set. *)

  val (++) : attr -> attr -> attr
  (** [a1 ++ a2] is the concatenation of [a1] and [a2], the attribute that has
      [a2]'s foreground (resp. background), unless {e unset}, in which case it
      is [a1]'s, and the union of both style sets.

      [++] is left-associative, and forms a monoid with [empty]. *)

  val fg : color -> attr
  (** [fg c] is [empty] with foreground [c]. *)

  val bg : color -> attr
  (** [bg c] is [empty] with background [c]. *)

  val st : style -> attr
  (** [st s] is [empty] with style [s]. *)
end

(** [I] is for image.

    Construction and composition of images.

    Consult the {{!basics}basics} for an overview. *)
module I : sig

  type t = image

  val height : image -> int
  val width  : image -> int

  val equal : t -> t -> bool
  (** [equal t1 t2] is [true] iff [t1] and [t2] are constructed by the same term.

      {b Note} This is a weak form of equality. Images that are not [equal]
      could still render the same. *)

  (** {1:imgprims Primitives} *)

  val empty : image
  (** [empty] is a zero-sized image. *)

  val string : attr -> string -> image
  (** [string attr s] is an image containing text [s], styled with [attr].

      @raise Invalid_argument if [string] is not a valid UTF-8 sequence, or
      contains {{!ctrls}control characters}. *)

  val uchars : attr -> Uchar.t array -> image
  (** [uchars attr us] is an image containing text [us], styled with [attr].

      @raise Invalid_argument if [us] contains {{!ctrls}control characters}. *)

  val char : attr -> char -> int -> int -> image
  (** [char attr c w h] is a [w * h] grid of [c].

      @raise Invalid_argument if [c] is a {{!ctrls}control character}. *)

  val uchar : attr -> Uchar.t -> int -> int -> image
  (** [uchar attr u w h] is a [w * h] grid of [u].

      @raise Invalid_argument if [u] is a {{!ctrls}control character}. *)

  val void  : int -> int -> image
  (** [void w h] is a [w * h] rectangle of transparent cells.

      [void] is magical: it has geometry, but no displayable content. This is
      different, for example, from the space character [U+0020], which renders
      as a cell filled with the background color. This means that [void]
      interacts specially with {{!(</>)}overlays}.

      [void 0 0 = empty].
      [void] with only one dimension [0] acts as a spacing element in the other
      dimension. Negative size is treated as [0]. *)

  (** {1:imgcomp Image composition}

      Three basic composition modes allow construction of more complex images
      from simpler ones.

      Composition operators are left-associative and form a monoid with [void].
      *)

  val (<|>) : image -> image -> image
  (**  [i1 <|> i2] is the horizontal combination of [i1] and [i2].

      [width (i1 <|> i2) = width i1 + width i2]
      [height (i1 <|> i2) = max (height i1) (height i2)]

      Images are top-aligned. The missing region is implicitly filled with
      {{!void}[void]}.

{v
[x] <|> [y] = [xy]
        [y]   [.y]
v}

      where [.] denotes {{!void}[void]}. *)

  val (<->) : image -> image -> image
  (** [i1 <-> i2] is the vertical combination of [i1] and [i2].

      [width (i1 <-> i2) = max (width i1) (width i2)]
      [height (i1 <-> i2) = height i1 + height i2]

      Images are left-aligned. The missing region is implicitly filled with
      {{!void}[void]}.

{v
[xx] <-> [y] = [xx]
               [y.]
v}
      *)

  val (</>) : image -> image -> image
  (** [i1 </> i2] is [i1] overlaid over [i2].

      [width (i1 </> i2) = max (width i1) (width i2)]
      [height (i1 </> i2) = max (height i1) (height i2)]

      Images are top-left-aligned. In the region of their overlap, only the
      {{!void}[void]} cells of [i1] show fragments of [i2].

{v
[x.x] </> [yyyy] = [xyxy]
v}
      *)

  (** {1:imgcrop Cropping and padding} *)

  val hcrop : int -> int -> image -> image
  (** [hcrop left right i] is [i] with [left] leftmost, and [right]
      rightmost columns missing. If [left + right >= width i] the result is
      [empty].

      If either [left] or [right] is negative, instead of being cropped, the
      image is padded on that side.

      For example:
      {ul
      {- [hcrop 0 1 [abc]] = [[ab]]}
      {- [hcrop 1 1 [abc]] = [[b]]}
      {- [hcrop (-1) 1 [abc]] = [void 1 1 <|> hcrop 0 1 [abc]] = [[.ab]]}
      {- [hcrop 2 2 [abc]] = [empty]}} *)

  val vcrop : int -> int -> image -> image
  (** [vcrop top bottom i] is the vertical analogue to {{!hcrop}[hcrop]}. *)

  val crop : ?l:int -> ?r:int -> ?t:int -> ?b:int -> image -> image
  (** [crop ~l:left ~r:right ~t:top ~b:bottom i] is
      [vcrop left right (hcrop top bottom) i].

      Missing arguments default to [0]. *)

  val hpad : int -> int -> image -> image
  (** {{!hcrop}[hcrop]} with margins negated. *)

  val vpad : int -> int -> image -> image
  (** {{!vcrop}[vcrop]} with margins negated. *)

  val pad : ?l:int -> ?r:int -> ?t:int -> ?b:int -> image -> image
  (** {{!crop}[crop]} with margins negated. *)


  (** {1 Additional combinators} *)

  val hcat : image list -> image
  (** [hcat xs] horizontally concatenates [xs]. See {{!(<|>)}beside}. *)

  val vcat : image list -> image
  (** [vcat xs] vertically concatenates [xs]. See {{!(<->)}above}. *)

  val zcat : image list -> image
  (** [zcat xs] overlays [xs]. See {{!(</>)}over}. *)

  val tabulate : int -> int -> (int -> int -> image) -> image
  (** [tabulate m n f] is the grid of values [f x y] with [x = 0..m-1]
      and [y = 0..n-1], where [x] grows to the right, and [y] growns down.

      [f a y] is to the left of [f b y] if [a < b], and [f x a] is above [f x b]
      if [a < b], but the exact alignment is unspecified if the various [f x y]
      have different dimensions. *)

  val hsnap : ?align:[ `Left | `Middle | `Right ] -> int -> image -> image
  (** [hsnap ~align w i] is an image of width strictly [w] obtained by either
      horizontally padding or cropping [i] and positioning it according to
      [~align].

      [~align] defaults to [`Middle]. *)

  val vsnap : ?align:[ `Top | `Middle | `Bottom ] -> int -> image -> image
  (** [vsnap ~align h i] is an image of height strictly [h] obtained by either
      vertically padding or cropping [i] and positioning it according to
      [~align].

      [~align] defaults to [`Middle]. *)

  (** {1 [Format] interoperability} *)

  val strf : ?attr:attr -> ?w:int -> ('a, Format.formatter, unit, image) format4 -> 'a
  (** [strf ?attr ?w:width format ...] pretty-prints like
      [Format.asprintf format ...], but returns an [image].

      [attr] is the (outermost) attribute. Defaults to {!A.empty}.

      [width] is used to set the margin on the formatter. This is only a hint,
      and does not guarantee the width of the result. Consult
      {{: http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html#VALset_margin}
      [Format.set_margin]} for details. Defaults to an unspecified, large
      number.

      @raise Invalid_argument if the printing process attempts to directly
      output {{!ctrls}control characters}, by embedding them in [format] or a
      string printed with the [%s] conversion, for example.
      {{: http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html#fpp}
      Formatted printing} is allowed. *)

  val kstrf : ?attr:attr -> ?w:int -> (image -> 'a) -> ('b, Format.formatter, unit, 'a) format4 -> 'b
  (** [kstrf ?attr ?w k format ...] is continuation-based [strf ?attr ?w format ...]. *)

  val pp_attr : attr -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
  (** [pp_attr a f] is a pretty-printer like [f], except its output is styled
      with [a]. This applies only outside of any styling [f] itself might embed. *)
end

(** Operators, repeated. *)
module Infix : sig

  (** {2 [I]}

      See {{!I}[I]}. *)

  val (<->) : image -> image -> image
  val (<|>) : image -> image -> image
  val (</>) : image -> image -> image

  (** {2 [A]}

      See {{!A}[A]}. *)

  val (++)  : attr -> attr -> attr
end

(** {1 Low-level interface}

    You can ignore it, unless you are porting [Notty] to a new platform not
    supported by the existing IO backends. *)

(** Terminal capabilities.

    This module describes how to output things so that a terminal understands
    them. *)
module Cap : sig

  type t
  (** A set of capabilities that distinguish terminals from one another.

      A bundle of magic strings, really. *)

  val ansi : t
  (** The usual ANSI terminal, with colors, text styles and cursor
      positioning. *)

  val dumb : t
  (** Pure text output. Text attributes are stripped and positioning is done
      with the character [U+0020], SPACE. *)
end

(** Dump images to string buffers. *)
module Render : sig

  val to_buffer : Buffer.t -> Cap.t -> int * int -> int * int -> image -> unit
  (** [to_buffer buf cap (x, y) (w, h) i] writes the string representation of
      [i] to [buf], as interpreted by [cap].

      It renders the [w * h] rectangle of [i], offset by [(x, y)] from the top
      left. *)

  val pp : Cap.t -> Format.formatter -> image -> unit
  (** [pp cap ppf i] renders [i] to the pretty-printer [ppf].

      {b Note} [pp] is generally meant for development and debugging. It tries
      to be reasonable, but dedicated IO modules handle the actual output
      better. *)

  (**/**)
  (* Toplevel. *)
  val pp_image : Format.formatter -> image -> unit
  val pp_attr : Format.formatter -> attr -> unit
  (**/**)
end

(** Parse and decode escape sequences in character streams. *)
module Unescape : sig

  (** {1 Input events} *)

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
  (** A selection of extra keys on the keyboard. *)

  type button = [ `Left | `Middle | `Right | `Scroll of [ `Up | `Down ] ]
  (** Mouse buttons. *)

  type mods = [ `Meta | `Ctrl | `Shift ] list
  (** Modifier state. *)

  type key = [ special | `Uchar of Uchar.t | `ASCII of char ] * mods
  (** Keypress event. *)

  type mouse = [ `Press of button | `Drag | `Release ] * (int * int) * mods
  (** Mouse event. *)

  type paste = [ `Start | `End ]
  (** Paste event. *)

  type event = [ `Key of key | `Mouse of mouse | `Paste of paste ]
  (** Things that terminals say to applications.

      {ul
      {- [`Key (k, mods)] is keyboard input.

         [k] is a {{!key}key}, one of:
         {ul
         {- [`ASCII c] where [c] is a [char] in the
            {{: https://tools.ietf.org/html/rfc20}ASCII} range;}
         {- [`Uchar u] where [u] is any other {{!Uchar.t}unicode character}; or}
         {- a {{!special}special key}.}}

         [`ASCII] and [`Uchar] together represent the textual part of the input.
         These characters are guaranteed not to be {{!ctrls}control
         characters}, and are safe to use when constructing images. ASCII is
         separated from the rest of Unicode for convenient pattern-matching.

         [mods] are the extra {{!mods}modifier keys}.

         }
      {- [`Mouse (event, (x, y), mods)] is mouse input.

         [event] is the actual mouse event: {{!button}[button]} press, release,
         or motion of the mouse with buttons depressed.

         [(x, y)] are column and row position of the mouse. The origin is
         [(0,0)], the upper-left corner.

         {b Note} Every [`Press (`Left|`Middle|`Right)] generates a corresponding
         [`Release], but there is no portable way to detect which button was
         released. [`Scroll (`Up|`Down)] presses are not followed by releases.

         }
      {- [`Paste (`Start|`End)] are {e bracketed paste} events, signalling the
         beginning and end of a sequence of events pasted into the terminal.

         {b Note} This mechanism is useful, but not reliable. The pasted text
         could contain spurious start-of-paste or end-of-paste markers, or they
         could be entered by hand. }}

      Terminal input protocols are historical cruft, and heavily overload the
      ASCII range. For instance:
      {ul
      {- It is impossible to distinguish lower- and upper-case ASCII characters
         if {b Ctrl} is pressed;}
      {- several combinations of key-presses are aliased as special keys; and}
      {- in a UTF-8 encoded stream, there is no representation for non-ASCII
         characters with modifier keys.}}

      This means that many values that inhabit the [event] type are impossible,
      while some reflect multiple different user actions. Limitations include:

      {ul
      {- [`Shift] is reported only with special keys, and not all of them.}
      {- [`Meta] and [`Control] are reported with mouse events, key events with
         special keys, and key events with values in the ranges [0x40-0x5f]
         ([@] to [_]) and [0x60-0x7e] ([`] to [~]). If {b Ctrl} is pressed, the higher
         range is mapped into the lower range.}
      {- Terminals will variously under-report modifier key state.}}

      Perform own experiments before relying on elaborate key combinations. *)

  val uchar : [< `Uchar of Uchar.t | `ASCII of char ] -> Uchar.t
  (** [uchar x] is the {!Uchar.t} corresponding to [x]. This operations merges
      the ASCII and Unicode variants of {{!key}key}. *)

  (** {1 Decoding filter}

      Simple IO-less terminal input processor. It can be used for building
      custom terminal input abstractions. *)

  type t
  (** Input decoding filter.

      The filter should be {{!input}fed} strings, which it first decodes from
      UTF-8, and then extracts the input events.

      Malformed UTF-8 input bytes and unrecognized escape sequences are silently
      discarded. *)

  val create : unit -> t
  (** [create ()] is a new, empty filter. *)

  val input : t -> bytes -> int -> int -> unit
  (** [input t buffer i len] feeds [len] bytes of [string] into [t], starting
      from position [len].

      [len = 0] signals the end of input.

      [buffer] is immediately processed and can be reused after the call
      returns. *)

  val next : t -> [ event | `Await | `End ]
  (** [next t] is the next event in the filter's input stream:

      {ul
      {- [#event], an input {{!event}[event]}.}
      {- [`Await] if the filter needs more {{!input}input}.}
      {- [`End] if the input had ended.}} *)

  val pending : t -> bool
  (** [pending t] is [true] if a call to [next], without any intervening input,
      would {e not} return [`Await]. *)

  (** {1 Low-level parsing}

      {b Warning} The parsing interface is subject to change.

      Implementation of small parts of
      {{: http://www.ecma-international.org/publications/standards/Ecma-035.htm}ECMA-35}
      and
      {{: http://www.ecma-international.org/publications/standards/Ecma-048.htm}ECMA-48},
      as needed by terminal emulators in common use. *)

  val decode : Uchar.t list -> event list
  (** [decode us] are the events encoded by [us].

      [us] are assumed to have been generated in a burst, and the end of the
      list is taken to mean a pause.
      Therefore, [decode us1 @ decode us2 <> decode (us1 @ us2)] if [us1] ends
      with a partial escape sequence, including a lone [\x1b].

      Unsupported escape sequences are silently discarded. *)
end

(**/**)
(** {1 Private}

    These are private interfaces, prone to breakage. Don't use them. *)

module Operation : sig
  type t
  val of_image : (int * int) -> int * int -> image -> t list
end

module Tmachine : sig

  type t

  val create  : mouse:bool -> bpaste:bool -> Cap.t -> t
  val release : t -> bool
  val output  : t -> Buffer.t -> unit

  val refresh  : t -> unit
  val cursor   : t -> (int * int) option -> unit
  val image    : t -> image -> unit

  val set_size : t -> int * int -> unit

  val size : t -> int * int
  val dead : t -> bool
end

module Direct : sig
  val move_cursor : Buffer.t -> Cap.t -> [ `Home | `By of int * int | `To of int * int ] -> unit
  val show_cursor : Buffer.t -> Cap.t -> bool -> unit
end
(**/**)

(** {1:basics Basics}

    Print a red-on-black ["Wow!"] above its right-shifted copy:
{[
let wow = I.string A.(fg red ++ bg black) "Wow!" in
I.(wow <-> (void 2 0 <|> wow)) |> Notty_unix.output_image
]}

    {2:meaning The meaning of images}

    An {{!image}[image]} value is a rectangle of styled character cells. It has a
    width and height, but is not anchored to an origin. A single character with
    associated display attributes, or a short fragment of text, are simple
    examples of images.

    Images are created by combining text fragments with {{!attributes}display
    attributes}, and composed by placing them {{!I.(<|>)}beside} each other,
    {{!I.(<->)}above} each other, and {{!I.(</>)}over} each other.

    Once constructed, an image can be rendered, and only at that point it obtains
    absolute placement.

    Consult {{!I}[I]} for more details.

    {2:attributes Display attributes}

    {{!attr}[attr]} values describe the styling characteristics of fragments of
    text.

    They combine a foreground and a background {{!A.color}[color]} with a
    set of {{!A.style}[styles]}. Either color can be {e unset}, which corresponds to
    the terminal's default foreground (resp. background) color.

    Attributes are used to construct primitive images.

    Consult {{!A}[A]} for more details.

    {2:ctrls Control characters}

    These are taken to be characters in the ranges [0x00-0x1f] ({b C0}), [0x7f]
    (BACKSPACE), [0x80-0x9f] ({b C1}). This is the
    {{: http://unicode.org/reports/tr44/#General_Category_Values}Unicode
    general category} {b Cc}.

    As control characters directly influence the cursor positioning, they
    cannot be used to create images.

    This, in particular, means that images cannot contain [U+000a] (NEWLINE).

    {1:limitations Limitations}

    [Notty] does not use Terminfo. If your terminal is particularly
    idiosyncratic, things might fail to work. Get in touch with the author to
    expand support.

    [Notty] assumes that the terminal is using UTF-8 for input and output.
    Things might break arbitrarily if this is not the case.

    For performance considerations, consult the {{!perf}performance model}.

    {2:cwidth Unicode vs. Text geometry}

    [Notty] uses [Uucp.Break.tty_width_hint] to guess the width of text
    fragments when computing geometry, and it suffers from the same
    shortcomings:

    {ul
    {- Geometry in general works for alphabets and east Asian scripts, mostly
        works for abjad scripts, and is a matter of luck for abugidas.}
    {- East Asian scripts work better when in
        {{:http://unicode.org/glossary/#normalization_form_c}NFC}.}
    {- For proper emoji display, [Uucp] and the terminal have to agree on the
       Unicode version.}}

    When in doubt, see
    {{: http://erratique.ch/software/uucp/doc/Uucp.Break.html#VALtty_width_hint}
    [Uucp.Break.tty_width_hint]}.

    Unicode has special interaction with {{!I.hcrop}horizontal cropping}:
    {ul
    {- Strings within images are cropped at {{:
        http://unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries}grapheme
        cluster} boundaries. This means that scalar value sequences that are
        rendered combined, or overlaid, stay unbroken.}
    {- When a crop splits a wide character in two, the remaining half is
        replaced by [U+0020] (SPACE). Hence, character-cell-accurate cropping is
        possible even in the presence of characters that horizontally occupy
        more than one cell.}}

    {1:examples Examples}

    We assume a toplevel with [Notty] support ([#require "notty.top"]).

    {2 Hello}

    ["Rad!"] with default foreground and background:

    {[I.string A.empty "Rad!"]}

    Everything has to start somewhere.

    {2 Colors}

    ["Rad!"] in rad letters:

    {[I.string A.(fg lightred) "Rad!"]}

    {2 Padding and spacing}

{[
let a1 = A.(fg lightwhite ++ bg red)
and a2 = A.(fg red)
]}

    ["Rad"] and [" stuff!"] in different colors:

    {[I.(string a1 "Rad" <|> string a2 " stuff!")]}

    The second word hanging on a line below:

    {[I.(string a1 "Rad" <|> (string a2 "stuff!" |> vpad 1 0))]}

    {2 More geometry}

    Sierpinski triangle:

{[
let square = "\xe2\x96\xaa"

let rec sierp n =
  if n > 1 then
    let ss = sierp (pred n) in I.(ss <-> (ss <|> ss))
  else I.(string A.(fg magenta) square |> hpad 1 0)
]}

    {[sierp 8]}

    A triangle overlaid over its shifted copy:

    {[let s = sierp 6 in I.(s </> vpad 1 0 s)]}

    Blinkenlights:

{[
let rad n color =
  let a1 = A.fg color in
  let a2 = A.(st blink ++ a1) in
  I.((string a2 "Rad" |> hpad n 0) <->
     (string a1 "(⌐■_■)" |> hpad (n + 7) 0))

let colors = A.[red; green; yellow; blue; magenta; cyan]
]}

{[
colors |> List.mapi I.(fun i c -> rad i c |> pad ~t:i ~l:(2 * i))
       |> I.zcat
]}

    {b Note} Usage of {{!A.blink}[blink]} might be regulated by law in some
    jurisdictions.

    {2 Pretty-printing}

    Images can be pretty-printed into:

    {[I.strf "(%d)" 42]}

    Attributes can be applied to the entire format string, or by decorating
    {e user-defined printers} that are supplied with [%a] conversions:

    {[let pp = Format.pp_print_int]}

    {[I.strf ~attr:A.(fg lightwhite) "(%a)" (I.pp_attr A.(fg green) pp) 42]}

    {2 Now with output}

    The core module has no real IO. Examples above are simple [image]-valued
    expressions, displayed by the pretty-printer that is installed by the
    toplevel support. Self-contained programs need a separate IO module:

    {[#require "notty.unix"]}

    {[sierp 8 |> Notty_unix.output_image]}

    (Note the difference in cropping behavior.)

    Computations can be adapted to the current terminal size. A line can stretch
    end-to-end:

{[
Notty_unix.output_image_size @@ fun (w, _) ->
  let i1 = I.string A.(fg green) "very"
  and i2 = I.string A.(fg yellow) "melon" in
  I.(i1 <|> void (w - width i1 - width i2) 1 <|> i2)
]}

    The largest triangle that horizontally fits into the terminal:

{[
Notty_unix.output_image_size @@ fun (w, _) ->
  let steps = int_of_float ((log (float w)) /. log 2.) in
  sierp steps |> I.vpad 0 1
]}

    {2 Simple interaction}

    Interactive Sierpinski:

    {[open Notty_unix]}

{[
let img (double, n) =
  let s = sierp n in
  if double then I.(s </> vpad 1 0 s) else s
in
let rec update t state = Term.image t (img state); loop t state
and loop t (double, n as state) =
  match Term.event t with
  | `Key (`Enter,_)        -> ()
  | `Key (`Arrow `Left,_)  -> update t (double, max 1 (n - 1))
  | `Key (`Arrow `Right,_) -> update t (double, min 8 (n + 1))
  | `Key (`ASCII ' ', _)   -> update t (not double, n)
  | `Resize _              -> update t state
  | _                      -> loop t state
in
let t = Term.create ()
in
update t (false, 1); Term.release t
]}

    The program uses a fullscreen {{!Notty_unix.Term}terminal} and loops reading
    the {{!Notty_unix.Term.event}input}. LEFT and RIGHT control the iteration
    count, and SPACE toggles double-drawing. Resizing the window causes a
    redraw. When the loop exits on ENTER, the terminal is
    {{!Notty_unix.Term.release}cleaned up}.

    {1:perf Performance model}

    This section is only relevant if using [Notty] becomes your bottleneck.

    {b TL;DR} Shared sub-expressions do not share work, so operators stick with
    you.

    The main performance parameter is {e image complexity}. This roughly
    corresponds to the number of image {{!I.imgcomp}composition} and
    {{!I.imgcrop}cropping} operators in the fully expanded [image] term,
    {b ignoring all sharing}.

    Outline numbers:

    {ul
    {- Highly complex images can be rendered and pushed out to a full-screen
       terminal more than 1000 times per second.}
    {- With more realistic images, this number is closer to 30,000.}
    {- Input processing is somewhere around 50MB/s.}}


    Image complexity [cplx] of an image [i] is:
    {ul
    {- For a {{!I.imgprims}primitive} [i], [cplx i = 1].}
    {- For a {{!I.imgcomp}composition} operator [op],
       [cplx (op i1 i2) = 1 + cplx i1 + cplx i2].}
    {- For a {{!I.imgcomp}crop} [cr],
       [cplx (cr i1) = 1 + cplx i1 - k], where [k] is the combined complexity of
       all the {e maximal} sub-terms that do not contribute to the output.}}

    For example (assuming an image [i]):

{[
  let img1 = I.((i <|> i) <-> (i <|> i))
  let img2 = I.(let x = i <|> i in x <-> x)
  let img3 = I.(((i <|> i) <|> i) <|> i)
]}

    Complexity of each of these is [4 * cplx i + 3]. This might be surprising
    for [img2].

    If [width i = 1], [cplx (hcrop 1 0 img1) = 3 + 2 * cplx i], and
    [cplx (hcrop 2 0 img3) = 2 + 2 * cplx i].

    While [Notty] strives to be accommodating to all usage scenarios, these are
    the things to keep in mind if the rendering becomes slow:

    {ol
    {- Image composition is cheap.

       Combining images performs a negligible amount of computation.

       Constructing primitive images that contain scalar values outside of the
       ASCII range does a little more work upfront and is worth holding onto.

       }
    {- {{!Render}Rendering} depends on image complexity.

       As a consequence, this real-world example of wrapping renders in time
       O(n{^ 2}) in the number of lines:

{[
let wrap1 width img =
  let rec go img = img ::
    if I.width img > width then go (I.hcrop width 0 img) else []
  in go img |> I.vcat |> I.hsnap ~align:`Left width
]}

       Although [crop] is applied only [lines] times, the image complexity of
       each line depends on the number of preceding lines.

       An O(n) version does not iterate [crop]:

{[
let wrap2 width img =
  let rec go off = I.hcrop off 0 img ::
    if I.width img - off > width then go (off + width) else []
  in go 0 |> I.vcat |> I.hsnap ~align:`Left width
]}
       }
    {- Rendering depends on the {e output} dimensions, but not on the {e image}
       dimensions.

       Rendering an image to [w * h] implicitly crops it to its leftmost [w]
       columns and topmost [h] rows. While [w] and [h] will have an impact on
       the rendering performance, the complexity of the (cropped) image tends to
       be more important.}}

*)
