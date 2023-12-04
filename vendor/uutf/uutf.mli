(*---------------------------------------------------------------------------
   Copyright (c) 2012 The uutf programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Non-blocking streaming Unicode codec.

  [Uutf] is a non-blocking streaming codec to {{:#decode}decode} and
  {{:#encode}encode} the {{:http://www.ietf.org/rfc/rfc3629.txt}
  UTF-8}, {{:http://www.ietf.org/rfc/rfc2781.txt} UTF-16}, UTF-16LE
  and UTF-16BE encoding schemes. It can efficiently work character by
  character without blocking on IO. Decoders perform
  character position tracking and support {{!nln}newline normalization}.

  Functions are also provided to {{!String} fold over} the characters
  of UTF encoded OCaml string values and to {{!Buffer}directly encode}
  characters in OCaml {!Stdlib.Buffer.t} values. {b Note} that since OCaml
  4.14, that functionality can be found in {!Stdlib.String} and
  {!Stdlib.Buffer} and you are encouraged to migrate to it.

  See {{:#examples}examples} of use.

  {b References}
  {ul
  {- The Unicode Consortium.
  {e {{:http://www.unicode.org/versions/latest}The Unicode Standard}}.
  (latest version)}}
*)

  (** {1:ucharcsts Special Unicode characters} *)

val u_bom : Uchar.t
(** [u_bom] is the {{:http://unicode.org/glossary/#byte_order_mark}byte
    order mark} (BOM) character ([U+FEFF]). From OCaml 4.06 on, use
    {!Uchar.bom}. *)

val u_rep : Uchar.t
(** [u_rep] is the
    {{:http://unicode.org/glossary/#replacement_character}replacement}
    character ([U+FFFD]). From OCaml 4.06 on, use
    {!Uchar.rep}. *)


(** {1:schemes Unicode encoding schemes} *)

type encoding = [ `UTF_16 | `UTF_16BE | `UTF_16LE | `UTF_8 ]
(** The type for Unicode
    {{:http://unicode.org/glossary/#character_encoding_scheme}encoding
    schemes}. *)

type decoder_encoding = [ encoding | `US_ASCII | `ISO_8859_1 ]
(** The type for encoding schemes {e decoded} by [Uutf]. Unicode encoding
    schemes plus {{:http://tools.ietf.org/html/rfc20}US-ASCII} and
    {{:http://www.ecma-international.org/publications/standards/Ecma-094.htm}
    ISO/IEC 8859-1} (latin-1). *)

val encoding_of_string : string -> decoder_encoding option
(** [encoding_of_string s] converts a (case insensitive)
    {{:http://www.iana.org/assignments/character-sets}IANA character set name}
    to an encoding. *)

val encoding_to_string : [< decoder_encoding] -> string
(** [encoding_to_string e] is a
    {{:http://www.iana.org/assignments/character-sets}IANA character set name}
    for [e]. *)

(** {1:decode Decode} *)

type src = [ `Channel of in_channel | `String of string | `Manual ]
(** The type for input sources. With a [`Manual] source the client
    must provide input with {!Manual.src}. *)

type nln = [ `ASCII of Uchar.t | `NLF of Uchar.t | `Readline of Uchar.t ]
(** The type for newline normalizations. The variant argument is the
    normalization character.
    {ul
    {- [`ASCII], normalizes CR ([U+000D]), LF ([U+000A]) and CRLF
       (<[U+000D], [U+000A]>).}
    {- [`NLF], normalizes the Unicode newline function (NLF). This is
       NEL ([U+0085]) and the normalizations of [`ASCII].}
    {- [`Readline], normalizes for a Unicode readline function. This is FF
       ([U+000C]), LS ([U+2028]), PS ([U+2029]), and the normalizations
       of [`NLF].}}
    Used with an appropriate normalization character the [`NLF] and
    [`Readline] normalizations allow to implement all the different
    recommendations of Unicode's newline guidelines (section 5.8 in
    Unicode 9.0.0). *)

type decoder
(** The type for decoders. *)

val decoder : ?nln:[< nln] -> ?encoding:[< decoder_encoding] -> [< src] ->
  decoder
(** [decoder nln encoding src] is a decoder that inputs from [src].

    {b Byte order mark.}
    {{:http://unicode.org/glossary/#byte_order_mark}Byte order mark}
    (BOM) constraints are application dependent and prone to
    misunderstandings (see the
    {{:http://www.unicode.org/faq/utf_bom.html#BOM}FAQ}). Hence,
    [Uutf] decoders have a simple rule: an {e initial BOM is always
    removed from the input and not counted in character position
    tracking}. The function {!decoder_removed_bom} does however return
    [true] if a BOM was removed so that all the information can be
    recovered if needed.

    For UTF-16BE and UTF-16LE the above rule is a violation of
    conformance D96 and D97 of the standard. [Uutf] favors the idea
    that if there's a BOM, decoding with [`UTF_16] or the [`UTF_16XX]
    corresponding to the BOM should decode the same character sequence
    (this is not the case if you stick to the standard). The client
    can however regain conformance by consulting the result of
    {!decoder_removed_bom} and take appropriate action.

    {b Encoding.} [encoding] specifies the decoded encoding
    scheme. If [`UTF_16] is used the endianness is determined
    according to the standard: from a
    {{:http://unicode.org/glossary/#byte_order_mark}BOM}
    if there is one, [`UTF_16BE] otherwise.

    If [encoding] is unspecified it is guessed. The result of a guess
    can only be [`UTF_8], [`UTF_16BE] or [`UTF_16LE]. The heuristic
    looks at the first three bytes of input (or less if impossible)
    and takes the {e first} matching byte pattern in the table below.
{v
xx = any byte
.. = any byte or no byte (input too small)
pp = positive byte
uu = valid UTF-8 first byte

Bytes    | Guess     | Rationale
---------+-----------+-----------------------------------------------
EF BB BF | `UTF_8    | UTF-8 BOM
FE FF .. | `UTF_16BE | UTF-16BE BOM
FF FE .. | `UTF_16LE | UTF-16LE BOM
00 pp .. | `UTF_16BE | ASCII UTF-16BE and U+0000 is often forbidden
pp 00 .. | `UTF_16LE | ASCII UTF-16LE and U+0000 is often forbidden
uu .. .. | `UTF_8    | ASCII UTF-8 or valid UTF-8 first byte.
xx xx .. | `UTF_16BE | Not UTF-8 => UTF-16, no BOM => UTF-16BE
.. .. .. | `UTF_8    | Single malformed UTF-8 byte or no input.
v}
    This heuristic is compatible both with BOM based
    recognitition and
    {{:http://tools.ietf.org/html/rfc4627#section-3}JSON-like encoding
    recognition} that relies on ASCII being present at the beginning
    of the stream. Also, {!decoder_removed_bom} will tell the client
    if the guess was BOM based.

    {b Newline normalization.} If [nln] is specified, the given
    newline normalization is performed, see {!nln}. Otherwise
    all newlines are returned as found in the input.

    {b Character position.} The line number, column number, byte count
    and character count of the last decoded character (including
    [`Malformed] ones) are respectively returned by {!decoder_line},
    {!decoder_col}, {!decoder_byte_count} and {!decoder_count}. Before
    the first call to {!val-decode} the line number is [1] and the column
    is [0].  Each {!val-decode} returning [`Uchar] or [`Malformed]
    increments the column until a newline.  On a newline, the line
    number is incremented and the column set to zero. For example the
    line is [2] and column [0] after the first newline was
    decoded. This can be understood as if {!val-decode} was moving an
    insertion point to the right in the data.  A {e newline} is
    anything normalized by [`Readline], see {!nln}.

    [Uutf] assumes that each Unicode scalar value has a column width
    of 1. The same assumption may not be made by the display program
    (e.g. for [emacs]' compilation mode you need to set
    [compilation-error-screen-columns] to [nil]). The problem is in
    general difficult to solve without interaction or convention with the
    display program's rendering engine. Depending on the context better column
    increments can be implemented by using {!Uucp.Break.tty_width_hint} or
    {{:http://unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries}
    grapheme cluster boundaries} (see {!Uuseg}). *)

val decode : decoder ->
  [ `Await | `Uchar of Uchar.t | `End | `Malformed of string]
(** [decode d] is:
    {ul
    {- [`Await] if [d] has a [`Manual] input source and awaits
       for more input. The client must use {!Manual.src} to provide it.}
    {- [`Uchar u] if a Unicode scalar value [u] was decoded.}
    {- [`End] if the end of input was reached.}
    {- [`Malformed bytes] if the [bytes] sequence is malformed according to
       the decoded encoding scheme. If you are interested in a best-effort
       decoding you can still continue to decode after an error until the
       decoder synchronizes again on valid bytes. It may however be a good
       idea to signal the malformed characters by adding an {!u_rep}
       character to the parsed data, see the {{:#examples}examples}.}}

    {b Note.} Repeated invocation always eventually returns [`End], even
    in case of errors.  *)

val decoder_encoding : decoder -> decoder_encoding
(** [decoder_encoding d] is [d]'s the decoded encoding scheme of [d].

    {b Warning.} If the decoder guesses the encoding or uses [`UTF_16],
    rely on this value only after the first [`Uchar] was decoded. *)

(**/**)

(* This function is dangerous, it may destroy the current continuation.
   But it's needed for things like XML parsers. *)

val set_decoder_encoding : decoder -> [< decoder_encoding] -> unit
(** [set_decoder_encoding d enc] changes the decoded encoding
    to [enc] after decoding started.

    {b Warning.} Call only after {!val-decode} was called on [d] and that the
    last call to it returned something different from [`Await] or data may
    be lost. After encoding guess wait for at least three [`Uchar]s. *)

(**/**)

val decoder_line : decoder -> int
(** [decoder_line d] is the line number of the last
    decoded (or malformed) character. See {!val-decoder} for details. *)

val decoder_col : decoder -> int
(** [decoder_col d] is the column number of the last decoded
    (or malformed) character. See {!val-decoder} for details. *)

val decoder_byte_count : decoder -> int
(** [decoder_byte_count d] is the number of bytes already decoded on
    [d] (including malformed ones). This is the last {!val-decode}'s
    end byte offset counting from the beginning of the stream. *)

val decoder_count : decoder -> int
(** [decoder_count d] is the number of characters already decoded on [d]
    (including malformed ones). See {!val-decoder} for details. *)

val decoder_removed_bom : decoder -> bool
(** [decoder_removed_bom d] is [true] iff an {e initial}
    {{:http://unicode.org/glossary/#byte_order_mark}BOM} was
    removed from the input stream. See {!val-decoder} for details. *)

val decoder_src : decoder -> src
(** [decoder_src d] is [d]'s input source. *)

val decoder_nln : decoder -> nln option
(** [decoder_nln d] returns [d]'s newline normalization (if any). *)

val pp_decode : Format.formatter ->
  [< `Await | `Uchar of Uchar.t | `End | `Malformed of string] -> unit
(** [pp_decode ppf v] prints an unspecified representation of [v] on
    [ppf]. *)

(** {1:encode Encode} *)

type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]
(** The type for output destinations. With a [`Manual] destination the client
    must provide output storage with {!Manual.dst}. *)

type encoder
(** The type for Unicode encoders. *)

val encoder : [< encoding] -> [< dst] -> encoder
(** [encoder encoding dst] is an encoder for [encoding] that outputs
    to [dst].

    {b Note.} No initial
    {{:http://unicode.org/glossary/#byte_order_mark}BOM}
    is encoded. If needed, this duty is left to the client. *)

val encode :
  encoder -> [<`Await | `End | `Uchar of Uchar.t ] -> [`Ok | `Partial ]
(** [encode e v] is :
    {ul
    {- [`Partial] iff [e] has a [`Manual] destination and needs more output
       storage. The client must use {!Manual.dst} to provide a new buffer
       and then call {!val-encode} with [`Await] until [`Ok] is returned.}
    {- [`Ok] when the encoder is ready to encode a new [`Uchar] or [`End]}}

    For [`Manual] destination, encoding [`End] always returns
    [`Partial], the client should continue as usual with [`Await]
    until [`Ok] is returned at which point {!Manual.dst_rem} [e] is
    guaranteed to be the size of the last provided buffer (i.e. nothing
    was written).

    {b Raises.} [Invalid_argument] if an [`Uchar] or [`End] is encoded
    after a [`Partial] encode. *)

val encoder_encoding : encoder -> encoding
(** [encoder_encoding e] is [e]'s encoding. *)

val encoder_dst : encoder -> dst
(** [encoder_dst e] is [e]'s output destination. *)

(** {1:manual Manual sources and destinations.} *)

(** Manual sources and destinations.

    {b Warning.} Use only with [`Manual] decoder and encoders. *)
module Manual : sig
  val src : decoder -> Bytes.t -> int -> int -> unit
  (** [src d s j l] provides [d] with [l] bytes to read, starting at
      [j] in [s]. This byte range is read by calls to {!val-decode} with [d]
      until [`Await] is returned. To signal the end of input call the function
      with [l = 0]. *)

  val dst : encoder -> Bytes.t -> int -> int -> unit
  (** [dst e s j l] provides [e] with [l] bytes to write, starting
      at [j] in [s]. This byte range is written by calls to
      {!val-encode} with [e] until [`Partial] is returned. Use {!dst_rem} to
      know the remaining number of non-written free bytes in [s]. *)

  val dst_rem : encoder -> int
  (** [dst_rem e] is the remaining number of non-written, free bytes
      in the last buffer provided with {!Manual.dst}. *)
end

(** {1:strbuf String folders and Buffer encoders} *)

(** Fold over the characters of UTF encoded OCaml [string] values.

    {b Note.} Since OCaml 4.14, UTF decoders are available in
    {!Stdlib.String}. You are encouraged to migrate to them. *)
module String : sig

(** {1 Encoding guess} *)

  val encoding_guess : string -> [ `UTF_8 | `UTF_16BE | `UTF_16LE ] * bool
  (** [encoding_guess s] is the encoding guessed for [s] coupled with
      [true] iff there's an initial
      {{:http://unicode.org/glossary/#byte_order_mark}BOM}. *)

(** {1 String folders}

    {b Note.} Initial {{:http://unicode.org/glossary/#byte_order_mark}BOM}s
    are also folded over. *)

  type 'a folder = 'a -> int -> [ `Uchar of Uchar.t | `Malformed of string ] ->
    'a
  (** The type for character folders. The integer is the index in the
      string where the [`Uchar] or [`Malformed] starts. *)

  val fold_utf_8 : ?pos:int -> ?len:int -> 'a folder -> 'a -> string -> 'a
  (** [fold_utf_8 f a s ?pos ?len ()] is
      [f (] ... [(f (f a pos u]{_0}[) j]{_1}[ u]{_1}[)] ... [)] ... [)
      j]{_n}[ u]{_n}
      where [u]{_i}, [j]{_i} are characters and their start position
      in the UTF-8 encoded substring [s] starting at [pos] and [len]
      long. The default value for [pos] is [0] and [len] is
      [String.length s - pos]. *)

  val fold_utf_16be : ?pos:int -> ?len:int -> 'a folder -> 'a -> string -> 'a
  (** [fold_utf_16be f a s ?pos ?len ()] is
      [f (] ... [(f (f a pos u]{_0}[) j]{_1}[ u]{_1}[)] ... [)] ... [)
      j]{_n}[ u]{_n}
      where [u]{_i}, [j]{_i} are characters and their start position
      in the UTF-8 encoded substring [s] starting at [pos] and [len]
      long. The default value for [pos] is [0] and [len] is
      [String.length s - pos]. *)

  val fold_utf_16le : ?pos:int -> ?len:int -> 'a folder -> 'a -> string -> 'a
  (** [fold_utf_16le f a s ?pos ?len ()] is
      [f (] ... [(f (f a pos u]{_0}[) j]{_1}[ u]{_1}[)] ... [)] ... [)
      j]{_n}[ u]{_n}
      where [u]{_i}, [j]{_i} are characters and their start position
      in the UTF-8 encoded substring [s] starting at [pos] and [len]
      long. The default value for [pos] is [0] and [len] is
      [String.length s - pos]. *)
end

(**  UTF encode characters in OCaml {!Buffer.t} values.

     {b Note.} Since OCaml 4.06, these encoders are available in
     {!Stdlib.Buffer}. You are encouraged to migrate to them. *)
module Buffer : sig

  (** {1 Buffer encoders} *)

  val add_utf_8 : Buffer.t -> Uchar.t -> unit
  (** [add_utf_8 b u] adds the UTF-8 encoding of [u] to [b]. *)

  val add_utf_16be : Buffer.t -> Uchar.t -> unit
  (** [add_utf_16be b u] adds the UTF-16BE encoding of [u] to [b]. *)

  val add_utf_16le : Buffer.t -> Uchar.t -> unit
  (** [add_utf_16le b u] adds the UTF-16LE encoding of [u] to [b]. *)
end

(** {1:examples Examples}

    {2:readlines Read lines}

    The value of [lines src] is the list of lines in [src] as UTF-8
    encoded OCaml strings. Line breaks are determined according to the
    recommendation R4 for a [readline] function in section 5.8 of
    Unicode 9.0.0. If a decoding error occurs we silently replace the
    malformed sequence by the replacement character {!u_rep} and continue.
{[let lines ?encoding (src : [`Channel of in_channel | `String of string]) =
  let rec loop d buf acc = match Uutf.decode d with
  | `Uchar u ->
      begin match Uchar.to_int u with
      | 0x000A ->
          let line = Buffer.contents buf in
          Buffer.clear buf; loop d buf (line :: acc)
      | _ ->
          Uutf.Buffer.add_utf_8 buf u; loop d buf acc
      end
  | `End -> List.rev (Buffer.contents buf :: acc)
  | `Malformed _ -> Uutf.Buffer.add_utf_8 buf Uutf.u_rep; loop d buf acc
  | `Await -> assert false
  in
  let nln = `Readline (Uchar.of_int 0x000A) in
  loop (Uutf.decoder ~nln ?encoding src) (Buffer.create 512) []
]}
  Using the [`Manual] interface, [lines_fd] does the same but on a Unix file
  descriptor.
{[let lines_fd ?encoding (fd : Unix.file_descr) =
  let rec loop fd s d buf acc = match Uutf.decode d with
  | `Uchar u ->
      begin match Uchar.to_int u with
      | 0x000A ->
          let line = Buffer.contents buf in
          Buffer.clear buf; loop fd s d buf (line :: acc)
      | _ ->
          Uutf.Buffer.add_utf_8 buf u; loop fd s d buf acc
      end
  | `End -> List.rev (Buffer.contents buf :: acc)
  | `Malformed _ -> Uutf.Buffer.add_utf_8 buf Uutf.u_rep; loop fd s d buf acc
  | `Await ->
      let rec unix_read fd s j l = try Unix.read fd s j l with
      | Unix.Unix_error (Unix.EINTR, _, _) -> unix_read fd s j l
      in
      let rc = unix_read fd s 0 (Bytes.length s) in
      Uutf.Manual.src d s 0 rc; loop fd s d buf acc
  in
  let s = Bytes.create 65536 (* UNIX_BUFFER_SIZE in 4.0.0 *) in
  let nln = `Readline (Uchar.of_int 0x000A) in
  loop fd s (Uutf.decoder ~nln ?encoding `Manual) (Buffer.create 512) []
]}

    {2:recode Recode}

    The result of [recode src out_encoding dst] has the characters of
    [src] written on [dst] with encoding [out_encoding].  If a
    decoding error occurs we silently replace the malformed sequence
    by the replacement character {!u_rep} and continue.  Note that we
    don't add an initial
    {{:http://unicode.org/glossary/#byte_order_mark}BOM} to [dst],
    recoding will thus loose the initial BOM [src] may have. Whether
    this is a problem or not depends on the context.
{[let recode ?nln ?encoding out_encoding
    (src : [`Channel of in_channel | `String of string])
    (dst : [`Channel of out_channel | `Buffer of Buffer.t])
  =
  let rec loop d e = match Uutf.decode d with
  | `Uchar _ as u -> ignore (Uutf.encode e u); loop d e
  | `End -> ignore (Uutf.encode e `End)
  | `Malformed _ -> ignore (Uutf.encode e (`Uchar Uutf.u_rep)); loop d e
  | `Await -> assert false
  in
  let d = Uutf.decoder ?nln ?encoding src in
  let e = Uutf.encoder out_encoding dst in
  loop d e]}
  Using the [`Manual] interface, [recode_fd] does the same but between
  Unix file descriptors.
{[let recode_fd ?nln ?encoding out_encoding
    (fdi : Unix.file_descr)
    (fdo : Unix.file_descr)
  =
  let rec encode fd s e v = match Uutf.encode e v with `Ok -> ()
  | `Partial ->
      let rec unix_write fd s j l =
        let rec write fd s j l = try Unix.single_write fd s j l with
        | Unix.Unix_error (Unix.EINTR, _, _) -> write fd s j l
        in
        let wc = write fd s j l in
        if wc < l then unix_write fd s (j + wc) (l - wc) else ()
      in
      unix_write fd s 0 (Bytes.length s - Uutf.Manual.dst_rem e);
      Uutf.Manual.dst e s 0 (Bytes.length s);
      encode fd s e `Await
  in
  let rec loop fdi fdo ds es d e = match Uutf.decode d with
  | `Uchar _ as u -> encode fdo es e u; loop fdi fdo ds es d e
  | `End -> encode fdo es e `End
  | `Malformed _ -> encode fdo es e (`Uchar Uutf.u_rep); loop fdi fdo ds es d e
  | `Await ->
      let rec unix_read fd s j l = try Unix.read fd s j l with
      | Unix.Unix_error (Unix.EINTR, _, _) -> unix_read fd s j l
      in
      let rc = unix_read fdi ds 0 (Bytes.length ds) in
      Uutf.Manual.src d ds 0 rc; loop fdi fdo ds es d e
  in
  let ds = Bytes.create 65536 (* UNIX_BUFFER_SIZE in 4.0.0 *) in
  let es = Bytes.create 65536 (* UNIX_BUFFER_SIZE in 4.0.0 *) in
  let d = Uutf.decoder ?nln ?encoding `Manual in
  let e = Uutf.encoder out_encoding `Manual in
  Uutf.Manual.dst e es 0 (Bytes.length es);
  loop fdi fdo ds es d e]}
*)

(*---------------------------------------------------------------------------
   Copyright (c) 2012 The uutf programmers

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
