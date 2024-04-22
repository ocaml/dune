Parsing of Dune Files
---------------------

Parsing ``dune`` files is done in two steps:

- They are parsed as S-expressions using :file:`src/dune_sexp/parser.mli`;
- Then they are decoded using :file:`src/dune_sexp/decoder.mli`. The result of
  this decoding step is added to an extensible variant using a mechanism in
  :file:`src/dune_lang/stanza.mli`.

Instead of writing a parser or using pattern matching, we define decoders,
which are abstract values of type ``'a Decoder.t`` (returning a value of type
``'a``). These decoders are assembled using combinators. For example, we can
use simple decoders to write a decoder for a record type. This decoder
abstraction is monadic, but the applicative subset is sufficient for most
decoders.

As an example, here is how ``(copy_files)`` is parsed:

`src/dune_rules/stanzas/copy_files.ml <https://github.com/ocaml/dune/blob/3.15.0/src/dune_rules/stanzas/copy_files.ml#L31-L50>`_
  .. code-block:: ocaml
    :linenos:
    :lineno-start: 31

    let long_form =
      let check = Dune_lang.Syntax.since Stanza.syntax (2, 7) in
      let+ alias = field_o "alias" (check >>> Dune_lang.Alias.decode)
      and+ mode = field "mode" ~default:Rule.Mode.Standard (check >>> Rule_mode_decoder.decode)
      and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:(Some (2, 8)) ()
      and+ files = field "files" (check >>> String_with_vars.decode)
      and+ only_sources =
        field_o
          "only_sources"
          (Dune_lang.Syntax.since Stanza.syntax (3, 14) >>> decode_only_sources)
      and+ syntax_version = Dune_lang.Syntax.get_exn Stanza.syntax in
      let only_sources = Option.value only_sources ~default:Blang.false_ in
      { add_line_directive = false
      ; alias
      ; mode
      ; enabled_if
      ; files
      ; only_sources
      ; syntax_version
      }

The fields are queried individually, and a record is built using all the
intermediate results. This will automatically take care of generating "unknown
field X," "duplicate field X," and similar error messages.

Another interesting thing to note is that the fields are not decoded directly,
but use the following pattern:

.. code:: ocaml

   Syntax.since Stanza.syntax (x, y) >>> decoder

Let's unpack this: ``(>>>)`` will run a ``unit Decoder.t`` on the input before
passing the input to an actual decoder. The first decoder can be used to
implement a check and trigger an error in some cases.

Here, it is used for versioning. For example the ``(copy_files)`` stanza
started supporting ``(enabled_if``) in version 2.8. Decoding this field is
protected by this ``since`` call: it means that if the language version in
:doc:`/reference/dune-project/index` file is greater than 2.8. In particular,
this ensures that the project can not be built with Dune versions older than
``2.8.0``.

Once decoding succeeds, various stanzas are turned into various types defined
in :file:`src/dune_rules/stanzas/`.
