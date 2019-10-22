.. _metadata-format:

*******************
Lexical conventions
*******************

All configuration files read by Dune are using a syntax similar to the
one of S-expressions, which is very simple. The Dune language can
represent three kinds of values: atoms, strings and lists. By
combining these, it is possible to construct arbitrarily complex
project descriptions.

A Dune configuration file is a sequence of atoms, strings or lists
separated by spaces, newlines and comments. The other sections of this
manual describe how each configuration file is interpreted. We
describe below the syntax of the language.

Comments
========

The Dune language only has end of line comments. End of line comments
are introduced with a semicolon and span up to the end of the end of
the current line. Everything from the semicolon to the end of the line
is ignored. For instance:

.. code::

   ; This is a comment

Atoms
=====

An atom is a non-empty contiguous sequences of character other than
special characters. Special characters are:

- spaces, horizontal tabs, newlines and form feed
- opening and closing parenthesis
- double quotes
- semicolons

For instance ``hello`` or ``+`` are valid atoms.

Note that backslashes inside atoms have no special meaning are always
interpreted as plain backslashes characters.

Strings
=======

A string is a sequence of characters surrounded by double quotes. A
string represent the exact text between the double quotes, except for
escape sequences. Escape sequence are introduced by the a backslash
character. Dune recognizes and interprets the following escape
sequences:

- ``\n`` to represent a newline character
- ``\r`` to represent a carriage return (character with ASCII code 13)
- ``\b`` to represent ASCII character 8
- ``\t`` to represent a horizontal tab
- ``\NNN``, a backslash followed by three decimal characters to
  represent the character with ASCII code ``NNN``
- ``\xHH``, a backslash followed by two hexadecimal characters to
  represent the character with ASCII code ``HH`` in hexadecimal
- ``\\``, a double backslash to represent a single backslash
- ``\%{`` to represent ``%{`` (see :ref:`variables`)

Additionally, a backslash that comes just before the end of the line
is used to skip the newline up to the next non-space character. For
instance the following two strings represent the same text:

.. code::

   "abcdef"
   "abc\
      def"

In most places where Dune expect a string, it will also accept an
atom. As a result it possible to write most Dune configuration file
using very few double quotes. This is very convenient in practice.

End of line strings
===================

End of line strings are another way to write strings. The are a
convenient way to write blocks of text inside a Dune file.

End of line strings are introduced by ``"\|`` or ``"\>`` and span up
the end of the current line. If the next line starts as well by
``"\|`` or ``"\>`` it is the continuation of the same string. For
readability, it is necessary that the text that follows the delimiter
is either empty or starts with a space that is ignored.

For instance:

.. code::

   "\| this is a block
   "\| of text

represent the same text as the string ``"this is a block\nof text"``.

Escape sequences are interpreted in text that follows ``"\|`` but not
in text that follows ``"\>``. Both delimiters can be mixed inside the
same block of text.

Lists
=====

Lists are sequences of values enclosed by parentheses. For instance
``(x y z)`` is a list containing the three atoms ``x``, ``y`` and
``z``. Lists can be empty, for instance: ``()``.

Lists can be nested, allowing to represent arbitrarily complex
descriptions. For instance:

.. code::

   (html
    (head (title "Hello world!"))
    (body
      This is a simple example of using S-expressions))
