.. highlight:: dune

#####################
 Lexical Conventions
#####################

All configuration files read by Dune use a simple syntax that's similar
to S-expressions. The Dune language can represent three kinds of values:
atoms, strings, and lists. By combining these, it's possible to
construct arbitrarily complex project descriptions.

A Dune configuration file is a sequence of atoms, strings, or lists
separated by spaces, newlines, and comments. The other sections of this
manual describe how each configuration file is interpreted, and we
illustrate the syntax below:

**********
 Comments
**********

The Dune language only has end of line comments. A semicolon introduces
end of line comments and span up to the end of the current line. The
system ignores everything from the semicolon to the end of the line. For
instance:

.. code::

   ; This is a comment

*******
 Atoms
*******

An atom is a non-empty contiguous sequences of character other than
special characters. Special characters are:

-  spaces, horizontal tabs, newlines and form feed
-  opening and closing parenthesis
-  double quotes
-  semicolons

For instance ``hello`` or ``+`` are valid atoms.

Note that backslashes inside atoms have no special meaning and Dune
always interprets them as plain backslash characters.

*********
 Strings
*********

A string is a sequence of characters surrounded by double quotes. A
string represent the exact text between the double quotes, except for
escape sequences. A backslash character introduces escape sequences.
Dune recognizes and interprets the following escape sequences:

-  ``\n`` to represent a newline character
-  ``\r`` to represent a carriage return (character with ASCII code 13)
-  ``\b`` to represent ASCII character 8
-  ``\t`` to represent a horizontal tab
-  ``\NNN``, a backslash followed by three decimal characters to
   represent the character with ASCII code ``NNN``
-  ``\xHH``, a backslash followed by two hexadecimal characters to
   represent the character with ASCII code ``HH`` in hexadecimal
-  ``\\``, a double backslash to represent a single backslash
-  ``\%{`` to represent ``%{`` (see :doc:`../concepts/variables`)

Additionally, you can use a backslash just before the end of the line.
This skips the newline leading up to the next non-space character. For
instance, the following two strings represent the same text:

.. code::

   "abcdef"
   "abc\
      def"

In most places where Dune expects a string, it will also accept an atom.
As a result, it's possible to write most Dune configuration files using
very few double quotes. This is very convenient in practice.

*********************
 End of Line Strings
*********************

You can also write string using end of line strings. They are a
convenient way to write blocks of text inside a Dune file.

The characters ``"\|`` or ``"\>`` introduce end of line strings and span
to the end of the current line. If the next line also starts with
``"\|`` or ``"\>``, Dune reads it as a continuation of the same string.
For readability, either leave the text following the delimiter empty or
start it with a space (that will be ignored).

For instance:

.. code::

   "\| this is a block
   "\| of text

represents the same text as the string ``"this is a block\nof text"``.

Escape sequences are interpreted in text that follows ``"\|`` but not in
text that follows ``"\>``. Both delimiters can be mixed inside the same
block of text.

*******
 Lists
*******

Lists are sequences of values enclosed by parentheses. For instance ``(x
y z)`` is a list containing the three atoms ``x``, ``y`` and ``z``.
Lists can be empty, for instance: ``()``.

Lists can be nested, allowing arbitrary representation for complex
descriptions. For instance:

.. code::

   (html
    (head (title "Hello world!"))
    (body
      This is a simple example of using S-expressions))
