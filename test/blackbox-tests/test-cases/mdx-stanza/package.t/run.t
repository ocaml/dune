The mdx stanza supports (package):

  $ dune runtest
  File "doc-a.md", line 1, characters 0-0:
  Error: Files _build/default/doc-a.md and
  _build/default/.mdx/doc-a.md.corrected differ.
  File "doc-b.md", line 1, characters 0-0:
  Error: Files _build/default/doc-b.md and
  _build/default/.mdx/doc-b.md.corrected differ.
  File "doc-nopkg.md", line 1, characters 0-0:
  Error: Files _build/default/doc-nopkg.md and
  _build/default/.mdx/doc-nopkg.md.corrected differ.
  [1]

In the following test doc-a is not checked because it is not part of package b
  $ dune runtest --only-packages b
  File "doc-b.md", line 1, characters 0-0:
  Error: Files _build/default/doc-b.md and
  _build/default/.mdx/doc-b.md.corrected differ.
  File "doc-nopkg.md", line 1, characters 0-0:
  Error: Files _build/default/doc-nopkg.md and
  _build/default/.mdx/doc-nopkg.md.corrected differ.
  [1]
