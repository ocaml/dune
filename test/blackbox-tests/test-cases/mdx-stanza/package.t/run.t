The mdx stanza supports (package):

  $ dune runtest
  File "doc-a.md", line 1, characters 0-0:
  --- doc-a.md
  +++ .mdx/doc-a.md.corrected
  @@ -1,3 +1,4 @@
   ```ocaml
   # let x = 1;;
  +val x : int = 1
   ```
  File "doc-b.md", line 1, characters 0-0:
  --- doc-b.md
  +++ .mdx/doc-b.md.corrected
  @@ -1,3 +1,4 @@
   ```ocaml
   # let x = 1;;
  +val x : int = 1
   ```
  File "doc-nopkg.md", line 1, characters 0-0:
  --- doc-nopkg.md
  +++ .mdx/doc-nopkg.md.corrected
  @@ -1,3 +1,4 @@
   ```ocaml
   # let x = 1;;
  +val x : int = 1
   ```
  [1]

In the following test doc-a is not checked because it is not part of package b
  $ dune runtest --only-packages b
  File "doc-b.md", line 1, characters 0-0:
  --- doc-b.md
  +++ .mdx/doc-b.md.corrected
  @@ -1,3 +1,4 @@
   ```ocaml
   # let x = 1;;
  +val x : int = 1
   ```
  File "doc-nopkg.md", line 1, characters 0-0:
  --- doc-nopkg.md
  +++ .mdx/doc-nopkg.md.corrected
  @@ -1,3 +1,4 @@
   ```ocaml
   # let x = 1;;
  +val x : int = 1
   ```
  [1]
