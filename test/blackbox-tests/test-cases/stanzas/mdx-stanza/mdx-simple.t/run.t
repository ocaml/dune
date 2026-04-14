You can use the mdx stanza to check your documentation in markdown and mli files

  $ dune runtest
  File "README.md", line 1, characters 0-0:
  --- README.md
  +++ .mdx/README.md.corrected
  @@ -2,5 +2,5 @@
   
   ```ocaml
   # 1 + 1;;
  -- : int = 3
  +- : int = 2
   ```
  [1]
