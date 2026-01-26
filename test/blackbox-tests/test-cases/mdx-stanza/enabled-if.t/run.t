The mdx stanza supports (enabled_if):

  $ dune runtest
  File "iftrue.md", line 1, characters 0-0:
  --- iftrue.md
  +++ .mdx/iftrue.md.corrected
  @@ -1,3 +1,5 @@
   ```ocaml
   # print_endline "run in iftrue";;
  +run in iftrue
  +- : unit = ()
   ```
  [1]
