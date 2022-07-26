This tests the support for the bigarray atom in the dune libraries stanza.

History:
- OCaml 4.05 ([ocaml/ocaml#997](https://github.com/ocaml/ocaml/pull/997) and
[ocaml/ocaml#1077](https://github.com/ocaml/ocaml/pull/1077)) add
`Unix.map_file` allowing the `map_file` functions in `Bigarray` to be
deprecated. Bigarray remains a separate library.
- OCaml 4.07 ([ocaml/ocaml#1685](https://github.com/ocaml/ocaml/pull/1685))
adds `Stdlib.Bigarray`, but without the `map_file` functions (since these
required `Unix.file_descr`. The separate library remains with those
functions (but still marked as deprecated). Code can be updated to use
Unix.map_file and then Stdlib.Bigarray and not require the separate library
at all, but the separate remains compatible with OCaml 4.06.
- OCaml 4.08 ([ocaml/ocaml#2263](https://github.com/ocaml/ocaml/pull/2263))
deletes the `map_file` functions completely, requiring _all_ code to be
updated to use `Unix.map_file`, if appropriate. From this release, it is
unnecessary to link with the separate Bigarray library.
- OCaml 5.00 ([ocaml/ocaml#10896](https://github.com/ocaml/ocaml/pull/10896)
removes the separate Bigarray library.

Code may be written which is designed to support both OCaml 4.06 and earlier and
also OCaml 5.0+. In such cases, it is appropriate to have `(libraries bigarray)`
even though there is no Bigarray library in OCaml 5.

This test uses `(libraries bigarray)` (the program uses `Bigarray`)
  $ dune exec a/a.exe
  Welcome to a
This test uses `(libraries (re_export bigarray))` similarly
  $ dune exec b/b.exe
  Welcome to b
This test uses a `(select )` construct and should always select bigarray support
  $ dune exec c/c.exe
  Welcome to c WITH bigarray support
