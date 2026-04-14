Single-module library consumers and recompilation behavior.

When a consumer library has only one module, dune skips ocamldep for that
stanza as an optimization (no intra-stanza deps to compute). This means
per-module library dependency filtering cannot determine which libraries
the module actually references, so the consumer depends on all library
files via glob. Modifying an unused module in a dependency triggers
unnecessary recompilation of the consumer module's .cmo/.cmx.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ mkdir base_lib
  $ cat > base_lib/dune <<EOF
  > (library (name base_lib) (wrapped false))
  > EOF
  $ cat > base_lib/alpha.ml <<EOF
  > let alpha_val = 1
  > EOF
  $ cat > base_lib/alpha.mli <<EOF
  > val alpha_val : int
  > EOF
  $ cat > base_lib/unused.ml <<EOF
  > let unused_val = 99
  > EOF
  $ cat > base_lib/unused.mli <<EOF
  > val unused_val : int
  > EOF

  $ mkdir consumer_lib
  $ cat > consumer_lib/dune <<EOF
  > (library (name consumer_lib) (wrapped false) (libraries base_lib))
  > EOF
  $ cat > consumer_lib/uses_alpha.ml <<EOF
  > let f () = Alpha.alpha_val
  > EOF
  $ cat > consumer_lib/uses_alpha.mli <<EOF
  > val f : unit -> int
  > EOF

  $ cat > dune <<EOF
  > (executable (name main) (libraries consumer_lib))
  > EOF
  $ cat > main.ml <<EOF
  > let () = print_int (Uses_alpha.f ())
  > EOF

  $ dune build ./main.exe

Modify only the unused module:

  $ cat > base_lib/unused.mli <<EOF
  > val unused_val : int
  > val new_fn : unit -> string
  > EOF
  $ cat > base_lib/unused.ml <<EOF
  > let unused_val = 99
  > let new_fn () = "new"
  > EOF

uses_alpha.cmx is recompiled even though uses_alpha.ml only references
Alpha, not Unused. This is because the consumer_lib stanza has a single
module, so dune skips ocamldep for it and falls back to glob deps on all
of base_lib's .cmi files. The trace shows compilation targets for
uses_alpha being rebuilt:

  $ dune build ./main.exe
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("uses_alpha"))]'
  [
    {
      "target_files": [
        "_build/default/consumer_lib/.consumer_lib.objs/byte/uses_alpha.cmi",
        "_build/default/consumer_lib/.consumer_lib.objs/byte/uses_alpha.cmti"
      ]
    },
    {
      "target_files": [
        "_build/default/consumer_lib/.consumer_lib.objs/native/uses_alpha.cmx",
        "_build/default/consumer_lib/.consumer_lib.objs/native/uses_alpha.o"
      ]
    }
  ]
