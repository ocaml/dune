A META file template may add a library requirement that is absent from Dune's
internal library graph. The scoped layout must not emit that requirement
without also handling its dependency.

  $ make_dune_project 3.24
  $ cat >>dune-project <<EOF
  > (package (name template-root))
  > (package (name template-middle))
  > (package (name template-extra))
  > (package (name template-explicit))
  > EOF

  $ mkdir root middle extra unrelated explicit
  $ cat >root/dune <<'EOF'
  > (library
  >  (name root)
  >  (public_name template-root)
  >  (libraries template-middle.selected))
  > EOF
  $ echo 'let value = ()' >root/root.ml

  $ cat >middle/dune <<'EOF'
  > (library
  >  (name selected)
  >  (public_name template-middle.selected))
  > EOF
  $ echo 'let value = ()' >middle/selected.ml

  $ cat >extra/dune <<'EOF'
  > (library
  >  (name selected)
  >  (public_name template-extra.selected))
  > EOF
  $ echo 'let value = ()' >extra/selected.ml

  $ cat >unrelated/dune <<'EOF'
  > (library
  >  (name unrelated)
  >  (public_name template-extra.unrelated))
  > EOF
  $ echo 'let value = ()' >unrelated/unrelated.ml

  $ cat >explicit/dune <<'EOF'
  > (library
  >  (name explicit)
  >  (public_name template-explicit))
  > EOF
  $ echo 'let value = ()' >explicit/explicit.ml

  $ cat >META.template-explicit.template <<'EOF'
  > requires += "template-extra.selected"
  > # DUNE_GEN
  > EOF

  $ cat >META.template-middle.template <<'EOF'
  > package "selected" (
  >  directory = "selected"
  >  requires = "template-extra.selected"
  > )
  > EOF

Ordinary installation continues to accept and install the custom template.

  $ dune build @install

A scoped package dependency currently omits even the ordinary Dune library
closure. After that closure is added, the template-only edge must not be left
dangling.

  $ cat >dune <<'EOF'
  > (rule
  >  (target recursive)
  >  (deps (package template-root))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:ocamlfind} query -recursive template-root))))
  > (rule
  >  (target explicit-result)
  >  (deps (package template-explicit))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:ocamlfind} query -recursive template-explicit))))
  > (rule
  >  (target recursive-with-middle)
  >  (deps
  >   (package template-root)
  >   (package template-middle))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:ocamlfind} query -recursive template-root))))
  > EOF

Making the ordinary middle dependency explicit exposes the template-only edge
rather than failing on the missing library closure first.

  $ dune build recursive-with-middle
  File "dune", lines 13-20, characters 0-196:
  13 | (rule
  14 |  (target recursive-with-middle)
  15 |  (deps
  16 |   (package template-root)
  17 |   (package template-middle))
  18 |  (action
  19 |   (with-stdout-to %{target}
  20 |    (run %{bin:ocamlfind} query -recursive template-root))))
  ocamlfind: Package `template-extra.selected' not found - required by `template-middle.selected'
  [1]

  $ dune build recursive
  File "dune", lines 1-6, characters 0-154:
  1 | (rule
  2 |  (target recursive)
  3 |  (deps (package template-root))
  4 |  (action
  5 |   (with-stdout-to %{target}
  6 |    (run %{bin:ocamlfind} query -recursive template-root))))
  ocamlfind: Package `template-middle.selected' not found - required by `template-root'
  [1]

The same problem occurs when the dependency-changing template belongs to the
explicitly requested package.

  $ dune build explicit-result
  File "dune", lines 7-12, characters 0-168:
   7 | (rule
   8 |  (target explicit-result)
   9 |  (deps (package template-explicit))
  10 |  (action
  11 |   (with-stdout-to %{target}
  12 |    (run %{bin:ocamlfind} query -recursive template-explicit))))
  ocamlfind: Package `template-extra.selected' not found - required by `template-explicit'
  [1]
