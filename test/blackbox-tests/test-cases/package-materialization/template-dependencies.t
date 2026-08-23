A META file template may add a library requirement that is absent from Dune's
internal library graph. The scoped layout must not emit that requirement
without also handling its dependency.

  $ make_dune_project 3.24
  $ cat >>dune-project <<EOF
  > (package (name template-root))
  > (package (name template-middle))
  > (package (name template-extra))
  > (package (name template-explicit))
  > (package (name template-safe-root))
  > (package (name template-safe-middle))
  > EOF

  $ mkdir root middle extra unrelated explicit safe-root safe-middle
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

A dependency change in a template subpackage outside the selected library
hierarchy does not affect the scoped metadata.

  $ cat >safe-root/dune <<'EOF'
  > (library
  >  (name safe_root)
  >  (public_name template-safe-root)
  >  (libraries template-safe-middle.selected))
  > EOF
  $ echo 'let value = ()' >safe-root/safe_root.ml

  $ cat >safe-middle/dune <<'EOF'
  > (library
  >  (name selected)
  >  (public_name template-safe-middle.selected))
  > EOF
  $ echo 'let value = ()' >safe-middle/selected.ml

  $ cat >META.template-safe-middle.template <<'EOF'
  > # DUNE_GEN
  > package "unused" (requires = "missing")
  > EOF

Ordinary installation continues to accept and install the custom templates.

  $ dune build @install

A scoped package dependency rejects a template-only edge rather than leaving it
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
  > (rule
  >  (target safe-result)
  >  (deps (package template-safe-root))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:ocamlfind} query -recursive template-safe-root))))
  > (rule
  >  (target explicit-unused)
  >  (deps (package template-safe-middle))
  >  (action (write-file %{target} "unused")))
  > EOF

The template-only edge is rejected even when the ordinary middle dependency is
also requested explicitly.

  $ dune build recursive-with-middle
  File "META.template-middle.template", line 1, characters 0-0:
  Error: Package template-middle has a META file template that changes library
  dependencies or artifact metadata.
  Such templates cannot be used in a scoped package dependency.
  [1]

  $ dune build recursive
  File "META.template-middle.template", line 1, characters 0-0:
  Error: Package template-middle has a META file template that changes library
  dependencies or artifact metadata.
  Such templates cannot be used in a scoped package dependency.
  [1]

The same problem occurs when the dependency-changing template belongs to the
explicitly requested package.

  $ dune build explicit-result
  File "META.template-explicit.template", line 1, characters 0-0:
  Error: Package template-explicit has a META file template that changes
  library dependencies or artifact metadata.
  Such templates cannot be used in a scoped package dependency.
  [1]

The unrelated template subpackage is filtered before validating support
metadata.

  $ dune build safe-result

An explicitly requested package retains its complete META, so the same
subpackage is rejected there.

  $ dune build explicit-unused
  File "META.template-safe-middle.template", line 1, characters 0-0:
  Error: Package template-safe-middle has a META file template that changes
  library dependencies or artifact metadata.
  Such templates cannot be used in a scoped package dependency.
  [1]
