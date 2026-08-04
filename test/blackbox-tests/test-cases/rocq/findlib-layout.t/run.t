Rocq public theories use findlib-style packages and install under rocq.d.

  $ make_rocq_project 3.25 0.15
  $ cat >> dune-project << EOF
  > (package (name pkg))
  > EOF
  $ touch pkg.opam

A public theory with a public_name is installed in its findlib package
(directory) under rocq.d, not under coq/user-contrib.

  $ mkdir A
  $ cat > A/dune << EOF
  > (rocq.theory
  >  (name A)
  >  (public_name pkg)
  >  (synopsis "A theory"))
  > EOF
  $ cat > A/a.v << EOF
  > Definition x := 0.
  > EOF

The legacy layout can still be requested explicitly.

  $ mkdir B
  $ cat > B/dune << EOF
  > (rocq.theory
  >  (name B)
  >  (public_name pkg.b)
  >  (theories A)
  >  (legacy_install))
  > EOF
  $ cat > B/b.v << EOF
  > From A Require Import a.
  > Definition y := x.
  > EOF

  $ dune build @install
  $ find _build/install/default/lib -type l | sort
  _build/install/default/lib/coq/user-contrib/B/b.glob
  _build/install/default/lib/coq/user-contrib/B/b.v
  _build/install/default/lib/coq/user-contrib/B/b.vo
  _build/install/default/lib/pkg/META
  _build/install/default/lib/pkg/b/rocq.d/b.glob
  _build/install/default/lib/pkg/b/rocq.d/b.v
  _build/install/default/lib/pkg/b/rocq.d/b.vo
  _build/install/default/lib/pkg/dune-package
  _build/install/default/lib/pkg/opam
  _build/install/default/lib/pkg/rocq.d/a.glob
  _build/install/default/lib/pkg/rocq.d/a.v
  _build/install/default/lib/pkg/rocq.d/a.vo

  $ cat _build/install/default/lib/pkg/META
  description = "A theory"
  rocqpath = "A"
  requires = "rocq-core"
  package "b" (
    directory = "b"
    description = ""
    rocqpath = "B"
    requires = "pkg rocq-core"
  )
