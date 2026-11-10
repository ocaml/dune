When two packages in the same (deps (package ...)) set install a file
to the same un-namespaced destination in a _root section (lib_root,
share_root, libexec_root), the layout materialization fails with a
user error naming the conflicting packages and entry.

  $ make_dune_project 3.24
  $ cat >>dune-project <<'EOF'
  > (package (name pkg-a))
  > (package (name pkg-b))
  > EOF

  $ mkdir pkg-a-src pkg-b-src

Both packages install a file named "topfind" to lib_root:

  $ echo "from pkg-a" > pkg-a-src/topfind
  $ cat >pkg-a-src/dune <<'EOF'
  > (install (section lib_root) (files topfind) (package pkg-a))
  > EOF

  $ echo "from pkg-b" > pkg-b-src/topfind
  $ cat >pkg-b-src/dune <<'EOF'
  > (install (section lib_root) (files topfind) (package pkg-b))
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package pkg-a) (package pkg-b))
  >  (target out)
  >  (action (with-stdout-to %{target} (echo "ok"))))
  > EOF

The materialization step fails with a user error naming the two
conflicting packages and the install entry:

  $ dune build out 2>&1 | censor
  Error: "pkg-a" and "pkg-b" both install "topfind" to section lib_root.
  The lib_root, share_root, and libexec_root sections install directly to the
  section root with no per-package subdirectory, so file names must be unique
  across the set of packages a rule depends on.
  -> required by _build/default/out
  Hint: Rename one of the install entries.
  [1]

The same policy applies when a native package and an opaque package collide.

  $ mkdir mixed && cd mixed
  $ cat >dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name native) (dir native))
  > (package (name opaque) (dir opaque) (allow_empty))
  > EOF
  $ mkdir native opaque
  $ echo native > native/topfind
  $ cat >native/dune <<'EOF'
  > (install
  >  (package native)
  >  (section lib_root)
  >  (files topfind))
  > EOF
  $ cat >opaque/dune <<'EOF'
  > (opam
  >  (package opaque)
  >  (dir src)
  >  (version 1.0)
  >  (build
  >   (progn
  >    (write-file topfind "opaque\n")
  >    (write-file opaque.install "lib_root: [ \"topfind\" ]\n"))))
  > EOF
  $ cat >dune <<'EOF'
  > (rule
  >  (target out)
  >  (deps (package native) (package opaque))
  >  (action (write-file %{target} ok)))
  > EOF

The diagnostic names both kinds of producer and the shared destination.

  $ dune build out 2>&1 | censor
  Error: "native" and "opaque" both install "topfind" to section lib_root.
  The lib_root, share_root, and libexec_root sections install directly to the
  section root with no per-package subdirectory, so file names must be unique
  across the set of packages a rule depends on.
  -> required by _build/default/out
  Hint: Rename one of the install entries.
  [1]
