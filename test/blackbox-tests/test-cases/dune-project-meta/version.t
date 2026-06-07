Version generated in opam and META files
----------------------------------------

After calling `dune subst`, dune should embed the version inside the
generated META and opam files.

### With opam files and no package stanzas

  $ cat > dune-project <<EOF
  > (lang dune 1.10)
  > (name foo)
  > EOF

  $ cat > foo.opam <<EOF
  > EOF

  $ cat > dune <<EOF
  > (library (public_name foo))
  > EOF

  $ (git init -q
  >  git add .
  >  git commit -qm _
  >  git tag -a 1.0 -m 1.0
  >  dune subst)

  $ dune build foo.opam META.foo

  $ grep ^version foo.opam
  version: "1.0"

  $ grep ^version _build/default/META.foo
  version = "1.0"

With package stanzas and generating the opam files

  $ cat > dune-project <<EOF
  > (lang dune 1.10)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo))
  > EOF

  $ (git tag -d 1.0 >/dev/null
  >  git add .
  >  git commit -qm _
  >  git tag -a 1.0 -m 1.0
  >  dune subst)

  $ dune build foo.opam META.foo

  $ grep ^version foo.opam
  version: "1.0"

  $ grep ^version _build/default/META.foo
  version = "1.0"

Version source precedence without dune subst
--------------------------------------------

The previous tests cover versions written by ``dune subst``. Without
substitution, generated package metadata uses the package version stored in
Dune's package description. In particular, Dune does not read the VCS version
when generating META files during an ordinary build.

  $ show_meta_version() {
  >   dir=$1
  >   dune build --root "$dir" META.foo
  >   grep '^version' "$dir/_build/default/META.foo" || echo "no version"
  > }

When there are no ``(package ...)`` stanzas, Dune infers the package from the
opam file and uses the opam file's version before the top-level project version.

  $ mkdir opam-only
  $ cat > opam-only/dune-project <<EOF
  > (lang dune 3.24)
  > (name foo)
  > (version 2.0.0)
  > EOF
  $ cat > opam-only/foo.opam <<EOF
  > opam-version: "2.0"
  > version: "1.0.0"
  > EOF
  $ cat > opam-only/dune <<EOF
  > (library (public_name foo))
  > EOF
  $ touch opam-only/foo.ml
  $ show_meta_version opam-only
  version = "1.0.0"

When a ``(package ...)`` stanza exists, Dune uses that package description. A
package-specific version overrides the top-level project version and any version
in an existing opam file.

  $ mkdir package-version
  $ cat > package-version/dune-project <<EOF
  > (lang dune 3.24)
  > (name foo)
  > (version 2.0.0)
  > (package
  >  (name foo)
  >  (version 3.0.0))
  > EOF
  $ cat > package-version/foo.opam <<EOF
  > opam-version: "2.0"
  > version: "1.0.0"
  > EOF
  $ cat > package-version/dune <<EOF
  > (library (public_name foo))
  > EOF
  $ touch package-version/foo.ml
  $ show_meta_version package-version
  version = "3.0.0"

If the package stanza has no version, the top-level project version is used.
The opam file's version is not used for packages declared in ``dune-project``.

  $ mkdir project-version
  $ cat > project-version/dune-project <<EOF
  > (lang dune 3.24)
  > (name foo)
  > (version 2.0.0)
  > (package (name foo))
  > EOF
  $ cat > project-version/foo.opam <<EOF
  > opam-version: "2.0"
  > version: "1.0.0"
  > EOF
  $ cat > project-version/dune <<EOF
  > (library (public_name foo))
  > EOF
  $ touch project-version/foo.ml
  $ show_meta_version project-version
  version = "2.0.0"

If neither the package nor the project has a version, ordinary builds don't fall
back to the VCS version for generated package metadata.

  $ mkdir vcs-only
  $ cat > vcs-only/dune-project <<EOF
  > (lang dune 3.24)
  > (name foo)
  > (package (name foo))
  > EOF
  $ cat > vcs-only/dune <<EOF
  > (library (public_name foo))
  > EOF
  $ touch vcs-only/foo.ml
  $ (cd vcs-only && git init -q && git add . && git commit -qm _ && \
  >  git tag -a 9.0.0 -m 9.0.0)
  $ show_meta_version vcs-only
  no version
