Test the toolchain field in the workspace.

  $ unset OCAMLFIND_TOOLCHAIN
  $ unset OCAMLFIND_CONF

First, we create the project
  $ cat >dune <<EOF
  > (library
  >  (public_name foo))
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.10)
  > (package
  >  (name foo))
  > EOF

  $ buildfoo() {
  >   dune clean
  >   dune build @install -x foobar
  >   [ -e _build/default.foobar/foo.cma ] && echo built foo.cma
  > }

Now the toolchain:

Now we create the toolchain and make sure we actually use it:

  $ actualocamlc="$(command -v ocamlc)"

  $ mkdir -p etc/findlib.conf.d
  $ cat >etc/findlib.conf <<EOF
  > path=""
  > ocamlc="$PWD/notocamlc"
  > EOF
  $ cat >etc/findlib.conf.d/foo.conf <<EOF
  > path(foobar)=""
  > ocamlc(foobar)="$PWD/notocamlc-foobar"
  > EOF

  $ cat >notocamlc <<EOF
  > #!/usr/bin/env sh
  > echo "from notocamlc" >&2
  > $actualocamlc \$@
  > EOF
  $ cat >notocamlc-foobar <<EOF
  > #!/usr/bin/env sh
  > echo "from notocamlc-foobar" >&2
  > $actualocamlc \$@
  > EOF
  $ chmod +x notocamlc notocamlc-foobar

We set an undefined toolchain with findlib:

  $ OCAMLFIND_TOOLCHAIN=foobar buildfoo
  ocamlfind: [WARNING] Undefined toolchain: foobar
  ocamlfind: [WARNING] Undefined toolchain: foobar
  built foo.cma
  $ OCAMLFIND_CONF=$PWD/etc/findlib.conf OCAMLFIND_TOOLCHAIN=foobar buildfoo
  from notocamlc-foobar
  from notocamlc-foobar
  from notocamlc-foobar
  from notocamlc-foobar
  built foo.cma

  $ unset OCAMLFIND_TOOLCHAIN
  $ unset OCAMLFIND_CONF

Now we set it in the workspace:

  $ cat >dune-workspace <<EOF
  > (lang dune 1.10)
  > (context
  >  (default
  >   (toolchain foobar)))
  > EOF

  $ buildfoo
  built foo.cma

  $ OCAMLFIND_CONF=$PWD/etc/findlib.conf buildfoo
  from notocamlc-foobar
  from notocamlc-foobar
  from notocamlc-foobar
  from notocamlc-foobar
  from notocamlc-foobar
  from notocamlc-foobar
  built foo.cma
