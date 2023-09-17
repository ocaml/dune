This test is about the semantics of `(public_name -)`.

  $ set_ver() {
  > cat > dune-project << EOF
  > (lang dune $1)
  > (package
  >  (name e)
  >  (allow_empty))
  > EOF
  > }
  $ cat > dune << EOF
  > (executable
  >  (public_name -)
  >  (name e))
  > EOF
  $ touch e.ml

In 3.7, this declares a public name of "-":

  $ set_ver 3.7
  $ dune build @install
  $ cat _build/default/e.install
  lib: [
    "_build/install/default/lib/e/META"
    "_build/install/default/lib/e/dune-package"
  ]
  bin: [
    "_build/install/default/bin/-"
  ]

In 3.8, this is equivalent to no `(public_name)` field, consistently with what
happens with `(executables)`.

  $ set_ver 3.8
  $ dune build @install
  $ cat _build/default/e.install
  lib: [
    "_build/install/default/lib/e/META"
    "_build/install/default/lib/e/dune-package"
  ]
