A lookup made while generating a workspace package's rules must not build a
locked package that depends on that package. Another workspace package that
declares the locked package still sees it.

  $ make_dune_project 3.24
  $ cat >> dune-project <<EOF
  > (package (name myws) (dir src) (depends (mylock :with-test)))
  > (package (name consumer) (allow_empty) (dir consumer) (depends mylock))
  > EOF

The workspace library has an optional dependency that nothing provides, so
resolving it is a miss:

  $ mkdir src consumer
  $ cat > src/dune <<EOF
  > (library
  >  (public_name myws)
  >  (libraries
  >   (select impl.ml from
  >    (nowhere -> impl.with.ml)
  >    (-> impl.without.ml))))
  > (rule
  >  (alias tool)
  >  (action (echo "%{bin-available:mybin}\n")))
  > EOF
  $ echo 'let x = "with"' > src/impl.with.ml
  $ echo 'let x = "without"' > src/impl.without.ml

An opaque locked package depends on the workspace package and installs a
binary:

  $ make_lockdir
  $ make_lockpkg mylock <<'EOF'
  > (version 0.0.1)
  > (depends myws)
  > (build
  >  (progn
  >   (system "printf '#!/bin/sh\necho from mylock\n' > mybin")
  >   (system "chmod +x mybin")
  >   (system "echo 'bin: [ \"mybin\" ]' > mylock.install")))
  > EOF

The library miss does not build the locked package:

  $ dune build @src/all 2>&1 | head -1
  $ tail -1 _build/default/src/impl.ml
  let x = "without"

Neither does a program lookup in the package's own directory, although the
package lists the locked package as a test dependency:

  $ dune build @src/tool 2>&1 | head -1
  false

A workspace package that declares the locked package runs it:

  $ cat > consumer/dune <<EOF
  > (rule
  >  (alias mine)
  >  (action (run mybin)))
  > EOF
  $ dune build @consumer/mine 2>&1 | head -1
  from mylock
