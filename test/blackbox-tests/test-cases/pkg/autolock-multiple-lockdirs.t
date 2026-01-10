Test autolocking with multiple named contexts, each using a different lock
directory.

  $ mkrepo

Make a library package:

  $ mkdir foo
  $ cat > foo/dune-project <<EOF
  > (lang dune 3.13)
  > (package (name foo))
  > EOF
  $ cat > foo/dune <<EOF
  > (rule
  >  (with-stdout-to foo.ml
  >   (echo "let context = \"%{context_name}\"")))
  > (library
  >  (public_name foo))
  > EOF
  $ tar cf foo.tar foo
  $ rm -rf foo

  $ mkpkg foo <<EOF
  > build: [
  >   ["dune" "build" "-p" name "@install"]
  > ]
  > url {
  >  src: "$PWD/foo.tar"
  > }
  > EOF

  $ cat > dune-workspace <<EOF
  > (lang dune 3.22)
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock))
  > (lock_dir
  >  (path dune.stable.lock)
  >  (repositories mock))
  > (lock_dir
  >  (path dune.latest.lock)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$PWD/mock-opam-repository"))
  > (context
  >  (default
  >   (name default)
  >   (lock_dir dune.lock)))
  > (context
  >  (default
  >   (name stable)
  >   (lock_dir dune.stable.lock)))
  > (context
  >  (default
  >   (name latest)
  >   (lock_dir dune.latest.lock)))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (package
  >  (name myapp)
  >  (depends foo))
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (with-stdout-to myapp.ml
  >   (echo "let () = Printf.printf \"myapp: %{context_name}, foo: %s\\n\" Foo.context")))
  > (executable
  >  (name myapp)
  >  (libraries foo))
  > EOF

  $ test() {
  >   dune exec --context=default ./myapp.exe
  >   dune exec --context=stable  ./myapp.exe
  >   dune exec --context=latest  ./myapp.exe
  > }

Test partial locking: only lock the stable context:

  $ dune_pkg_lock_normalized dune.stable.lock
  Solution for dune.stable.lock:
  - foo.0.0.1

Only the stable context should work:

  $ dune exec --context=stable ./myapp.exe
  myapp: stable, foo: default

The other contexts should fail since they have no lock files:

  $ dune exec --context=default ./myapp.exe 2>&1 | head -5
  File "dune", line 6, characters 12-15:
  6 |  (libraries foo))
                  ^^^
  Error: Library "foo" not found.
  -> required by _build/default/.myapp.eobjs/native/dune__exe__Myapp.cmx
  [1]

  $ dune exec --context=latest ./myapp.exe 2>&1 | head -5
  File "dune", line 6, characters 12-15:
  6 |  (libraries foo))
                  ^^^
  Error: Library "foo" not found.
  -> required by _build/latest/.myapp.eobjs/native/dune__exe__Myapp.cmx
  [1]

With autolocking enabled, the contexts with missing lock files are fixed:

  $ enable_pkg
  $ test
  myapp: default, foo: default
  myapp: stable, foo: default
  myapp: latest, foo: default

Now lock all contexts explicitly:

  $ dune_pkg_lock_normalized --all
  Solution for dune.latest.lock:
  - foo.0.0.1
  Solution for dune.lock:
  - foo.0.0.1
  Solution for dune.stable.lock:
  - foo.0.0.1

  $ test
  myapp: default, foo: default
  myapp: stable, foo: default
  myapp: latest, foo: default

Now delete all lock directories and fallback to autolocking. 

  $ rm -rf dune.latest.lock dune.lock dune.stable.lock
  $ test
  myapp: default, foo: default
  myapp: stable, foo: default
  myapp: latest, foo: default

