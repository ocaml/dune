Create a workspace with multiple contexts, each specifying a lockdir name.
  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (context
  >  (default
  >   (lock foo.lock)))
  > (context
  >  (default
  >   (name foo)
  >   (lock bar.lock)))
  > (context
  >  (opam
  >   (name bar)
  >   (switch default)))
  > EOF

Generate a `dune-project` file listing some dependencies.
  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name lockfile_generation_test)
  >  (depends
  >    foo
  >    (bar (>= "0.3"))
  >   ))
  > EOF

Test that we get an error when --context and --all-contexts are passed at the same time.
  $ dune pkg lock --opam-env=pure --opam-repository=mock-opam-repository --all-contexts --context=foo
  Error: --context and --all-contexts are mutually exclusive
  [1]

Test that we get an error if a non-existant context is specified.
  $ dune pkg lock --opam-env=pure --opam-repository=mock-opam-repository --context=baz
  Error: Unknown build context: baz
  [1]

Test that we get an error if an opam context is specified.
  $ dune pkg lock --opam-env=pure --opam-repository=mock-opam-repository --context=bar
  Error: Unexpected opam build context: bar
  [1]

Generate the lockdir for the default context.
  $ dune pkg lock --opam-env=pure --opam-repository=mock-opam-repository
  Selected the following packages:
  bar.0.5.0
  baz.0.1.0
  foo.0.0.1

Only foo.lock (the default context's lockdir) was generated.
  $ find *.lock | sort
  foo.lock
  foo.lock/bar.pkg
  foo.lock/baz.pkg
  foo.lock/foo.pkg
  foo.lock/lock.dune
  $ rm -rf *.lock

Generate the lockdir with the default context explicitly specified.
  $ dune pkg lock --opam-env=pure --opam-repository=mock-opam-repository --context=default
  Selected the following packages:
  bar.0.5.0
  baz.0.1.0
  foo.0.0.1

Again, only foo.lock (the default context's lockdir) was generated.
  $ find *.lock | sort
  foo.lock
  foo.lock/bar.pkg
  foo.lock/baz.pkg
  foo.lock/foo.pkg
  foo.lock/lock.dune
  $ rm -rf *.lock

Generate the lockdir for the non-default context.
  $ dune pkg lock --opam-env=pure --opam-repository=mock-opam-repository --context=foo
  Selected the following packages:
  bar.0.5.0
  baz.0.1.0
  foo.0.0.1

Now only bar.lock was generated.
  $ find *.lock | sort
  bar.lock
  bar.lock/bar.pkg
  bar.lock/baz.pkg
  bar.lock/foo.pkg
  bar.lock/lock.dune
  $ rm -rf *.lock

Generate the lockdir for all (non-opam) contexts.
  $ dune pkg lock --opam-env=pure --opam-repository=mock-opam-repository --all-contexts
  Selected the following packages:
  bar.0.5.0
  baz.0.1.0
  foo.0.0.1

Now both lockdirs were generated.
  $ find *.lock | sort
  bar.lock
  bar.lock/bar.pkg
  bar.lock/baz.pkg
  bar.lock/foo.pkg
  bar.lock/lock.dune
  foo.lock
  foo.lock/bar.pkg
  foo.lock/baz.pkg
  foo.lock/foo.pkg
  foo.lock/lock.dune
  $ rm -rf *.lock
