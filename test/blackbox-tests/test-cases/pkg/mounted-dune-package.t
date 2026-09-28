An eligible locked Dune project builds in the current graph. The recorded
opaque recipe must not run, and the fetched inputs must not own the artifact
root.

  $ make_lockdir
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name main) (depends foo))
  > EOF
  $ cat > dune <<'EOF'
  > (executable (name main) (libraries foo))
  > EOF
  $ echo 'let () = print_endline Foo.message' > main.ml
  $ mkdir _foo
  $ cat > _foo/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name foo))
  > EOF
  $ echo '(library (public_name foo))' > _foo/dune
  $ echo 'let message = "native"' > _foo/foo.ml
  $ echo unselected > _foo/unused.txt
  $ make_lockpkg foo <<EOF
  > (version 1.0)
  > (source (copy $PWD/_foo))
  > (build (run false))
  > EOF

  $ dune build main.exe
  $ _build/default/main.exe
  native
  $ test -f _build/default/.lockfile/pkg/foo/.foo.objs/native/foo.cmx
  $ test ! -e _build/default/.lockfile/pkg/foo/unused.txt
  $ test ! -e _build/default/.lockfile/pkg/foo/.opam/foo/target
  $ cat _build/_fetch/url/*/dir/unused.txt
  unselected

Native installation uses the shared materializer, with metadata describing the
lock version rather than inventing an opaque recipe output.

  $ dune build @pkg-install
  $ grep '^version' _build/default/.lockfile/pkg/foo/META.foo
  version = "1.0"
  $ test -L _build/_private/default/.packages/*/lib/foo/META
  $ test ! -e _build/default/.lockfile/pkg/foo/.opam/foo/target

An opaque recipe consumes the native package through the shared layout. Its
installed library retains the native provider when resolving its requirements.

  $ make_lockpkg opaque <<'EOF'
  > (version 1.0)
  > (depends foo)
  > (build
  >  (progn
  >   (write-file opaque.ml "let message = Foo.message")
  >   (run ocamlc -I %{pkg:foo:lib} -a -o opaque.cma opaque.ml)
  >   (run ocamlopt -I %{pkg:foo:lib} -a -o opaque.cmxa opaque.ml)
  >   (write-file META "requires = \"foo\"\narchive(byte) = \"opaque.cma\"\narchive(native) = \"opaque.cmxa\"\n")
  >   (write-file opaque.install "lib: [ \"META\" \"opaque.cmi\" \"opaque.cma\" \"opaque.cmx\" \"opaque.cmxa\" \"opaque.a\" ]")))
  > EOF
  $ build_pkg opaque
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name main) (depends opaque))
  > EOF
  $ echo '(executable (name main) (libraries opaque))' > dune
  $ echo 'let () = print_endline Opaque.message' > main.ml
  $ dune build main.exe
  $ _build/default/main.exe
  native

A mounted project's tool lookup is restricted to its locked dependencies even
when its authored package has no exclusive directory. A negative lookup must
not force either unrelated recipes or the package's own installation.

  $ make_lockpkg unrelated <<'EOF'
  > (version 1.0)
  > (build (run false))
  > EOF
  $ cat >> _foo/dune <<'EOF'
  > (rule
  >  (target probe.ml)
  >  (action (write-file %{target} "let visible = %{bin-available:unrelated-tool}")))
  > EOF
  $ echo 'let message = "native/" ^ string_of_bool Probe.visible' > _foo/foo.ml
  $ dune build _build/default/.lockfile/pkg/foo/foo.cma
  $ cat _build/default/.lockfile/pkg/foo/probe.ml
  let visible = false
  $ test ! -e _build/default/.lockfile/pkg/unrelated/.opam/unrelated/target

A second native project can consume that opaque library without searching an
unrelated provider. Its actions use the mounted project root.

  $ mkdir _bar
  $ cat > _bar/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name bar))
  > EOF
  $ cat > _bar/dune <<'EOF'
  > (library (public_name bar) (libraries opaque))
  > (rule
  >  (target generated.ml)
  >  (action
  >   (with-stdout-to %{target}
  >    (run sh -c "test \"$DUNE_PROJECT_ROOT\" = \"$PWD\" && echo 'let suffix = \"/bar\"'"))))
  > EOF
  $ echo 'let message = Opaque.message ^ Generated.suffix' > _bar/bar.ml
  $ make_lockpkg bar <<EOF
  > (version 1.0)
  > (depends opaque)
  > (source (copy $PWD/_bar))
  > (build (run false))
  > EOF
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name main) (dir .) (depends bar))
  > EOF
  $ echo '(executable (name main) (libraries bar))' > dune
  $ echo 'let () = print_endline Bar.message' > main.ml
  $ dune build main.exe
  $ _build/default/main.exe
  native/false/bar
  $ test ! -e _build/default/.lockfile/pkg/unrelated/.opam/unrelated/target

An unused mounted package with a malformed project must not prevent building
another package. Requesting the broken package should report its own error,
without falling back to its otherwise successful recipe.

  $ mkdir _broken
  $ printf '(lang dune 3.25)\n(broken-project)\n' > _broken/dune-project
  $ echo '(library (public_name broken))' > _broken/dune
  $ make_lockpkg broken <<EOF
  > (version 1.0)
  > (source (copy $PWD/_broken))
  > (build (echo opaque-fallback-must-not-run))
  > EOF

BUG: preparing every candidate eagerly reports the unrelated project's error.

  $ dune build main.exe
  File "_build/default/.lockfile/pkg/broken/dune-project", line 2, characters 1-15:
  Error: Unknown field "broken-project"
  [1]
  $ dune build _build/default/.lockfile/pkg/broken/broken.cma
  File "_build/default/.lockfile/pkg/broken/dune-project", line 2, characters 1-15:
  Error: Unknown field "broken-project"
  [1]
