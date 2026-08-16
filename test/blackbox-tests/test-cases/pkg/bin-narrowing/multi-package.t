Per-package narrowing with multiple owning packages: each workspace package's
stanzas resolve only the lockdir binaries of the packages it depends on.

  $ make_lockdir

Two independent lockdir packages, each installing a distinct binary:

  $ make_lockpkg tool-a <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "echo '#!/bin/sh' > bin-a")
  >   (system "echo 'echo from-a' >> bin-a")
  >   (system "chmod +x bin-a")
  >   (system "echo 'bin: [ \"bin-a\" ]' > tool-a.install")))
  > EOF

  $ make_lockpkg tool-b <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "echo '#!/bin/sh' > bin-b")
  >   (system "echo 'echo from-b' >> bin-b")
  >   (system "chmod +x bin-b")
  >   (system "echo 'bin: [ \"bin-b\" ]' > tool-b.install")))
  > EOF

Two workspace packages, each owning a subdirectory and depending on a
different lockdir tool:

  $ mkdir -p a b
  $ cat >a/dune <<'EOF'
  > (rule (with-stdout-to a-sees-a (echo %{bin-available:bin-a})))
  > (rule (with-stdout-to a-sees-b (echo %{bin-available:bin-b})))
  > (rule (action (with-stdout-to a-path (bash "echo $PATH"))))
  > EOF
  $ cat >b/dune <<'EOF'
  > (rule (with-stdout-to b-sees-a (echo %{bin-available:bin-a})))
  > (rule (with-stdout-to b-sees-b (echo %{bin-available:bin-b})))
  > (rule (action (with-stdout-to b-path (bash "echo $PATH"))))
  > EOF

  $ make_dune_project 3.25
  $ cat >> dune-project << 'EOF'
  > (package (name pkg-a) (allow_empty) (dir a) (depends tool-a))
  > (package (name pkg-b) (allow_empty) (dir b) (depends tool-b))
  > EOF

  $ dune build @all

pkg-a (depends tool-a) sees bin-a, but not bin-b:

  $ cat _build/default/a/a-sees-a
  true
  $ cat _build/default/a/a-sees-b
  false

pkg-b (depends tool-b) sees bin-b, but not bin-a:

  $ cat _build/default/b/b-sees-b
  true
  $ cat _build/default/b/b-sees-a
  false

Only the dependency closure's packages' bin layouts are on $PATH:

  $ env_added "$(cat _build/default/a/a-path)" "$PATH" | censor
  $PWD/_build/_private/default/.pkg/tool-a.0.0.1-$DIGEST/target/bin
  $ env_added "$(cat _build/default/b/b-path)" "$PATH" | censor
  $PWD/_build/_private/default/.pkg/tool-b.0.0.1-$DIGEST/target/bin

The narrowing is ultimately about the build-dependency set, not just the
visibility and PATH shown above. Previously, a lookup forced every lockdir
package's cookie. With the current narrowing, only the packages in the
transitive dependency closure of [pkg-a] get built:

  $ dune clean
  $ dune build a/a-sees-a
  $ if test -f "$(get_build_pkg_dir tool-b)/target/cookie"
  >  then echo "tool-b *was* built"; else echo "tool-b was not built"; fi
  tool-b was not built

The load-bearing pair is [a-sees-b] false alongside [b-sees-b] true -- the same
lockdir package [tool-b] is hidden from [pkg-a] yet visible to [pkg-b], so the
narrowing set is keyed on each dir's owning package, not a single global set.
