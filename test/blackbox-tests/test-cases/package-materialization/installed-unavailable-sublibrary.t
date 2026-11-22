An unusable library advertised by an installed package must not suppress its
independent usable roots or their required libraries.

The usable root a requires b. Its sibling a.optional requires unavailable,
which will be removed after installation. Keeping a -> b is important: merely
building a could otherwise pass even if its entire support closure were lost.

  $ setup_package_a_requiring_b
  $ mkdir source/optional source/unavailable prefix probe-dune probe-meta
  $ echo '(package (name unavailable))' >>source/dune-project
  $ echo '(library (public_name b) (modes byte))' >source/b/dune
  $ echo 'let value = 1' >source/b/b.ml
  $ cat >source/optional/dune <<'EOF'
  > (library
  >  (name optional)
  >  (public_name a.optional)
  >  (modes byte)
  >  (libraries unavailable))
  > EOF
  $ echo 'let value = Unavailable.value' >source/optional/optional.ml
  $ cat >source/unavailable/dune <<'EOF'
  > (library (public_name unavailable) (modes byte))
  > EOF
  $ echo 'let value = 3' >source/unavailable/unavailable.ml
  $ dune build --root source @install
  $ dune install --root source --prefix "$PWD/prefix" 2>/dev/null
  $ export OCAMLPATH="$PWD/prefix/lib"
  $ b_lib="$PWD/prefix/lib/b"
  $ test -f prefix/lib/a/dune-package
  $ test -f prefix/lib/a/optional/optional.cmi
  $ test -f prefix/lib/a/optional/optional.cma
  $ test -f "$b_lib/dune-package"
  $ test -f "$b_lib/b.cmi"
  $ test -f "$b_lib/b.cma"

Before removing its dependency, a.optional is advertised by META and usable
through Dune's installed-library resolver. The probe is a separate project,
so its later error cannot interfere with the usable consumer.

  $ ocamlfind query -recursive -format %p a.optional | sort
  a.optional
  unavailable
  $ echo '(lang dune 3.24)' >probe-dune/dune-project
  $ cat >probe-dune/dune <<'EOF'
  > (executable
  >  (name main)
  >  (modes byte)
  >  (libraries a.optional))
  > EOF
  $ echo 'let () = Printf.printf "%d\n" Optional.value' >probe-dune/main.ml
  $ cp probe-dune/dune probe-dune/dune-project probe-dune/main.ml probe-meta/
  $ dune build --root probe-dune main.bc
  $ probe-dune/_build/default/main.bc
  3
  $ test -f prefix/lib/unavailable/dune-package
  $ rm -r prefix/lib/unavailable

Dune still finds a.optional but can no longer resolve its requirement. Check
the error explicitly rather than treating an arbitrary failed build as proof.

  $ dune build --root probe-dune main.bc >probe-dune.log 2>&1
  [1]
  $ grep -F 'Error: Library "unavailable" not found.' probe-dune.log
  Error: Library "unavailable" not found.
  $ grep -Fq 'required by library "a.optional"' probe-dune.log

The consumer requests package a, but uses only its independent usable root.

  $ setup_package_a_consumers
  $ dune build --root dune-consumer main.exe
  $ dune-consumer/_build/default/main.exe
  2

The usable root's required package b is tracked. An unusable sibling does
not prevent following the available dependencies.

  $ dune rules --root dune-consumer --format=json main.exe >dune-rules.json
  $ jq_dune --arg b "$b_lib" --arg metadata dune-package '
  >   rulesMatchingTarget("main.exe")
  >   | rulePackageDependencyReport($b; "b"; $metadata)
  > ' dune-rules.json
  {
    "reader": "dune-package",
    "required_interface": true,
    "required_archive": true,
    "required_metadata": true
  }

Repeat the missing-library diagnostic and successful usable-root build through
META. The artifact-dependency checks remain false: META-only packages do not
yet register these installed files as dependencies.

  $ rm prefix/lib/a/dune-package "$b_lib/dune-package"
  $ test -f prefix/lib/a/META
  $ test -f "$b_lib/META"
  $ dune build --root probe-meta main.bc >probe-meta.log 2>&1
  [1]
  $ grep -F 'Error: Library "unavailable" not found.' probe-meta.log
  Error: Library "unavailable" not found.
  $ grep -Fq 'required by library "a.optional"' probe-meta.log
  $ dune build --root meta-consumer main.exe
  $ meta-consumer/_build/default/main.exe
  2
  $ dune rules --root meta-consumer --format=json main.exe >meta-rules.json
  $ jq_dune --arg b "$b_lib" --arg metadata META '
  >   rulesMatchingTarget("main.exe")
  >   | rulePackageDependencyReport($b; "b"; $metadata)
  > ' meta-rules.json
  {
    "reader": "META",
    "required_interface": false,
    "required_archive": false,
    "required_metadata": false
  }
