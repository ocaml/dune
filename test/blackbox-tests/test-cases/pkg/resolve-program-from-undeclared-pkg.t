Several stanzas resolve their tool binaries via [Super_context.resolve_program]
-> [Artifacts.binary]: [(ocamllex ...)] / [(ocamlyacc ...)] in
[parser_generator_rules.ml], [(menhir ...)] in [menhir/menhir_rules.ml],
[(cinaps ...)], [(mdx ...)], etc.

[Artifacts.binary]'s default [Context.which] step falls through to
[Pkg_rules.which], which currently scans every lockdir package's binary index.
So a binary installed by any locked package is discoverable from any stanza.

  $ make_lockdir

A lockdir package [tool_provider] that installs fake [ocamllex] and
fake [menhir] binaries. Each writes a distinctive marker into its
output so we can verify which binary actually ran:

  $ make_lockpkg tool_provider <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "\| cat > ocamllex <<'EOI'
  >           "\| #!/usr/bin/env bash
  >           "\| out=
  >           "\| while [ $# -gt 0 ]; do
  >           "\|   case "$1" in
  >           "\|     -o) out=$2; shift 2 ;;
  >           "\|     -q) shift ;;
  >           "\|     *) shift ;;
  >           "\|   esac
  >           "\| done
  >           "\| echo "(* fake ocamllex from tool_provider *)" > "$out"
  >           "\| EOI
  >   )
  >   (system "\| cat > menhir <<'EOI'
  >           "\| #!/usr/bin/env bash
  >           "\| base=
  >           "\| while [ $# -gt 0 ]; do
  >           "\|   case "$1" in
  >           "\|     --base) base=$2; shift 2 ;;
  >           "\|     *) shift ;;
  >           "\|   esac
  >           "\| done
  >           "\| [ -z "$base" ] && exit 1
  >           "\| echo "(* fake menhir from tool_provider *)" > "$base.ml"
  >           "\| echo "(* fake menhir from tool_provider *)" > "$base.mli"
  >           "\| EOI
  >   )
  >   (system "chmod +x ocamllex menhir")
  >   (system "echo 'bin: [ \"ocamllex\" \"menhir\" ]' > tool_provider.install")
  >  )
  > )
  > EOF

[(ocamllex ...)] and [(menhir ...)] resolve their tools via
[Super_context.resolve_program]:

  $ cat >foo.mll <<'EOF'
  > rule scan = parse | _ { () }
  > EOF

  $ cat >bar.mly <<'EOF'
  > %token EOF
  > %start <unit> main
  > %%
  > main: EOF { () }
  > EOF

  $ cat >dune <<'EOF'
  > (ocamllex foo)
  > (library (name bar))
  > (menhir (modules bar) (infer false))
  > (rule
  >   (with-stdout-to path-output
  >     (bash "echo $PATH")))
  > EOF

With the current full-lockdir lookup, the fake ocamllex and fake menhir are
picked up despite no explicit dependency declaration:

  $ make_dune_project 3.24
  $ cat >> dune-project << 'EOF'
  > (using menhir 2.1)
  > EOF
  $ dune build foo.ml bar.ml path-output 2>/dev/null

  $ grep -c "fake ocamllex" _build/default/foo.ml
  1
  $ grep -c "fake menhir" _build/default/bar.ml
  1

The rules depend on the ocamllex and menhir binaries from the tool_provider
lockdir package:

  $ dune rules --format=json foo.ml | jq_dune '.[] | ruleDepFilePaths' | censor
  "_build/_private/default/.pkg/tool_provider.0.0.1-$DIGEST/target/bin/ocamllex"
  "_build/default/foo.mll"
  $ dune rules --format=json bar.ml | jq_dune '.[] | ruleDepFilePaths' | censor
  "_build/_private/default/.pkg/tool_provider.0.0.1-$DIGEST/target/bin/menhir"
  "_build/default/bar.mly"

The tool_provider bin layout is added to $PATH:

  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  $PWD/_build/_private/default/.pkg/tool_provider.0.0.1-$DIGEST/target/bin

With a package defined in the project, *without a dir field*, the behavior is the
same.

  $ make_dune_project 3.24
  $ cat >> dune-project << 'EOF'
  > (using menhir 2.1)
  > (package
  >   (allow_empty)
  >   (name my-pkg))
  > EOF

  $ dune clean
  $ dune build foo.ml bar.ml path-output 2>/dev/null

  $ grep -c "fake ocamllex" _build/default/foo.ml
  1
  $ grep -c "fake menhir" _build/default/bar.ml
  1

  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  $PWD/_build/_private/default/.pkg/tool_provider.0.0.1-$DIGEST/target/bin

With a package defined in the project, *with a dir field, but no dependencies*,
the behavior is still the same.

  $ make_dune_project 3.24
  $ cat >> dune-project << 'EOF'
  > (using menhir 2.1)
  > (package
  >   (allow_empty)
  >   (name my-pkg)
  >   (dir .))
  > EOF

  $ dune clean
  $ dune build foo.ml bar.ml path-output 2>/dev/null

  $ grep -c "fake ocamllex" _build/default/foo.ml
  1
  $ grep -c "fake menhir" _build/default/bar.ml
  1

  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  $PWD/_build/_private/default/.pkg/tool_provider.0.0.1-$DIGEST/target/bin

With a package defined in the project, *with a dir field, and explicit depends on
[tool_provider]*, the behavior remains the same.

  $ make_dune_project 3.24
  $ cat >> dune-project << 'EOF'
  > (using menhir 2.1)
  > (package
  >   (allow_empty)
  >   (name my-pkg)
  >   (dir .)
  >   (depends tool_provider))
  > EOF

  $ dune clean
  $ dune build foo.ml bar.ml path-output 2>/dev/null

  $ grep -c "fake ocamllex" _build/default/foo.ml
  1
  $ grep -c "fake menhir" _build/default/bar.ml
  1

  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  $PWD/_build/_private/default/.pkg/tool_provider.0.0.1-$DIGEST/target/bin
