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

[(ocamllex foo)] resolves [ocamllex] via [Super_context.resolve_program] in
[parser_generator_rules.ml]. The full-lockdir lookup picks up the fake ocamllex
despite no explicity dependency declaration:

  $ make_dune_project 3.24

  $ cat >foo.mll <<'EOF'
  > rule scan = parse | _ { () }
  > EOF

  $ cat >dune <<'EOF'
  > (ocamllex foo)
  > (rule
  >   (with-stdout-to path-output
  >     (bash "echo $PATH")))
  > EOF

  $ dune build foo.ml 2>/dev/null

Test that the fake ocamllex from tool_provider is used:

  $ grep -c "fake ocamllex" _build/default/foo.ml
  1

The rule depends on the ocamllex binary from the tool_provider lockdir package:

  $ dune rules --format=json foo.ml | jq 'include "dune"; .[] | ruleDepFilePaths' | censor
  "_build/_private/default/.pkg/tool_provider.0.0.1-$DIGEST/target/bin/ocamllex"
  "_build/default/foo.mll"


Similarly, [(menhir ...)] resolves [menhir] from the lockdir package:

  $ rm -rf _build

  $ cat >dune-project <<'EOF'
  > (lang dune 3.21)
  > (using menhir 2.1)
  > EOF

  $ cat >bar.mly <<'EOF'
  > %token EOF
  > %start <unit> main
  > %%
  > main: EOF { () }
  > EOF

  $ cat >dune <<'EOF'
  > (library (name bar))
  > (menhir (modules bar) (infer false))
  > EOF

  $ dune build bar.ml
  $ grep -c "fake menhir" _build/default/bar.ml
  1

The rule depends on the menhir binary from the tool_provider lockdir package:

  $ dune rules --format=json bar.ml | jq 'include "dune"; .[] | ruleDepFilePaths' | censor
  "_build/_private/default/.pkg/tool_provider.0.0.1-$DIGEST/target/bin/menhir"
  "_build/default/bar.mly"
