Package dependencies and executable dependencies can overlap in a sandbox.
Cross native/opaque providers with/without [(deps (package provider))],
default/copy sandboxing, and literal/pform program lookup. The project package
always depends on the provider; all cells share their source definitions.

  $ export DUNE_CACHE=disabled
  $ mkdir _provider native
  $ (cd _provider && make_dune_project_with_package 3.25 provider)
  $ echo '(install (section bin) (files mybin))' > _provider/dune
  $ cat > _provider/mybin <<'EOF'
  > #!/bin/sh
  > printf 'from provider: %s\n' "$1"
  > EOF
  $ chmod +x _provider/mybin
  $ cat > native/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name main) (dir .) (allow_empty) (depends provider))
  > EOF
  $ rule() {
  >   echo "(rule (target $1) (deps $2 (sandbox always))"
  >   echo " (action (with-stdout-to %{target} (run $3))))"
  > }
  $ { rule with-dep-literal '(package provider)' 'mybin literal'
  >   rule with-dep-pform '(package provider)' '%{bin:mybin} pform'
  >   rule without-dep-literal '' 'mybin literal'
  >   rule without-dep-pform '' '%{bin:mybin} pform'
  > } > native/dune

Both user rules require sandboxing. Prefer none for internal rules, then copy
for the user rules; this makes copy mode effective even without a package dep.

  $ cat > native/copy-config <<'EOF'
  > (lang dune 3.25)
  > (sandboxing_preference none copy)
  > EOF
  $ cp -R native opaque
  $ make_backend_lockdirs provider "$PWD/_provider"

Native provider, default sandboxing, with and without a package dependency.
Literal and pform lookups have separate actions, so neither supplies the
other's sandbox dependencies.

  $ (cd native && dune build @all)
  $ cat native/_build/default/*-dep-*
  from provider: literal
  from provider: pform
  from provider: literal
  from provider: pform

Native provider, copy sandboxing. Remove the outputs to rerun every action.

  $ rm native/_build/default/*-dep-*
  $ (cd native && dune build @all --config-file copy-config)
  $ cat native/_build/default/*-dep-*
  from provider: literal
  from provider: pform
  from provider: literal
  from provider: pform
  $ test ! -e native/_build/default/.pkg/provider/_install/cookie

Opaque provider, default sandboxing, with the same consumers and lookups.

  $ (cd opaque && dune build @all)
  $ cat opaque/_build/default/*-dep-*
  from provider: literal
  from provider: pform
  from provider: literal
  from provider: pform

Opaque provider, copy sandboxing. The package directory and the executable
inside it must not be copied twice when both dependencies are present.

  $ rm opaque/_build/default/*-dep-*
  $ (cd opaque && dune build @all --config-file copy-config)
  $ cat opaque/_build/default/*-dep-*
  from provider: literal
  from provider: pform
  from provider: literal
  from provider: pform
  $ cat opaque/_build/default/.pkg/provider/_install/opaque-recipe
  opaque
  $ test -f opaque/_build/default/.pkg/provider/_install/cookie
