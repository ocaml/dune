[(depopts ...)] declares an optional dependency: the package may or may not be
present, and [%{bin-available:...}] is how a rule asks whether it is. The two
are meant to be used together.

The narrowing walks the owning package's [(depends ...)] and [(depopts ...)]
fields. Rule guarded by [%{bin-available:...}] should run correctly when the
package providing the binary is listed in either of the fields.

The provider is a Dune project installing [opt-tool], built natively or by an
opaque recipe. Each requester checks the binary and runs the guarded rule.

  $ mkdir -p _tool native opaque
  $ (cd _tool && make_dune_project_with_package 3.25 optional-tool)
  $ echo '(install (section bin) (files opt-tool))' > _tool/dune
  $ printf '#!/bin/sh\necho from optional-tool\n' > _tool/opt-tool
  $ chmod +x _tool/opt-tool
  $ make_backend_lockdirs optional-tool "$PWD/_tool"

  $ cat >dune <<'EOF'
  > (rule
  >  (with-stdout-to avail (echo %{bin-available:opt-tool})))
  > (rule
  >  (enabled_if %{bin-available:opt-tool})
  >  (action (with-stdout-to out (run %{bin:opt-tool}))))
  > EOF
  $ cp dune native/ && cp dune opaque/
  $ check() (
  >   cd "$1"
  >   make_dune_project 3.25
  >   printf '(package (name mypkg) (allow_empty) (dir .) (%s optional-tool))\n' \
  >     "$2" >> dune-project
  >   dune clean
  >   dune build @all
  >   echo "$(cat _build/default/avail)"
  >   cat _build/default/out
  > )

Declared as a hard dependency, the binary is available and the guarded rule
runs:

  $ check native depends
  true
  from optional-tool
  $ check opaque depends
  true
  from optional-tool
  $ cat opaque/$pkg_root/optional-tool/_install/opaque-recipe
  opaque
  $ test ! -e native/$pkg_root/optional-tool/_install/opaque-recipe

Moving the same package to [(depopts ...)], with the lock directory unchanged,
makes no difference:

  $ check native depopts
  true
  from optional-tool
  $ check opaque depopts
  true
  from optional-tool
