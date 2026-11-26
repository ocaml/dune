%{bin:X} and %{bin-available:X} lookups are narrowed to the packages declared
in the (depends ...) and (depopts ...) fields and their dependencies. Binaries
provided by packages outside that closure are not resolved.

The leaf [transitive] is a Dune project installing [transitive-bin], built
natively or by an opaque recipe:

  $ mkdir -p _transitive native opaque
  $ (cd _transitive && make_dune_project_with_package 3.25 transitive)
  $ echo '(install (section bin) (files transitive-bin))' > _transitive/dune
  $ printf '#!/bin/sh\necho from transitive\n' > _transitive/transitive-bin
  $ chmod +x _transitive/transitive-bin
  $ make_backend_lockdirs transitive "$PWD/_transitive"

The rest of the setup is shared by both lock directories.

  $ cd native

A lockdir package [direct] that depends on [transitive] and installs [direct-bin]:

  $ make_lockpkg direct <<'EOF'
  > (version 0.0.1)
  > (depends transitive)
  > (build
  >  (progn
  >   (system "\| cat > direct-bin <<'EOI'
  >           "\| #!/usr/bin/env bash
  >           "\| echo from direct
  >           "\| EOI
  >   )
  >   (system "chmod +x direct-bin")
  >   (system "echo 'bin: [ \"direct-bin\" ]' > direct.install")
  >  ))
  > EOF

A sibling lockdir package [other] that installs [other-bin], not depended on
by [direct]:

  $ make_lockpkg other <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "\| cat > other-bin <<'EOI'
  >           "\| #!/usr/bin/env bash
  >           "\| echo from other
  >           "\| EOI
  >   )
  >   (system "chmod +x other-bin")
  >   (system "echo 'bin: [ \"other-bin\" ]' > other.install")
  >  ))
  > EOF

The project's package depends only on [direct] (with a [dir] field so
narrowing kicks in):

  $ make_dune_project 3.25
  $ cat >> dune-project << 'EOF'
  > (package
  >   (allow_empty)
  >   (name my-pkg)
  >   (dir .)
  >   (depends direct))
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (action
  >    (with-stdout-to path-output (bash "echo $PATH"))))
  > (rule
  >  (with-stdout-to direct-out (echo %{bin-available:direct-bin})))
  > (rule
  >  (with-stdout-to transitive-out (echo %{bin-available:transitive-bin})))
  > (rule
  >  (with-stdout-to other-out (echo %{bin-available:other-bin})))
  > EOF

  $ cd ..
  $ cp native/dune.lock/direct.pkg native/dune.lock/other.pkg opaque/dune.lock/
  $ cp native/dune native/dune-project opaque/

Each check prints [direct-bin] (direct dep), [transitive-bin] (through
[direct]) and [other-bin] (package not in the deps closure):

  $ check() (
  >   cd "$1"
  >   dune build @all
  >   for out in direct transitive other; do
  >     echo "$(cat _build/default/$out-out)"
  >   done
  > )

  $ check native
  true
  true
  false
  $ check opaque
  true
  true
  false
  $ cat opaque/$pkg_root/transitive/_install/opaque-recipe
  opaque
  $ test ! -e native/$pkg_root/transitive/_install/opaque-recipe

The package [other] was *not* built. A native [transitive] runs no recipe:

  $ (cd native && dune trace cat | jq 'select(.cat == "process" and .name == "finish" and .args.target_dirs[]?) | .args.target_dirs[]' | sort -u)
  "_build/default/.pkg/direct/_install"
  $ (cd opaque && dune trace cat | jq 'select(.cat == "process" and .name == "finish" and .args.target_dirs[]?) | .args.target_dirs[]' | sort -u)
  "_build/default/.pkg/direct/_install"
  "_build/default/.pkg/transitive/_install"

The declared dependencies are not added to $PATH:

  $ cd opaque
  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  
  [1]
  $ cd ..

Narrowing shrinks the build-dependency set, not just the visibility and PATH
shown above. Previously, expanding a %{bin:X}/%{bin-available:X} pform forced
the install [cookie] of
every lockdir package through [Artifacts_and_deps.of_closure] (pkg_rules.ml).
Now only [direct]'s closure is forced, so building a single pform-expanding
target no longer pulls in [other], a package outside that closure:

  $ cold() (
  >   cd "$1"
  >   dune clean
  >   dune build direct-out
  >   if test -f "$(get_build_pkg_dir other)/_install/cookie"
  >    then echo "other *was* built"; else echo "other was not built"; fi
  > )
  $ cold native
  other was not built
  $ cold opaque
  other was not built

[transitive] (in [direct]'s closure) is built too, as it must be:

  $ if test -f "opaque/$(get_build_pkg_dir transitive)/_install/cookie"
  >  then echo "transitive *was* built"; else echo "transitive was not built"; fi
  transitive *was* built
  $ ls native/_build/_private/default/.packages/*/bin
  transitive-bin

Removing this [other] build dependency -- not merely the visibility above -- is
what breaks the in-out cycles: [other] is not built here, while [direct] and
[transitive] (in the closure) still are.
