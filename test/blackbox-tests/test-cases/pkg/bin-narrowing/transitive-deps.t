%{bin:X} and %{bin-available:X} lookups are narrowed to the dependency closure
of the packages declared in the (depends ...) field. All the binaries,
including those of transitive deps, can be resolved. Binaries provided by
packages not in the dependency closure are not resolved.


  $ make_lockdir

A lockdir package [transitive] that installs [transitive-bin]:

  $ make_lockpkg transitive <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "\| cat > transitive-bin <<'EOI'
  >           "\| #!/usr/bin/env bash
  >           "\| echo from transitive
  >           "\| EOI
  >   )
  >   (system "chmod +x transitive-bin")
  >   (system "echo 'bin: [ \"transitive-bin\" ]' > transitive.install")
  >  ))
  > EOF

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

  $ dune build @all

[direct-bin] (direct dep) is available:

  $ cat _build/default/direct-out
  true

[transitive-bin] is available:

  $ cat _build/default/transitive-out
  true

[other-bin] (package not in the deps closure) is not available:

  $ cat _build/default/other-out
  false

Only the transitive dependency closure's packages' bin layouts are added to $PATH:

  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  $PWD/_build/_private/default/.pkg/direct.0.0.1-$DIGEST1/target/bin
  $PWD/_build/_private/default/.pkg/transitive.0.0.1-$DIGEST2/target/bin

Narrowing shrinks the build-dependency set, not just the visibility and PATH
shown above. Previously, expanding a %{bin:X}/%{bin-available:X} pform forced
the install [cookie] of
every lockdir package through [Artifacts_and_deps.of_closure] (pkg_rules.ml).
Now only [direct]'s closure is forced, so building a single pform-expanding
target no longer pulls in [other], a package outside that closure:

  $ dune clean
  $ dune build direct-out
  $ if test -f "$(get_build_pkg_dir other)/target/cookie"
  >  then echo "other *was* built"; else echo "other was not built"; fi
  other was not built

[transitive] (in [direct]'s closure) is built too, as it must be:

  $ if test -f "$(get_build_pkg_dir transitive)/target/cookie"
  >  then echo "transitive *was* built"; else echo "transitive was not built"; fi
  transitive *was* built

Removing this [other] build dependency -- not merely the visibility above -- is
what breaks the in-out cycles: [other] is not built here, while [direct] and
[transitive] (in the closure) still are.
