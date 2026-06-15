When a binary of the same name is provided by both a workspace package and a
lockdir package, resolution of [%{bin:X}] consults [local_bins]. So the
workspace binary shadows the lockdir one.

  $ make_lockdir

A lockdir package [provider] that installs [mybin] printing "from lockdir":

  $ make_lockpkg provider <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "echo '#!/bin/sh' > mybin")
  >   (system "echo 'echo from lockdir' >> mybin")
  >   (system "chmod +x mybin")
  >   (system "echo 'bin: [ \"mybin\" ]' > provider.install")))
  > EOF

A workspace package [mypkg] that installs its own [mybin] printing "from
workspace":

  $ cat >mybin-ws.sh <<'EOF'
  > #!/bin/sh
  > echo from workspace
  > EOF
  $ chmod +x mybin-ws.sh
  $ cat >dune <<'EOF'
  > (install
  >  (package mypkg)
  >  (section bin)
  >  (files (mybin-ws.sh as mybin)))
  > (rule
  >  (with-stdout-to mybin-out (run %{bin:mybin})))
  > (rule
  >  (action (with-stdout-to path-output (bash "echo $PATH"))))
  > EOF

With [(depends provider)] the workspace binary still wins over the declared
lockdir binary of the same name:

  $ make_dune_project 3.25
  $ cat >> dune-project << 'EOF'
  > (package (name mypkg) (allow_empty) (dir .) (depends provider))
  > EOF

  $ dune build mybin-out
  $ cat _build/default/mybin-out
  from workspace

Without depending on [provider] at all, the workspace binary wins just the
same:

  $ make_dune_project 3.25
  $ cat >> dune-project << 'EOF'
  > (package (name mypkg) (allow_empty) (dir .))
  > EOF

  $ dune clean
  $ dune build mybin-out path-output
  $ cat _build/default/mybin-out
  from workspace

The lockdir [provider]'s bin layout is still on $PATH:

  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  $PWD/_build/_private/default/.pkg/provider.0.0.1-$DIGEST/target/bin

When the workspace binary is DISABLED via [(enabled_if false)], its
[local_bins] origin is filtered out and resolution falls through to the lockdir
lookup.

  $ cat >dune <<'EOF'
  > (install
  >  (package mypkg)
  >  (section bin)
  >  (enabled_if false)
  >  (files (mybin-ws.sh as mybin)))
  > (rule
  >  (with-stdout-to mybin-avail (echo %{bin-available:mybin})))
  > (rule
  >  (enabled_if %{bin-available:mybin})
  >  (action (with-stdout-to mybin-out (run %{bin:mybin}))))
  > (rule
  >  (action (with-stdout-to path-output (bash "echo $PATH"))))
  > EOF

Declaring [provider] makes the (now disabled) workspace binary fall through
to the lockdir binary:

  $ make_dune_project 3.25
  $ cat >> dune-project << 'EOF'
  > (package (name mypkg) (allow_empty) (dir .) (depends provider))
  > EOF

  $ dune clean
  $ dune build @all
  $ cat _build/default/mybin-avail
  true
  $ cat _build/default/mybin-out
  from lockdir

The lockdir [provider]'s bin layout is on $PATH:

  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  $PWD/_build/_private/default/.pkg/provider.0.0.1-$DIGEST/target/bin

Without declaring [provider], the lockdir package is narrowed out and the
lookup fails to resolve:

  $ make_dune_project 3.25
  $ cat >> dune-project << 'EOF'
  > (package (name mypkg) (allow_empty) (dir .))
  > EOF

  $ dune clean
  $ dune build @all
  $ cat _build/default/mybin-avail
  false

The lockdir [provider]'s bin layout is on $PATH:

  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  $PWD/_build/_private/default/.pkg/provider.0.0.1-$DIGEST/target/bin
