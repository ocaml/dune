When a lockdir binary's package isn't a declared dependency, narrowing removes
the lockdir copy from [%{bin:X}] resolution, and [Context.which] for a Lock
context falls back to [Which.which ~path:builder.path] -- the ambient PATH. So
[%{bin:X}] resolves to the ambient copy, not the lockdir one (before narrowing
it resolved to the lockdir copy). [(system X)] goes through the shell's own
$PATH lookup and also finds the ambient copy here.

  $ make_lockdir

A lockdir package [provider] installs [mybin] ("from lockdir"), which we will
NOT declare as a dependency:

  $ make_lockpkg provider <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "echo '#!/bin/sh' > mybin")
  >   (system "echo 'echo from lockdir' >> mybin")
  >   (system "chmod +x mybin")
  >   (system "echo 'bin: [ \"mybin\" ]' > provider.install")))
  > EOF

A [mybin] on the ambient PATH ("from system"):

  $ mkdir fakebin
  $ cat >fakebin/mybin <<'EOF'
  > #!/bin/sh
  > echo from system
  > EOF
  $ chmod +x fakebin/mybin

  $ cat >dune <<'EOF'
  > (rule
  >  (with-stdout-to mybin-avail (echo %{bin-available:mybin})))
  > (rule
  >  (enabled_if %{bin-available:mybin})
  >  (action (with-stdout-to mybin-out (run %{bin:mybin}))))
  > (rule
  >  (enabled_if %{bin-available:mybin})
  >  (action (with-stdout-to mybin-system (system mybin))))
  > (rule
  >  (action (with-stdout-to path-output (bash "echo $PATH"))))
  > EOF

  $ make_dune_project 3.25
  $ cat >> dune-project << 'EOF'
  > (package (name mypkg) (allow_empty) (dir .))
  > EOF

[mybin] is not a declared dep, and the lockdir package is filtered out. So, the
lookup resolves to the ambient PATH copy:

  $ PATH="$PWD/fakebin:$PATH" dune build @all
  $ cat _build/default/mybin-avail
  true
  $ cat _build/default/mybin-out
  from system

[(system mybin)] resolves via $PATH. The lockdir provider's bin layout is on
$PATH, but the shell finds the system binary.

  $ cat _build/default/mybin-system
  from system

  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  $PWD/_build/_private/default/.pkg/provider.0.0.1-$DIGEST/target/bin
  $PWD/fakebin
