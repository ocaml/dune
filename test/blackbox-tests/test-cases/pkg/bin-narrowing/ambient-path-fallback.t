Currently nothing is narrowed, so a lockdir binary shadows an identically-named
binary on the ambient PATH -- [%{bin:X}] and [(system X)] both resolve to the
lockdir copy, not the ambient one. Once narrowing hides the lockdir copy (its
package isn't a declared dep), [Context.which] for a Lock context falls back to
[Which.which ~path:builder.path] (the ambient/system PATH), so the binary would
then resolve from the surrounding environment. This test pins the current
"lockdir shadows ambient" behavior; it flips to the ambient fallback once
narrowing lands.

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

[mybin] is not a declared dep, but the lockdir copy is still resolved before
the ambient PATH copy:

  $ PATH="$PWD/fakebin:$PATH" dune build @all
  $ cat _build/default/mybin-avail
  true
  $ cat _build/default/mybin-out
  from lockdir

[(system mybin)] resolves via $PATH. The lockdir provider's bin layout is on
$PATH and the shell finds the lockdir binary.

  $ cat _build/default/mybin-system
  from lockdir

  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  $PWD/_build/_private/default/.pkg/provider.0.0.1-$DIGEST/target/bin
  $PWD/fakebin
