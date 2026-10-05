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

[(system mybin)] resolves via $PATH.

  $ cat _build/default/mybin-system
  from system

  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  $PWD/fakebin

An unscoped lookup must not build opaque packages to discover binaries. Native
binaries have declared names and remain available without building every
package. A missing optional tool should simply disable its rule.

  $ make_lockpkg unrelated <<'EOF'
  > (version 0.0.1)
  > (build (system "echo unexpected-unscoped-lookup; exit 1"))
  > EOF
  $ mkdir native
  $ cat >native/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name native-tool))
  > EOF
  $ cat >native/dune <<'EOF'
  > (install (section bin) (files (tool.sh as native-tool)))
  > EOF
  $ cat >native/tool.sh <<'EOF'
  > #!/bin/sh
  > echo from native
  > EOF
  $ chmod +x native/tool.sh
  $ cat >>dune <<'EOF'
  > (rule
  >  (with-stdout-to native-output (run %{bin:native-tool})))
  > (rule
  >  (with-stdout-to missing-available
  >   (echo %{bin-available:missing-unscoped-tool})))
  > (rule
  >  (target disabled)
  >  (enabled_if %{bin-available:missing-unscoped-tool})
  >  (action (run false)))
  > EOF

First with no workspace package, then with a package that has no directory
scope. Both currently force the unrelated opaque package.

  $ make_dune_project 3.25
  $ PATH="$PWD/fakebin:$PATH" dune build @all >output 2>&1
  [1]
  $ grep '^unexpected-unscoped-lookup$' output
  unexpected-unscoped-lookup
  $ cat >>dune-project <<'EOF'
  > (package (name mypkg) (allow_empty))
  > EOF
  $ PATH="$PWD/fakebin:$PATH" dune build @all >output 2>&1
  [1]
  $ grep '^unexpected-unscoped-lookup$' output
  unexpected-unscoped-lookup

An explicit package scope restricts the search, so this succeeds. The ambient
and native tools remain usable; the undeclared opaque package is not forced.

  $ make_dune_project 3.25
  $ cat >>dune-project <<'EOF'
  > (package
  >  (name mypkg)
  >  (allow_empty)
  >  (dir .)
  >  (depends native-tool))
  > EOF
  $ PATH="$PWD/fakebin:$PATH" dune build @all
  $ cat _build/default/mybin-out _build/default/native-output _build/default/missing-available
  from system
  from native
  false

Declaring the opaque provider in the package scope still builds and selects it.
The unrelated package must remain unbuilt.

  $ make_dune_project 3.25
  $ cat >>dune-project <<'EOF'
  > (package
  >  (name mypkg)
  >  (allow_empty)
  >  (dir .)
  >  (depends native-tool provider))
  > EOF
  $ PATH="$PWD/fakebin:$PATH" dune build @all
  $ cat _build/default/mybin-out
  from lockdir
  $ test ! -e _build/default/.lockfile/pkg/unrelated/.opam/unrelated/target

An explicit action dependency supplies a package list even without a directory
scope. Its opaque tool must take precedence over the ambient copy.

  $ make_dune_project 3.25
  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package provider))
  >  (action (with-stdout-to explicit-output (run %{bin:mybin}))))
  > EOF
  $ PATH="$PWD/fakebin:$PATH" dune build explicit-output >output 2>&1
  [1]
  $ grep '^unexpected-unscoped-lookup$' output
  unexpected-unscoped-lookup

Building the provider must not change subsequent unscoped lookup. Both the
pform and the shell should select the ambient tool, independently of build
history. Currently, even with the provider already built, lookup still tries
to build every other opaque package.

  $ cat >dune <<'EOF'
  > (rule (with-stdout-to after-bin (run %{bin:mybin})))
  > (rule (with-stdout-to after-system (system mybin)))
  > EOF
  $ PATH="$PWD/fakebin:$PATH" dune build after-bin after-system >output 2>&1
  [1]
  $ grep '^unexpected-unscoped-lookup$' output
  unexpected-unscoped-lookup
