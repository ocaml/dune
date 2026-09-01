A rule using a locked executable must be invalidated when runtime data installed
beside that executable changes.

  $ make_lockdir
  $ make_lockpkg provider <<'EOF'
  > (version 0.0.1)
  > (install
  >  (progn
  >   (run cp mybin %{pkg-self:bin}/mybin)
  >   (run chmod +x %{pkg-self:bin}/mybin)
  >   (run cp data.txt %{pkg-self:share}/data.txt)))
  > EOF

  $ make_dune_project 3.24
  $ provider_target="$(get_build_pkg_dir provider)/target"
  $ provider_data="$PWD/$provider_target/share/provider/data.txt"

  $ make_lockpkg_file provider mybin <<EOF
  > #!/bin/sh
  > cat "$provider_data"
  > EOF
  $ chmod +x dune.lock/provider.files/mybin
  $ echo one >dune.lock/provider.files/data.txt

  $ cat >dune <<'EOF'
  > (rule
  >  (target out)
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:mybin}))))
  > EOF

  $ dune build out
  $ cat _build/default/out
  one
  $ cp "$provider_target/bin/mybin" mybin.before

Change only runtime data in the package's .files directory.

  $ echo two >dune.lock/provider.files/data.txt
  $ dune build out
  $ cat "$provider_data"
  two
  $ cmp mybin.before "$provider_target/bin/mybin"

CR-someday alizter: This should print "two". The consuming rule depends only on
the unchanged executable, rather than its package runtime dependencies.

  $ cat _build/default/out
  one
