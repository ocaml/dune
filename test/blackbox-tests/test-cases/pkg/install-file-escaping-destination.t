An install file entry may name a destination outside its section's directory
for the package, as opam allows. A dependent finds the file where it was
installed.

  $ make_lockdir
  $ make_lockpkg escape <<'EOF2'
  > (version 1.0)
  > (build
  >  (progn
  >   (write-file META meta)
  >   (write-file escape.install "lib: [ \"META\" \"META\" {\"../other/META\"} ]")))
  > EOF2
  $ make_lockpkg consumer <<'EOF2'
  > (version 1.0)
  > (depends escape)
  > (build (run cat %{pkg:escape:lib}/../other/META))
  > EOF2
  $ build_pkg consumer
  meta
