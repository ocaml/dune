Moving a project with a cached cram result should invalidate that result.

This nested cram test prints a checksum of its working directory. The checksum
depends on the absolute workspace path, but the output itself is not a path
that Dune rewrites in the final transcript.

Create a nested project:

  $ mkdir before
  $ cd before
  $ cat > dune-project <<'EOF'
  > (lang dune 3.0)
  > (cram enable)
  > EOF
  $ cat > x.t <<'EOF'
  >   $ pwd | cksum | cut -d' ' -f1
  > EOF

Seed the expected output from the first run:

  $ dune runtest x.t > /dev/null 2>&1; echo $?
  1
  $ dune promote x.t > /dev/null 2>&1; echo $?
  0
  $ dune runtest x.t > /dev/null 2>&1; echo $?
  0

After moving the project root, Dune should rerun the cram test rather than
reusing the cached result from the old workspace path:

  $ cd ..
  $ mv before after
  $ cd after
  $ dune runtest x.t > /dev/null 2>&1; echo $?
  0
