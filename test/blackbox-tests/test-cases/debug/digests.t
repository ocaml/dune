Test the --debug-digests command line option

  $ echo '(lang dune 3.0)' > dune-project
  $ touch x

The bellow dune file make sure we wait for the fs clock to advance
after building x and before finishing the build. This is to make this
test more reproducible as Dune drops entries that have the same mtime
as the file system clock when saving the digest cache to disk.

  $ cat >dune<<EOF
  > (rule
  >  (alias default)
  >  (deps x)
  >  (action (run dune_cmd wait-for-fs-clock-to-advance)))
  > EOF

  $ dune build
  $ echo 1 > x
  $ dune build --debug-digests 2>&1 | sed 's/stats =.*/stats = XXX/' | grep -A 5 "file x "
  Re-digested file x because its stats changed:
  { old_digest = digest "b83631c134a9649ec383d0eb9c356803"
  ; new_digest = digest "705e0d7e5030b1831b18211b1e398faf"
  ; old_stats = XXX
  ; new_stats = XXX
  }
