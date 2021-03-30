Test the --debug-digests command line option

  $ echo '(lang dune 3.0)' > dune-project
  $ touch x
  $ dune build x --wait-for-filesystem-clock
  $ echo 1 > x
  $ dune build x --wait-for-filesystem-clock --debug-digests 2>&1 | sed 's/stats =.*/stats = XXX/'
  Re-digested file x because its stats changed:
  { old_digest = digest "b83631c134a9649ec383d0eb9c356803"
  ; new_digest = digest "705e0d7e5030b1831b18211b1e398faf"
  ; old_stats = XXX
  ; new_stats = XXX
  }
