Test the --debug-digests command line option

  $ echo '(lang dune 3.0)' > dune-project
  $ touch x
  $ dune build x --wait-for-filesystem-clock
  $ echo 1 > x
  $ dune build x --wait-for-filesystem-clock --debug-digests 2>&1 | sed 's/stats =.*/stats = XXX/'
  Re-digested file x because its stats changed:
  { old_digest = digest "662cf3f7d59f76428301690d4ead67ae"
  ; new_digest = digest "f31e1f1c33564e07cd02ad2c52f4df85"
  ; old_stats = XXX
  ; new_stats = XXX
  }
