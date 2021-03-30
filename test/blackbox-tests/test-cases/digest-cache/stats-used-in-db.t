This test expose the following problem: inside the digest cache, we
store parts of the file stats to detect when it changes and should be
re-hashed. However, for targets we use the stats before removing the
write permissions, which means that during the second run, dune always
re-hash all the targets.


  $ echo '(lang dune 3.0)' > dune-project
  $ touch x y z
  $ dune build x y z --wait-for-filesystem-clock

The following shouldn't rehash anything:

  $ dune build x y z --wait-for-filesystem-clock --debug-digests 2>&1 | sed 's/stats = { .*\( perm = [0-9]*\).*}/stats = {\1; ... }/'
  Re-digested file _build/default/x because its stats changed:
  { old_digest = digest "b83631c134a9649ec383d0eb9c356803"
  ; new_digest = digest "b83631c134a9649ec383d0eb9c356803"
  ; old_stats = { perm = 436; ... }
  ; new_stats = { perm = 292; ... }
  }
  Re-digested file _build/default/y because its stats changed:
  { old_digest = digest "b83631c134a9649ec383d0eb9c356803"
  ; new_digest = digest "b83631c134a9649ec383d0eb9c356803"
  ; old_stats = { perm = 436; ... }
  ; new_stats = { perm = 292; ... }
  }
  Re-digested file _build/default/z because its stats changed:
  { old_digest = digest "b83631c134a9649ec383d0eb9c356803"
  ; new_digest = digest "b83631c134a9649ec383d0eb9c356803"
  ; old_stats = { perm = 436; ... }
  ; new_stats = { perm = 292; ... }
  }
