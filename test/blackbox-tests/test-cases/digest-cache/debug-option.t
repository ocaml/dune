Test the tracing of digest events

  $ export DUNE_TRACE="digest"

  $ echo '(lang dune 3.0)' > dune-project
  $ touch x
  $ dune build x --wait-for-filesystem-clock
  $ echo 1 > x
  $ dune build x --wait-for-filesystem-clock

  $ dune trace cat | jq '
  >   select(.cat == "digest")
  > | .args
  > | .old_stats.mtime |= type
  > | .new_stats.mtime |= type
  > '
  {
    "path": "x",
    "old_digest": "662cf3f7d59f76428301690d4ead67ae",
    "new_digest": "f31e1f1c33564e07cd02ad2c52f4df85",
    "old_stats": {
      "mtime": "number",
      "size": 0,
      "perm": 420
    },
    "new_stats": {
      "mtime": "number",
      "size": 2,
      "perm": 420
    }
  }
