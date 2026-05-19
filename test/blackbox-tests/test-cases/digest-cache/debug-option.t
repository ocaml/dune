Test the tracing of digest events

  $ export DUNE_TRACE="digest"

  $ echo '(lang dune 3.23)' > dune-project
  $ touch x
  $ dune build x --wait-for-filesystem-clock
  $ echo 1 > x
  $ dune build x --wait-for-filesystem-clock

  $ dune trace cat | jq '
  >   select(.cat == "digest")
  > | .args
  > | .old_stats.mtime |= type
  > | .new_stats.mtime |= type
  > | .new_stats.dev |= type
  > | .new_stats.ino |= type
  > | .old_stats.dev |= type
  > | .old_stats.ino |= type
  > '
  {
    "path": "x",
    "old_digest": "662cf3f7d59f76428301690d4ead67ae",
    "new_digest": "f31e1f1c33564e07cd02ad2c52f4df85",
    "old_stats": {
      "mtime": "number",
      "size": 0,
      "perm": 420,
      "dev": "number",
      "ino": "number"
    },
    "new_stats": {
      "mtime": "number",
      "size": 2,
      "perm": 420,
      "dev": "number",
      "ino": "number"
    }
  }

  $ mkdir dir
  $ touch dir/one.in
  $ cat > dune <<EOF
  > (rule
  >  (alias default)
  >  (deps (glob_files dir/*.in))
  >  (target result)
  >  (action (with-stdout-to %{target} (echo ok))))
  > EOF
  $ dune build @default --wait-for-filesystem-clock
  $ touch dir/two.in
  $ dune build @default --wait-for-filesystem-clock

  $ dune trace cat | jq '
  >   select(.cat == "digest" and .name == "reread_dir" and .args.path == "dir")
  > | .args
  > | .old_contents |= keys
  > | .new_contents |= keys
  > | .old_stats |= with_entries(.value |= type)
  > | .new_stats |= with_entries(.value |= type)
  > '
  {
    "path": "dir",
    "old_contents": [
      "one.in"
    ],
    "new_contents": [
      "one.in",
      "two.in"
    ],
    "old_stats": {
      "mtime": "number",
      "size": "number",
      "perm": "number",
      "dev": "number",
      "ino": "number"
    },
    "new_stats": {
      "mtime": "number",
      "size": "number",
      "perm": "number",
      "dev": "number",
      "ino": "number"
    }
  }
