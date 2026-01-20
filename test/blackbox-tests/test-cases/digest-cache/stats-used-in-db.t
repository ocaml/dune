Reproduction case for a bug we discovered in the past; Dune was
storing the wrong stats for generated files in the digest cache and as
a result was always re-hashing all files in the build directory on the
second run.

More precisely, we were using the stats of the file before making it
read-only.

  $ export DUNE_TRACE=digest

  $ echo '(lang dune 3.0)' > dune-project
  $ touch x y z
  $ dune build x y z --wait-for-filesystem-clock

  $ dune trace cat | jq 'select(.cat == "digest")'

Check that the next build rehashes nothing:

  $ dune build x y z --wait-for-filesystem-clock

  $ dune trace cat | jq 'select(.cat == "digest")'
