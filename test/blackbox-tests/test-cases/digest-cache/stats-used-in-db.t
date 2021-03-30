Reproduction case for a bug we discovered in the past; Dune was
storing the wrong stats for generated files in the digest cache and as
a result was always re-hashing all files in the build directory on the
second run.

More precisely, we were using the stats of the file before making it
read-only.


  $ echo '(lang dune 3.0)' > dune-project
  $ touch x y z
  $ dune build x y z --wait-for-filesystem-clock

Check that the next build rehashes nothing:

  $ dune build x y z --wait-for-filesystem-clock --debug-digests 2>&1 | sed 's/stats = { .*\( perm = [0-9]*\).*}/stats = {\1; ... }/'
