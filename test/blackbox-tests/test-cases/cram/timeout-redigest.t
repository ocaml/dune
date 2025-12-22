Testing how timeout affects the digest:

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ cat > mytest.t

This test counts the occurances of the cram script in the log.
  $ check() {
  > dune test --trace-file trace.json mytest.t
  > jq '[ .[] | select(.cat == "process" and (.args.categories == ["cram"])) ] | length' trace.json
  > }

We can observe the test is run the first time:

  $ check
  1

And is not run the second time:

  $ check
  0

If we add a timeout, we would not expect for the digest of the cram test to
change.

  $ cat > dune <<EOF
  > (cram
  >  (timeout 1))
  > EOF

However this is currently not the case and we rerun the cram test:

  $ check
  1

  $ check
  0

This is again the case on another time change:

  $ cat > dune <<EOF
  > (cram
  >  (timeout 2))
  > EOF
 
  $ check
  1

  $ check
  0

