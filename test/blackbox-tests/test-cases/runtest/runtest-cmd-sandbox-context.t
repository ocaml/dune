Sandboxed cram failures should preserve their build context.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ cat > dune-workspace <<EOF
  > (lang dune 3.23)
  > (context (default (name alt)))
  > EOF

  $ cat > mytest.t <<EOF
  >   $ echo "Hello, world!"
  >   "Goodbye, world!"
  > EOF

  $ dune test --sandbox symlink mytest.t
  File "mytest.t", line 1, characters 0-0:
  --- mytest.t
  +++ mytest.t.corrected
  @@ -1,2 +1,2 @@
     $ echo "Hello, world!"
  -  "Goodbye, world!"
  +  Hello, world!
  [1]
