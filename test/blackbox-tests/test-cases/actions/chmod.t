Testing the capabilities of the chmod action.

  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > EOF

We create a file A and then chmod it to 777.
  $ cat > dune << EOF
  > (rule
  >  (targets A)
  >  (action (with-stdout-to A (echo "Hello World!"))))
  > (rule
  >  (alias chmod)
  >  (deps A)
  >  (action (chmod 0o777 A)))
  > EOF

  $ dune build A
  $ stat -c %a _build/default/A
  444
  $ dune build @chmod
  $ stat -c %a _build/default/A
  777

We take an existing file A and then chmod it to 777.
  $ cat > A

  $ cat > dune << EOF
  > (rule
  >  (alias chmod)
  >  (deps A)
  >  (action (chmod 0o777 A)))
  > EOF

  $ dune build A
  $ stat -c %a _build/default/A
  444
  $ dune build @chmod
  $ stat -c %a _build/default/A
  777


