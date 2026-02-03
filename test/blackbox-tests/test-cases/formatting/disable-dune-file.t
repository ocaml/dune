This tests how it is possible to disable formatting for a particular dialect in
a given subdirectory. This can be used to disable formatting of a particular
dune file.

  $ cat > dune-project << EOF
  > (lang dune 2.8)
  > EOF

  $ cat > dune << EOF
  > ; this file should be formatted
  > (rule (write-file a b))
  > EOF

  $ mkdir sub
  $ cat > sub/dune << EOF
  > ; this file should not
  > (env
  >  (_
  >   (formatting (enabled_for ocaml))))
  > 
  > (rule (write-file a b))
  > EOF

  $ dune build @fmt
  File "dune", line 1, characters 0-0:
  --- dune
  +++ .formatted/dune
  @@ -1,2 +1,4 @@
   ; this file should be formatted
  -(rule (write-file a b))
  +
  +(rule
  + (write-file a b))
  [1]

Disable foramtting in the root directory using context settings

  $ cat >dune-workspace <<EOF
  > (lang dune 3.11)
  > (env
  >  (_
  >   (formatting disabled)))
  > EOF

  $ dune build @fmt
