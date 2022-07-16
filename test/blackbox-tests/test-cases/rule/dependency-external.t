rules with dependencies outside the build dir are allowed

  $ mkdir -p a/b

  $ cat >a/b/dune-project <<EOF
  > (lang dune 2.8)
  > EOF

### absolute path

## Test absolute
  $ cat >a/b/dune <<EOF
  > (rule
  >  (alias test)
  >  (action (with-stdin-from "$(pwd)/external.txt" (run cat -))))
  > EOF

  $ cat >external.txt <<EOF
  > txt1
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  txt1

  $ dune build --root=a/b @test
  Entering directory 'a/b'

  $ cat >external.txt <<EOF
  > txt2
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  txt2

## Test copy files absolute
  $ cat >a/b/dune <<EOF
  > (rule
  >  (alias test)
  >  (action (with-stdin-from "external.txt" (run cat -))))
  > (copy_files "$(pwd)/external.txt")
  > EOF

  $ cat >external.txt <<EOF
  > txt1
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  txt1

# Check that nothing is done when nothing change
  $ dune build --root=a/b @test
  Entering directory 'a/b'

  $ cat >external.txt <<EOF
  > txt2
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  txt2

### 1 level below

## Test relative 1 level below
  $ cat >a/b/dune <<EOF
  > (rule
  >  (alias test)
  >  (action (with-stdin-from "../external.txt" (run cat -))))
  > EOF

  $ cat >a/external.txt <<EOF
  > txt1
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  txt1

# Check that nothing is done when nothing change
  $ dune build --root=a/b @test
  Entering directory 'a/b'

  $ cat >a/external.txt <<EOF
  > txt2
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  txt2

## Test copy files 1 level below
  $ cat >a/b/dune <<EOF
  > (rule
  >  (alias test)
  >  (action (with-stdin-from "external.txt" (run cat -))))
  > (copy_files ../external.txt)
  > EOF

  $ cat >a/external.txt <<EOF
  > txt1
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  txt1

# Check that nothing is done when nothing change
  $ dune build --root=a/b @test
  Entering directory 'a/b'

  $ cat >a/external.txt <<EOF
  > txt2
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  txt2

### 2 level below

## Test relative 2 level below
  $ cat >a/b/dune <<EOF
  > (rule
  >  (alias test)
  >  (action (with-stdin-from "../../external.txt" (run cat -))))
  > EOF

  $ rm -f a/external.txt

  $ cat >external.txt <<EOF
  > txt1
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  txt1

# Check that nothing is done when nothing change
  $ dune build --root=a/b @test
  Entering directory 'a/b'

  $ cat >external.txt <<EOF
  > txt2
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  txt2

## Test copy files 2 level below
  $ cat >a/b/dune <<EOF
  > (rule
  >  (alias test)
  >  (action (with-stdin-from "external.txt" (run cat -))))
  > (copy_files ../../external.txt)
  > EOF

  $ cat >external.txt <<EOF
  > txt1
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  txt1

# Check that nothing is done when nothing change
  $ dune build --root=a/b @test
  Entering directory 'a/b'

  $ cat >external.txt <<EOF
  > txt2
  > EOF

  $ dune build --root=a/b @test
  Entering directory 'a/b'
  txt2

## Test dune exec absolute
  $ cat >a/script.sh <<EOF
  > #!/bin/sh
  >  echo "txt1"
  > EOF

  $ chmod u+x a/script.sh

  $ dune exec --root=a/b -- $(pwd)/a/script.sh
  Entering directory 'a/b'
  txt1

## Test dune exec 1 level below
  $ dune exec --root=a/b -- ../script.sh
  Entering directory 'a/b'
  txt1

## Test dune exec 2 level below
  $ mv a/script.sh .

  $ dune exec --root=a/b -- ../../script.sh
  Entering directory 'a/b'
  txt1

# Regression test for #5572
  $ dune exec --root=a/b -- ../
  Entering directory 'a/b'
  Error: Program "../" not found!
  [1]
