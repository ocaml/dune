Testing the run-with-conditional-terms action

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF

Case with no conditional terms
  $ cat >dune.lock/foo.pkg <<EOF
  > (install
  >  (run-with-conditional-terms echo Hello, World!))
  > EOF
  $ dune build .pkg/foo/target/
  Hello, World!

Case with mix of conditional and unconditional terms in the program's arguments
  $ cat >dune.lock/foo.pkg <<EOF
  > (install
  >  (run-with-conditional-terms echo (true foo) bar (false baz) ((= 2 3) qux)))
  > EOF
  $ dune build .pkg/foo/target/
  foo bar

Case where the first term is conditional and included
  $ mkdir dune.lock/foo.files
  $ cat >dune.lock/foo.files/hello.txt <<EOF
  > Hello, World!
  > EOF
  $ cat >dune.lock/foo.pkg <<EOF
  > (install
  >  (run-with-conditional-terms (true echo) (false cat) hello.txt))
  > EOF
  $ dune build .pkg/foo/target/
  hello.txt

Case where the first term is conditional and excluded
  $ cat >dune.lock/foo.pkg <<EOF
  > (install
  >  (run-with-conditional-terms (false echo) (true cat) hello.txt))
  > EOF
  $ dune build .pkg/foo/target/
  Hello, World!

Case where the first non-filtered term is not a program
  $ cat >dune.lock/foo.pkg <<EOF
  > (install
  >  (run-with-conditional-terms (false echo) (false cat) (true hello.txt)))
  > EOF
  $ dune build .pkg/foo/target/
  File "dune.lock/foo.pkg", line 2, characters 60-69:
  2 |  (run-with-conditional-terms (false echo) (false cat) (true hello.txt)))
                                                                  ^^^^^^^^^
  Error: When using the run-with-conditional-terms action the first term which
  is either non-conditional or whose condition is true must be a program.
  In this case the first term was "hello.txt" which is not a valid program.
  [1]

Case where the first non-filtered term is computed from a variable
  $ cat >dune.lock/foo.pkg <<EOF
  > (install
  >  (run-with-conditional-terms (false echo) (false cat) (true %{context_name})))
  > EOF
  $ dune build .pkg/foo/target/
  File "dune.lock/foo.pkg", line 2, characters 60-75:
  2 |  (run-with-conditional-terms (false echo) (false cat) (true %{context_name})))
                                                                  ^^^^^^^^^^^^^^^
  Error: When using the run-with-conditional-terms action the first term which
  is either non-conditional or whose condition is true must be a program.
  In this case the first term was "default" which is not a valid program.
  [1]
