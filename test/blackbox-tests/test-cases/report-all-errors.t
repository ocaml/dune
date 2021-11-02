This test checks that we always report as many errors as possible. Or
put another way, that we always explore as much of the build DAG as
possible before stopping the build.

Technically, when there are multiple errors, the exact set of errors
we report depends on the internals of Dune. For instance, if a target
`a` depends on two independent targets `b` and `c` and both `b` and
`c` failed to build for independent reasons, then a naive
implementation might only report `b`'s failure. In such situation, the
first implementation of Dune (called jbuilder at the time) used to
report one of the two errors at random.

While the above example is simple, some situations are more
complex. This file checks the behaviour of Dune in such complex
situations to make sure we don't regress. We can also use it to record
cases we would like to improve.

Example 1
---------

In this example, we have a rule (attached to alias "default") which
cats the contents of two files `y` and `z`. Both of the the rules for
these two files are failing. What is more, the filename `z` is
accessed via an indirection. In the end, since the path to reach `z`
is independent of the path to reach `y`, both errors could be reported
at the same time.

However, previous versions of Dune only reported the failure for
`y`. This was because there was some staging in the evaluation of
rules, and the indirection to reach `z` meant that the build was
failing before it had a chance to start thinking about building `z`.

  $ echo '(lang dune 2.8)' > dune-project
  $ cat >dune<<EOF
  > (rule
  >  (alias default)
  >  (action
  >   (progn
  >    (cat %{read:x})
  >    (cat y))))
  > 
  > (rule (with-stdout-to x (system "printf z")))
  > 
  > (rule (with-stdout-to y (run ./fail.exe)))
  > (rule (with-stdout-to z (run ./fail.exe)))
  > 
  > (executable (name fail))
  > EOF
  $ echo 'exit 1' > fail.ml

  $ dune build
  File "dune", line 10, characters 0-42:
  10 | (rule (with-stdout-to y (run ./fail.exe)))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Command exited with code 1.
  File "dune", line 11, characters 0-42:
  11 | (rule (with-stdout-to z (run ./fail.exe)))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Command exited with code 1.
  [1]
