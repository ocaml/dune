Separator before the deterministic error summary
================================================

With `--error-reporting=twice`, each error is reported once as soon as it
is discovered and once more at the end of the build, in a deterministic
order. The second batch is preceded by a separator so the summary is easy
to find (issue #14757).

  $ echo '(lang dune 3.0)' > dune-project
  $ cat >dune<<EOF
  > (rule (with-stdout-to x (run ./fail.exe)))
  > (executable (name fail))
  > EOF
  $ echo 'exit 1' > fail.ml

  $ dune build x --error-reporting=twice
  File "dune", line 1, characters 0-42:
  1 | (rule (with-stdout-to x (run ./fail.exe)))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Command exited with code 1.
  ==== Error Summary ====
  File "dune", line 1, characters 0-42:
  1 | (rule (with-stdout-to x (run ./fail.exe)))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Command exited with code 1.
  [1]

When the build succeeds there are no errors to report, so the separator
is not printed.

  $ cat >dune<<EOF
  > (rule (with-stdout-to x (system "printf ok")))
  > EOF
  $ dune build x --error-reporting=twice
