# Tests

Read README.md for a description of the tests.

# Rules For Writing Tests

- Prefer blackbox tests written in cram over unit tests.

- For unit tests, prefer writing `[%expect{|..|}]` blocks over assertions

- Only write unit tests for parts of the code that does not require mocking

- Instead of adding a `For_tests` module, consider writing the test right in the
  library using ppx_expect.

- Rules may not contain OCaml stacktraces. Disable stack trace collection or
  filter out the stack trace for failing test that attempt to produce
  a stacktrace.
