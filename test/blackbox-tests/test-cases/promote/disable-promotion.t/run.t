This tests shows how all promotion to the source dir may be disabled. This
includes .install files and manually promoted executables

  $ dune build -p foo --disable-promotion

foo.exe and foo.install are absent
  $ test -f foo.exe && echo "foo.exe exists"
  [1]
  $ test -f foo.install && echo "foo.install exists"
  [1]

now we build without the option and see that they are present:
  $ dune build -p foo

  $ test -f foo.exe && echo "foo.exe exists"
  foo.exe exists
  $ test -f foo.install && echo "foo.install exists"
  foo.install exists

dune fmt rejects --disable-promotion:
  $ dune fmt --disable-promotion
  Error: The --disable-promotion flag is not applicable to `dune fmt`. Use
  --preview to display formatting changes without applying them.
  [1]

dune promotion apply rejects --disable-promotion:
  $ dune promotion apply --disable-promotion
  Error: The --disable-promotion flag is not applicable to `dune promotion
  apply`.
  [1]
