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
