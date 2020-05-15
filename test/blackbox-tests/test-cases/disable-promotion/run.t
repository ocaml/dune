This tests shows how all promotion to the source dir may be disabled. This
includes both .install and .merlin files

  $ dune build --disable-promotion @all
.merlin is absent
  $ test -f .merlin && echo ".merlin exists"
  [1]

now we build without the option and see that it is present:
  $ dune build @all
  $ test -f .merlin && echo ".merlin exists"
  .merlin exists
