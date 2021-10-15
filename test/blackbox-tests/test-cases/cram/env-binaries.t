Cram and private binaries from the env stanza

  $ mkdir fresh && cd fresh

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (cram enable)
  > EOF

  $ mkdir helper
  $ cat >helper/dune <<EOF
  > (executable (name helper))
  > EOF
  $ cat >helper/helper.ml <<EOF
  > print_endline "Helper launched successfully";;
  > EOF

  $ env_stanza="(env (_ (binaries (helper/helper.exe as helper))))"
  $ test_stanza="(cram (deps %{bin:helper}))"

First we test case where the cram stanza is one level below

  $ mkdir tests
  $ cat >tests/run.t <<EOF
  >   $ helper
  >   Helper launched successfully
  > EOF
  $ printf "%s\n" $test_stanza > tests/dune
  $ printf "%s\n" $env_stanza > dune

  $ dune runtest

Next, we test the case where the cram stanza is in the same directory as the
env stanza:

  $ printf "%s\n%s\n" $env_stanza $test_stanza > dune
  $ mv tests/run.t ./
  $ rm -r tests/

  $ dune runtest
