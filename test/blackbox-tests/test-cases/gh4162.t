Cram doesn't like tests in paths

  $ mkdir "aaa bbb" && cd "aaa bbb"
  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (cram enable)
  > EOF

  $ cat >foo.t <<EOF
  >   $ echo foo
  > EOF

  $ cat foo.t
    $ echo foo

  $ dune build @foo
            sh (internal) (exit 1)
  (cd _build/.sandbox/54ffcedad804d81955c7b33d18d42e68/default && /run/current-system/sw/bin/sh /var/folders/nc/x9_nmmsj0rjbfyzxb_kjk6qr0000gn/T/build_95f84b_dune/dune_cram_01b015_.foo.t/main.sh)
  /var/folders/nc/x9_nmmsj0rjbfyzxb_kjk6qr0000gn/T/build_95f84b_dune/dune_cram_01b015_.foo.t/main.sh: line 5: printf: bbb/_build/.sandbox/54ffcedad804d81955c7b33d18d42e68/default:$TESTCASE_ROOT=$TESTCASE_ROOT/aaa: invalid number
  -> required by alias foo
  [1]
