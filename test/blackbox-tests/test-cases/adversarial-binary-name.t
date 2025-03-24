Some macros treat ':' as an argument delimiter but others do not. A consequence
of this is macro arguments which intentionally include a ':' character may look
like a sequence of multiple arguments but are treated as a single argument by
macros that don't split their argument on ':'. This tests that we maintain this
behaviour as we change the way that macros are implemented.

This test installs a binary whose name contains a ':' character and then checks
that we can look up the binary with the `bin` macro which does not split its
arguments on ':'.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (package (name foo))
  > EOF

  $ cat > foo.sh <<EOF
  > #!/usr/bin/env sh
  > echo foo
  > EOF
  $ chmod +x foo.sh

  $ cat >dune <<EOF
  > ; Use an install stanza to rename the script to "foo:bar"
  > (install
  >  (section bin)
  >  (files (foo.sh as foo:bar)))
  > 
  > ; Generate out.txt by running the script now named "foo:bar"
  > (rule
  >  (target out.txt)
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:foo:bar}))))
  > EOF

  $ dune build out.txt

  $ cat _build/default/out.txt
  foo
