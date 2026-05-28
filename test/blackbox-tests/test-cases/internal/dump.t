Test internal commands.

  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >  (mode (promote (until-clean)))
  >  (action (with-stdout-to x (progn))))
  > EOF
  $ dune build x
  $ dune internal dump _build/.to-delete-in-source-tree
  set { "x" }

Pretty-print dune s-expressions from stdin.

  $ cat <<'EOF' | dune internal sexp-pp
  > ((foo
  >   bar) baz)
  > quux
  > EOF
  ((foo bar)
   baz)
  
  quux

Pretty-print canonical s-expressions from a file.

  $ printf '%s' '3:foo(3:bar3:baz)5:hello' > input.csexp
  $ dune internal sexp-pp --format=csexp input.csexp
  foo
  
  (bar baz)
  
  hello

Convert regular s-expressions to csexps.

  $ printf '%s' '(foo (bar baz) "hello world")' > input.sexp
  $ dune internal sexp-to-csexp input.sexp > output.csexp
  $ dune internal sexp-pp --format=csexp --compact output.csexp
  (foo (bar baz) "hello world")
