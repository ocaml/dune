Tests for the output of `dune rules`, covering:
- anonymous actions (actions attached to an alias) showing up as rules;
- rule locations being omitted unless `--with_locs` is passed;
- defaulting to the `@default` alias when no target is given.

  $ cat > dune-project << EOF
  > (lang dune 3.5)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (alias a)
  >  (action (echo "hi\n")))
  > (rule
  >  (target t)
  >  (action (with-stdout-to t (echo "x"))))
  > (alias
  >  (name default)
  >  (deps (alias a)))
  > EOF

An anonymous action (a rule attached to an alias with no target) shows up as a
rule, with no targets and an `aliases` field listing the aliases it is attached
to.
  $ dune rules @a
  ((deps ()) (action (chdir _build/default (echo "hi\n"))) (aliases (a)))

By default, rule locations are omitted from the output.
  $ dune rules t
  ((deps ())
   (targets ((files (_build/default/t)) (directories ())))
   (context default)
   (action (chdir _build/default (with-stdout-to t (echo x)))))

Passing `--with_locs` includes the location of each rule.
  $ dune rules --with_locs t
  ((deps ())
   (targets ((files (_build/default/t)) (directories ())))
   (context default)
   (action (chdir _build/default (with-stdout-to t (echo x))))
   (loc dune:4))

Anonymous actions carry a location too.
  $ dune rules --with_locs @a
  ((deps ())
   (action (chdir _build/default (echo "hi\n")))
   (aliases (a))
   (loc dune:1))

For a rule in a subdirectory, the location includes the full path to the dune
file, not just its basename.
  $ mkdir sub
  $ cat > sub/dune << EOF
  > (rule
  >  (alias b)
  >  (action (echo "sub\n")))
  > EOF

  $ dune rules --with_locs @sub/b
  ((deps ())
   (action (chdir _build/default/sub (echo "sub\n")))
   (aliases (b))
   (loc sub/dune:1))

With no target given, `dune rules` defaults to the `@default` alias. Here that
alias only depends on `@a`, so only the anonymous action is printed - notably
the `t` rule, which is not reachable from `@default`, is absent.
  $ dune rules
  ((deps ()) (action (chdir _build/default (echo "hi\n"))) (aliases (a)))
