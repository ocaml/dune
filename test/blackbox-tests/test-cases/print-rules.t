Tests for the output of `dune rules`, covering:
- anonymous actions (actions attached to an alias) showing up as rules;
- rule locations being included when `--with-locs` is passed;
- defaulting to the `@default` alias when no target is given;
- dependency cycles being reported.

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

Passing `--with-locs` includes the location of each rule.
  $ dune rules --with-locs t
  ((deps ())
   (targets ((files (_build/default/t)) (directories ())))
   (context default)
   (action (chdir _build/default (with-stdout-to t (echo x))))
   (loc dune:4))

Anonymous actions carry a location too.
  $ dune rules --with-locs @a
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

  $ dune rules --with-locs @sub/b
  ((deps ())
   (action (chdir _build/default/sub (echo "sub\n")))
   (aliases (b))
   (loc sub/dune:1))

With no target given, `dune rules` defaults to the `@default` alias. Here that
alias only depends on `@a`, so only the anonymous action is printed - notably
the `t` rule, which is not reachable from `@default`, is absent.
  $ dune rules
  ((deps ()) (action (chdir _build/default (echo "hi\n"))) (aliases (a)))

A cycle made only of aliases is caught while the aliases are expanded, so it is
described in terms of the aliases rather than the rules they contain.
  $ cat > dune << EOF
  > (rule (alias x) (deps (alias y)) (action (echo "x\n")))
  > (rule (alias y) (deps (alias x)) (action (echo "y\n")))
  > EOF
  $ dune rules -r @x
  Error: Dependency cycle between:
     alias x in dune:1
  -> alias y in dune:2
  -> alias x in dune:1
  [1]

A cycle between file targets is only discovered once the rules are collected,
and is reported in terms of the targets.
  $ cat > dune << EOF
  > (rule (target a) (deps b) (action (copy b a)))
  > (rule (target b) (deps a) (action (copy a b)))
  > EOF
  $ dune rules -r a
  Error: Dependency cycle detected:
     _build/default/a in dune:1
  -> _build/default/b in dune:2
  -> _build/default/a in dune:1
  [1]

Expanding an alias does not evaluate the rules its files come from, so a cycle
that leaves an anonymous action through a file target is also only found while
collecting the rules. The anonymous action has no target to name it by, so it is
reported with its alias.
  $ cat > dune << EOF
  > (rule (alias c) (deps a) (action (echo "c\n")))
  > (rule (target a) (deps (alias c)) (action (with-stdout-to a (echo "a"))))
  > EOF
  $ dune rules -r @c
  Error: Dependency cycle detected:
     alias c in dune:1
  -> _build/default/a in dune:2
  -> alias c in dune:1
  [1]
