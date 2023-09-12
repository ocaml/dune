  $ . ./helpers.sh

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF

Helper function to create a lockfile with a given install action and then build it, running the action.
  $ test_action() {
  >   dune clean || true
  >   echo "(install $1)" > dune.lock/test.pkg
  >   build_pkg test
  > }

Tests for concat:
  $ test_action '(run echo (concat foo))'
  foo
  $ test_action '(run echo (concat foo bar))'
  foobar
  $ test_action '(run echo (concat foo (concat bar (concat baz (concat)))))'
  foobarbaz
  $ test_action '(run echo (concat %{pkg-self:name} . %{pkg-self:version}))'
  test.dev
  $ test_action '(run (concat e c h o) foo)'
  foo
  $ test_action '(run echo foo (concat) bar (concat) baz)' # two spaces between each word because (concat) is the empty string
  foo  bar  baz

Tests for when:
  $ test_action '(run echo (when true foo) bar (when false baz) qux)'
  foo bar qux
  $ test_action '(run (when (= a b) xxx) (when (and (< 41 42) (<> foo bar)) echo) foo)'
  foo
  $ test_action '(run (when invalid-condition echo) foo)'
  File "dune.lock/test.pkg", line 1, characters 20-37:
  1 | (install (run (when invalid-condition echo) foo))
                          ^^^^^^^^^^^^^^^^^
  Error: This expression is used as a condition and so must evaluate to either
  "true" or "false", however it evaluated to "invalid-condition".
  [1]
  $ test_action '(run (when (and (< 1 2) (concat complex invalid condition)) echo) foo)'
  File "dune.lock/test.pkg", line 1, characters 33-67:
  1 | (install (run (when (and (< 1 2) (concat complex invalid condition)) echo) foo))
                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: This expression is used as a condition and so must evaluate to either
  "true" or "false", however it evaluated to "complexinvalidcondition".
  [1]
  $ test_action '(run echo (when (and (< 1 2) (or (concat t r u e) (concat f a l s e))) foo) bar)'
  foo bar
  $ test_action '(run echo (when (or true invalid-condition-ignored-due-to-laziness) foo))'
  foo
  $ test_action '(run echo (when (not (and false invalid-condition-ignored-due-to-laziness)) foo))'
  foo
  $ test_action '(run echo (when (when false xxx) foo))'
  File "dune.lock/test.pkg", line 1, characters 25-41:
  1 | (install (run echo (when (when false xxx) foo)))
                               ^^^^^^^^^^^^^^^^
  Error: This expression is used as a condition and so must evaluate to a
  single string whose value is either "true" or "false", however it evaluated
  to Nil (ie. zero strings)
  [1]
  $ test_action '(run echo (concat (when true foo) (when false bar) baz))'
  foobaz
  $ test_action '(run echo (when (or %{pkg-self:not_a_variable} true) foo))'
  File "dune.lock/test.pkg", line 1, characters 29-55:
  1 | (install (run echo (when (or %{pkg-self:not_a_variable} true) foo)))
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Undefined package variable "not_a_variable"
  [1]
  $ test_action '(run echo (when (or true %{pkg-self:not_a_variable}) foo))'
  foo

Tests for if:
  $ test_action '(run echo (if (= %{pkg-self:version} dev) foo bar) (if (<> %{pkg-self:name} test) baz qux))'
  foo qux
  $ test_action '(run echo (if (if true true false) (concat foo bar) (concat baz qux)))'
  foobar
  $ test_action '(run echo (if inivalid-condition foo bar))'
  File "dune.lock/test.pkg", line 1, characters 23-41:
  1 | (install (run echo (if inivalid-condition foo bar)))
                             ^^^^^^^^^^^^^^^^^^
  Error: This expression is used as a condition and so must evaluate to either
  "true" or "false", however it evaluated to "inivalid-condition".
  [1]

Tests for has_undefined_var:
  $ test_action '(run echo (if (has_undefined_var %{pkg-self:name}) foo bar))'
  bar
  $ test_action '(run echo (if (has_undefined_var %{pkg-self:not_a_variable}) foo bar))'
  foo
  $ test_action '(run echo (if (has_undefined_var (when %{pkg-self:not_a_variable} foo)) foo bar))'
  foo

Test the error message when the program doesn't exist:
  $ test_action '(run madeup)'
  File "dune.lock/test.pkg", line 1, characters 14-20:
  1 | (install (run madeup))
                    ^^^^^^
  Error: Program madeup not found in the tree or in PATH
   (context: default)
  [1]
  $ test_action '(run (concat e c h o) hello)'
  hello
  $ test_action '(run (concat m a d e u p) hello)'
  File "dune.lock/test.pkg", line 1, characters 14-34:
  1 | (install (run (concat m a d e u p) hello))
                    ^^^^^^^^^^^^^^^^^^^^
  Error: Program madeup not found in the tree or in PATH
   (context: default)
  [1]
  $ test_action '(run (if true madeup echo) hello)'
  File "dune.lock/test.pkg", line 1, characters 14-35:
  1 | (install (run (if true madeup echo) hello))
                    ^^^^^^^^^^^^^^^^^^^^^
  Error: Program madeup not found in the tree or in PATH
   (context: default)
  [1]
