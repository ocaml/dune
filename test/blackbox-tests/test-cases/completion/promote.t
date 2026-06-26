Tab completion for `dune promote` lists pending promotions.

  $ make_dune_project 3.25

With no pending promotions, the candidate list is empty.

  $ dune_complete promote ""

Set up two pending promotions: one at the top level, one in a subdir.

  $ cat > dune <<EOF
  > (rule (alias runtest) (action (diff a.expected a.actual)))
  > (rule (write-file a.actual "ACTUAL\n"))
  > (subdir sub
  >  (rule (alias runtest) (action (diff b.expected b.actual)))
  >  (rule (write-file b.actual "SUBACTUAL\n")))
  > EOF
  $ echo "expected" > a.expected
  $ mkdir sub && echo "subexpected" > sub/b.expected
  $ dune runtest
  File "a.expected", line 1, characters 0-0:
  --- a.expected
  +++ a.actual
  @@ -1 +1 @@
  -expected
  +ACTUAL
  File "sub/b.expected", line 1, characters 0-0:
  --- sub/b.expected
  +++ sub/b.actual
  @@ -1 +1 @@
  -subexpected
  +SUBACTUAL
  [1]

An empty token lists every pending promotion, cwd-relative.

  $ dune_complete promote ""
  a.expected
  sub/b.expected

A prefix token filters the candidate list.

  $ dune_complete promote "sub/"
  sub/b.expected

  $ dune_complete promote "a"
  a.expected

A token that matches nothing yields no candidates.

  $ dune_complete promote "no-such-prefix"

CR-someday Alizter: an already-typed positional arg is still offered as
a candidate. Fixing this needs a cmdliner change to pass the already-typed
positional args to the completion func (the public API exposes only the
context value and the cursor token; positionals aren't reachable without
patching the vendored cmdliner).

  $ dune_complete promote a.expected ""
  a.expected
  sub/b.expected

Outside any workspace, completion silently produces no candidates.

  $ (cd / && dune_complete promote "")

`--build-dir` is honoured: pending promotions in a non-default build dir
are still found.

  $ dune runtest --build-dir _build2
  File "a.expected", line 1, characters 0-0:
  --- a.expected
  +++ a.actual
  @@ -1 +1 @@
  -expected
  +ACTUAL
  File "sub/b.expected", line 1, characters 0-0:
  --- sub/b.expected
  +++ sub/b.actual
  @@ -1 +1 @@
  -subexpected
  +SUBACTUAL
  [1]
  $ dune_complete promote --build-dir _build2 ""
  a.expected
  sub/b.expected

`--root` is honoured by `dune promotion apply`. The bare `dune promote` alias
uses `Common.No_build.term` and so does not accept `--root` itself; the
underlying [Common.Builder.term] does.

  $ workspace=$PWD
  $ (cd / && dune_complete "promotion" "apply" --root "$workspace" "")
  a.expected
  sub/b.expected
