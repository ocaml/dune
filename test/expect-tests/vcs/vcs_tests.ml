open Vcs_tests_common

let%expect_test _ =
  run Git script;
  [%expect
    {|
$ git init -q
$ git config user.email dune@dune.com
$ git config user.name 'Dune Dune'
$ echo "-" > a
$ git add a
$ git commit -m 'commit message'
$ git describe [...]
<commit-id>

$ echo "-" > b
$ git add b
$ git describe [...]
<commit-id>-dirty

$ git commit -m 'commit message'
$ git describe [...]
<commit-id>

$ git tag -a 1.0 -m 1.0
$ git describe [...]
1.0

$ echo "-" > c
$ git add c
$ git describe [...]
1.0-dirty

$ git commit -m 'commit message'
$ git describe [...]
1.0-1-<commit-id>

$ echo "-" > d
$ git add d
$ git describe [...]
1.0-1-<commit-id>-dirty

$ git commit -m 'commit message'
$ git describe [...]
1.0-2-<commit-id>
|}]
;;
