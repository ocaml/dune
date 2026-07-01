open Vcs_tests_common

let%expect_test _ =
  run Hg script;
  [%expect
    {|
$ hg init -q
$ echo "-" > a
$ hg add a
$ hg commit -m 'commit message' -u toto
$ hg describe [...]
<commit-id>

$ echo "-" > b
$ hg add b
$ hg describe [...]
<commit-id>-dirty

$ hg commit -m 'commit message' -u toto
$ hg describe [...]
<commit-id>

$ hg tag 1.0 -u toto
$ hg describe [...]
1.0

$ echo "-" > c
$ hg add c
$ hg describe [...]
1.0-dirty

$ hg commit -m 'commit message' -u toto
$ hg describe [...]
1.0-1-<commit-id>

$ echo "-" > d
$ hg add d
$ hg describe [...]
1.0-1-<commit-id>-dirty

$ hg commit -m 'commit message' -u toto
$ hg describe [...]
1.0-2-<commit-id>
|}]
;;
