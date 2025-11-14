Concurrent builds with promotions maintain per-session isolation.

  $ . ./helpers.sh

  $ cat > dune-project << EOF
  > (lang dune 3.16)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target file1.gen)
  >  (action (bash "echo 'content1' > file1.gen")))
  > (rule
  >  (alias check1)
  >  (action (diff file1.expected file1.gen)))
  > (rule
  >  (target file2.gen)
  >  (action (bash "echo 'content2' > file2.gen")))
  > (rule
  >  (alias check2)
  >  (action (diff file2.expected file2.gen)))
  > (rule
  >  (target file3.gen)
  >  (action (bash "echo 'content3' > file3.gen")))
  > (rule
  >  (alias check3)
  >  (action (diff file3.expected file3.gen)))
  > EOF

  $ cat > file1.expected << EOF
  > wrong1
  > EOF

  $ cat > file2.expected << EOF
  > wrong2
  > EOF

  $ cat > file3.expected << EOF
  > wrong3
  > EOF

  $ start_dune

Trigger three concurrent builds that will fail with diffs.

  $ build @check1 > check1-output 2>&1 &
  $ CHECK1_PID=$!
  $ build @check2 > check2-output 2>&1 &
  $ CHECK2_PID=$!
  $ build @check3 > check3-output 2>&1 &
  $ CHECK3_PID=$!

  $ wait $CHECK1_PID || true
  $ wait $CHECK2_PID || true
  $ wait $CHECK3_PID || true

All three should report diffs.

  $ grep -q "content1" check1-output && echo "check1 detected diff"
  check1 detected diff
  $ grep -q "content2" check2-output && echo "check2 detected diff"
  check2 detected diff
  $ grep -q "content3" check3-output && echo "check3 detected diff"
  check3 detected diff

The promotion database should contain all three files.

  $ cat _build/.to-promote | grep -c "file[123].gen"
  3

  $ stop_dune
  Success, waiting for filesystem changes...
