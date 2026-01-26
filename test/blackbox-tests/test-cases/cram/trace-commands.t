Demonstrate command level trace events

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ cat > dune <<EOF
  > (cram
  >  (shell bash))
  > EOF

  $ cat >foo.t <<'EOF'
  >   $ echo a
  >   > echo b
  > Break
  >   $ echo c
  > EOF

  $ dune runtest foo.t
  File "foo.t", line 1, characters 0-0:
  --- foo.t
  +++ foo.t.corrected
  @@ -1,4 +1,7 @@
     $ echo a
     > echo b
  +  a
  +  b
   Break
     $ echo c
  +  c
  [1]

  $ dune trace cat | jq 'include "dune"; select(.cat == "cram") | .args | redactCommandTimes'
  {
    "test": "_build/default/foo.t",
    "commands": [
      {
        "command": [
          "echo a",
          "echo b"
        ],
        "real": "redacted",
        "user": "redacted",
        "system": "redacted"
      },
      {
        "command": [
          "echo c"
        ],
        "real": "redacted",
        "user": "redacted",
        "system": "redacted"
      }
    ]
  }

We have to override TIMEFORMAT to get this timing information:

  $ cat >timeformat.t <<'EOF'
  >   $ echo $TIMEFORMAT
  > EOF

  $ dune runtest timeformat.t
  File "timeformat.t", line 1, characters 0-0:
  --- timeformat.t
  +++ timeformat.t.corrected
  @@ -1 +1,2 @@
     $ echo $TIMEFORMAT
  +  %3R|%3S|%3U
  [1]
  $ dune promotion show timeformat.t
    $ echo $TIMEFORMAT
    %3R|%3S|%3U
  

Timeout:

  $ cat > dune <<EOF
  > (cram
  >  (timeout 0.1)
  >  (shell bash))
  > EOF

  $ cat >timeout.t <<EOF
  >    $ echo foo
  >    $ sleep 10
  > EOF

  $ dune runtest timeout.t
  File "timeout.t", line 1, characters 0-0:
  --- timeout.t
  +++ timeout.t.corrected
  @@ -1,2 +0,0 @@
  -   $ echo foo
  -   $ sleep 10
  [1]

  $ dune trace cat | jq 'include "dune"; select(.cat == "cram") | .args | redactCommandTimes'
  {
    "test": "_build/default/timeout.t",
    "commands": []
  }

  $ dune promotion show
  
