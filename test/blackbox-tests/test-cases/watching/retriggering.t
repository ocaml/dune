Modify an input file during the build so that Dune interrupts the build once.

  $ export DUNE_DEBUG_BLAH=1
  $ . ./helpers.sh

Bad rule! You are not supposed to modify the source tree. No ice-cream for you!

  $ mkdir test
  $ cd test

  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >   (deps (glob_files *.txt) (sandbox none))
  >   (alias default)
  >   (action (bash "\| echo "I'm seeing: %{deps}" >> ../../../output
  >                 "\| touch ../../new-source.txt
  > )))
  > EOF

  $ ls new-source.txt
  ls: cannot access new-source.txt: No such file or directory
  [2]

  $ touch old-source.txt
  $ start_dune

  $ inotifywait -r -m . &
  Setting up watches.  Beware: since -r was given, this may take a while!
  Watches established.
  ./ CREATE,ISDIR _build
  ./ OPEN,ISDIR _build
  ./ CLOSE_NOWRITE,CLOSE,ISDIR _build
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./_build/ CREATE,ISDIR .rpc
  ./_build/ OPEN,ISDIR .rpc
  ./_build/ CLOSE_NOWRITE,CLOSE,ISDIR .rpc
  ./_build/.sync/ CREATE 0
  ./_build/.sync/ OPEN 0
  ./_build/.sync/ CLOSE_WRITE,CLOSE 0
  ./ MODIFY dune-output
  ./_build/.sync/ DELETE 0
  ./ MODIFY dune-output
  ./_build/ CREATE .filesystem-clock
  ./_build/ OPEN .filesystem-clock
  ./_build/ MODIFY .filesystem-clock
  ./_build/ CLOSE_WRITE,CLOSE .filesystem-clock
  ./ MODIFY dune-output
  ./ OPEN,ISDIR 
  ./ CLOSE_NOWRITE,CLOSE,ISDIR 
  ./ OPEN dune-project
  ./ ACCESS dune-project
  ./ CLOSE_NOWRITE,CLOSE dune-project
  ./ MODIFY dune-output
  ./ OPEN dune
  ./ ACCESS dune
  ./ CLOSE_NOWRITE,CLOSE dune
  ./ OPEN dune
  ./ ACCESS dune
  ./ CLOSE_NOWRITE,CLOSE dune
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./_build/ MODIFY log
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./_build/ MODIFY log
  ./_build/ CREATE,ISDIR default
  ./_build/ OPEN,ISDIR default
  ./_build/ CLOSE_NOWRITE,CLOSE,ISDIR default
  ./_build/default/.dune/ CREATE configurator
  ./_build/default/.dune/ OPEN configurator
  ./_build/default/.dune/ MODIFY configurator
  ./_build/default/.dune/ CLOSE_WRITE,CLOSE configurator
  ./_build/default/.dune/ ATTRIB configurator
  ./_build/default/.dune/ OPEN configurator
  ./_build/default/.dune/ ACCESS configurator
  ./_build/default/.dune/ CLOSE_NOWRITE,CLOSE configurator
  ./_build/default/.dune/ CREATE configurator.v2
  ./_build/default/.dune/ OPEN configurator.v2
  ./_build/default/.dune/ MODIFY configurator.v2
  ./_build/default/.dune/ CLOSE_WRITE,CLOSE configurator.v2
  ./_build/default/.dune/ ATTRIB configurator.v2
  ./_build/default/.dune/ OPEN configurator.v2
  ./_build/default/.dune/ ACCESS configurator.v2
  ./_build/default/.dune/ CLOSE_NOWRITE,CLOSE configurator.v2
  ./_build/ OPEN,ISDIR default
  ./_build/default/ OPEN,ISDIR 
  ./_build/ CLOSE_NOWRITE,CLOSE,ISDIR default
  ./_build/default/ CLOSE_NOWRITE,CLOSE,ISDIR 
  ./ MODIFY dune-output
  ./ OPEN old-source.txt
  ./ CLOSE_NOWRITE,CLOSE old-source.txt
  ./ OPEN old-source.txt
  ./_build/default/ CREATE old-source.txt
  ./_build/default/ OPEN old-source.txt
  ./_build/default/ CLOSE_WRITE,CLOSE old-source.txt
  ./ CLOSE_NOWRITE,CLOSE old-source.txt
  ./_build/default/ ATTRIB old-source.txt
  ./_build/default/ OPEN old-source.txt
  ./_build/default/ CLOSE_NOWRITE,CLOSE old-source.txt
  ./_build/ CREATE,ISDIR .actions
  ./_build/ OPEN,ISDIR .actions
  ./_build/ CLOSE_NOWRITE,CLOSE,ISDIR .actions
  ./ CREATE new-source.txt
  ./ OPEN new-source.txt
  ./ ATTRIB new-source.txt
  ./ CLOSE_WRITE,CLOSE new-source.txt
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ OPEN,ISDIR 
  ./ CLOSE_NOWRITE,CLOSE,ISDIR 
  ./_build/ MODIFY log
  ./_build/.actions/default/ CREATE default-1e213338f578eb212a62b8479d5a0a6b
  ./_build/.actions/default/ OPEN default-1e213338f578eb212a62b8479d5a0a6b
  ./_build/.actions/default/ CLOSE_WRITE,CLOSE default-1e213338f578eb212a62b8479d5a0a6b
  ./_build/.actions/default/ ATTRIB default-1e213338f578eb212a62b8479d5a0a6b
  ./_build/.actions/default/ OPEN default-1e213338f578eb212a62b8479d5a0a6b
  ./_build/.actions/default/ CLOSE_NOWRITE,CLOSE default-1e213338f578eb212a62b8479d5a0a6b
  ./_build/.sync/ CREATE 1
  ./_build/.sync/ OPEN 1
  ./_build/.sync/ CLOSE_WRITE,CLOSE 1
  ./ MODIFY dune-output
  ./_build/.sync/ DELETE 1
  ./ MODIFY dune-output
  ./_build/ MODIFY .filesystem-clock
  ./_build/ OPEN .filesystem-clock
  ./_build/ MODIFY .filesystem-clock
  ./_build/ CLOSE_WRITE,CLOSE .filesystem-clock
  ./ MODIFY dune-output
  ./ OPEN dune-project
  ./ ACCESS dune-project
  ./ CLOSE_NOWRITE,CLOSE dune-project
  ./ OPEN dune
  ./ ACCESS dune
  ./ CLOSE_NOWRITE,CLOSE dune
  ./_build/default/ OPEN,ISDIR .dune
  ./_build/default/.dune/ OPEN,ISDIR 
  ./_build/default/ CLOSE_NOWRITE,CLOSE,ISDIR .dune
  ./_build/default/.dune/ CLOSE_NOWRITE,CLOSE,ISDIR 
  ./_build/ OPEN,ISDIR default
  ./_build/default/ OPEN,ISDIR 
  ./_build/ CLOSE_NOWRITE,CLOSE,ISDIR default
  ./_build/default/ CLOSE_NOWRITE,CLOSE,ISDIR 
  ./_build/.actions/ OPEN,ISDIR default
  ./_build/.actions/default/ OPEN,ISDIR 
  ./_build/.actions/ CLOSE_NOWRITE,CLOSE,ISDIR default
  ./_build/.actions/default/ CLOSE_NOWRITE,CLOSE,ISDIR 
  ./ MODIFY dune-output
  ./ OPEN new-source.txt
  ./ CLOSE_NOWRITE,CLOSE new-source.txt
  ./ OPEN new-source.txt
  ./_build/default/ CREATE new-source.txt
  ./_build/default/ OPEN new-source.txt
  ./_build/default/ CLOSE_WRITE,CLOSE new-source.txt
  ./ CLOSE_NOWRITE,CLOSE new-source.txt
  ./_build/default/ ATTRIB new-source.txt
  ./_build/default/ OPEN new-source.txt
  ./_build/default/ CLOSE_NOWRITE,CLOSE new-source.txt
  ./ MODIFY dune-output
  ./ OPEN new-source.txt
  ./ ATTRIB new-source.txt
  ./ CLOSE_WRITE,CLOSE new-source.txt
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ OPEN new-source.txt
  ./ CLOSE_NOWRITE,CLOSE new-source.txt
  ./_build/ MODIFY log
  ./_build/.actions/default/ CREATE default-40a65357c6143edf79be5c4d4c1b6dd4
  ./_build/.actions/default/ OPEN default-40a65357c6143edf79be5c4d4c1b6dd4
  ./_build/.actions/default/ CLOSE_WRITE,CLOSE default-40a65357c6143edf79be5c4d4c1b6dd4
  ./_build/.actions/default/ ATTRIB default-40a65357c6143edf79be5c4d4c1b6dd4
  ./_build/.actions/default/ OPEN default-40a65357c6143edf79be5c4d4c1b6dd4
  ./_build/.actions/default/ CLOSE_NOWRITE,CLOSE default-40a65357c6143edf79be5c4d4c1b6dd4
  ./_build/.sync/ CREATE 2
  ./_build/.sync/ OPEN 2
  ./_build/.sync/ CLOSE_WRITE,CLOSE 2
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./_build/.sync/ DELETE 2
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./ MODIFY dune-output
  ./_build/.rpc/ DELETE dune
  ./_build/ CREATE .db
  ./_build/ OPEN .db
  ./_build/ MODIFY .db
  ./_build/ CLOSE_WRITE,CLOSE .db
  ./_build/ MODIFY .filesystem-clock
  ./_build/ OPEN .filesystem-clock
  ./_build/ MODIFY .filesystem-clock
  ./_build/ CLOSE_WRITE,CLOSE .filesystem-clock
  ./_build/ CREATE .digest-db
  ./_build/ OPEN .digest-db
  ./_build/ MODIFY .digest-db
  ./_build/ CLOSE_WRITE,CLOSE .digest-db
  ./ OPEN dune-output
  ./ ACCESS dune-output
  ./ CLOSE_NOWRITE,CLOSE dune-output
  $ P=$!

  $ build .
  Success

We can see in the output below that the rule ran exactly twice. Note that the
file watcher generates the next touch event for new-source.txt but it
is ignored because the contents of new-source.txt is the same,
i.e. the empty file.

  $ cat ../output
  I'm seeing: old-source.txt
  I'm seeing: new-source.txt old-source.txt

  $ stop_dune
  XXX: watching "_build/.sync"
  XXX: watching "dune-workspace"
  XXX: watching "."
  XXX: watching "dune-workspace"
  XXX: raw inotify event: watch=1 cookie=0 events=CREATE "0"
  XXX: raw inotify event: watch=1 cookie=0 events=CLOSE_WRITE "0"
  XXX: inotify event: created _build/.sync/0
  XXX: inotify event: modified _build/.sync/0
  XXX: received sync: 0
  XXX: watching "."
  XXX: watching "dune"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/default/test/blackbox-tests/test-cases/.bin/ocamlc.opt"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/default/test/blackbox-tests/test-cases/.bin"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/default/test/blackbox-tests/test-cases/.bin/ocamlc.opt"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/default/test/blackbox-tests/test-cases/.bin/ocamlc"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/default/test/blackbox-tests/test-cases/.bin"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/default/test/blackbox-tests/test-cases/.bin/ocamlc"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/install/default/bin/ocamlc.opt"
  XXX: watching "/usr/local/home/jdimino/dune/_build/install/default/bin"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/install/default/bin/ocamlc.opt"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/install/default/bin/ocamlc"
  XXX: watching "/usr/local/home/jdimino/dune/_build/install/default/bin"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/install/default/bin/ocamlc"
  XXX: watching "/usr/local/home/jdimino/opam-root/4.12.1/bin/ocamlc.opt"
  XXX: raw inotify event: watch=1 cookie=0 events=DELETE "0"
  XXX: inotify event: unlinked _build/.sync/0
  XXX: watching "/usr/local/home/jdimino/opam-root/4.12.1/bin/ocamlopt.opt"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/default/test/blackbox-tests/test-cases/.bin/ocaml"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/default/test/blackbox-tests/test-cases/.bin"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/default/test/blackbox-tests/test-cases/.bin/ocaml"
  XXX: watching "/usr/local/home/jdimino/dune/_build/install/default/bin/ocaml"
  XXX: watching "/usr/local/home/jdimino/dune/_build/install/default/bin"
  XXX: watching "/usr/local/home/jdimino/dune/_build/install/default/bin/ocaml"
  XXX: watching "/usr/local/home/jdimino/opam-root/4.12.1/bin/ocaml"
  XXX: watching "/usr/local/home/jdimino/opam-root/4.12.1/bin/ocamldep.opt"
  XXX: watching "/usr/local/home/jdimino/opam-root/4.12.1/bin/ocamlmklib.opt"
  XXX: watching "/usr/local/home/jdimino/opam-root/4.12.1/bin/ocamlobjinfo.opt"
  XXX: watching "old-source.txt"
  XXX: raw inotify event: watch=2 cookie=0 events=CREATE "new-source.txt"
  XXX: raw inotify event: watch=2 cookie=0 events=CLOSE_WRITE "new-source.txt"
  XXX: inotify event: created ./new-source.txt
  XXX: inotify event: modified ./new-source.txt
  XXX: got fs event:
  { path = In_source_tree "new-source.txt"; kind = "Created" }
  XXX: got fs event:
  { path = In_source_tree "new-source.txt"; kind = "File_changed" }
  XXX: raw inotify event: watch=1 cookie=0 events=CREATE "1"
  XXX: raw inotify event: watch=1 cookie=0 events=CLOSE_WRITE "1"
  XXX: inotify event: created _build/.sync/1
  XXX: inotify event: modified _build/.sync/1
  XXX: received sync: 1
  XXX: watching "."
  XXX: watching "new-source.txt"
  XXX: raw inotify event: watch=1 cookie=0 events=DELETE "1"
  XXX: inotify event: unlinked _build/.sync/1
  XXX: raw inotify event: watch=2 cookie=0 events=CLOSE_WRITE "new-source.txt"
  XXX: raw inotify event: watch=13 cookie=0 events=CLOSE_WRITE
  XXX: inotify event: modified ./new-source.txt
  XXX: inotify event: modified new-source.txt
  XXX: got fs event:
  { path = In_source_tree "new-source.txt"; kind = "File_changed" }
  XXX: got fs event:
  { path = In_source_tree "new-source.txt"; kind = "File_changed" }
  XXX: raw inotify event: watch=1 cookie=0 events=CREATE "2"
  XXX: raw inotify event: watch=1 cookie=0 events=CLOSE_WRITE "2"
  XXX: inotify event: created _build/.sync/2
  XXX: inotify event: modified _build/.sync/2
  XXX: received sync: 2
  Success, waiting for filesystem changes...
  XXX: raw inotify event: watch=1 cookie=0 events=DELETE "2"
  XXX: inotify event: unlinked _build/.sync/2

  $ kill $P
  $ wait
