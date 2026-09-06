Dynamically requested dependencies must be made available in the running sandbox.
The client requests dependencies from a subdirectory, concurrently and repeatedly,
then reads an overlapping glob and an empty directory. Static inputs modified by
an earlier part of the action must not be replaced by their original contents.
A directory target requested after one of its files must not replace that file.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using action-plugin 0.1)
  > EOF
  $ mkdir inputs empty subdir
  $ cp bin/foo.exe .
  $ printf original > inputs/static.txt
  $ cat > dune <<'EOF'
  > (rule
  >  (target choice)
  >  (deps selection)
  >  (action (copy selection choice)))
  > (rule
  >  (target result)
  >  (deps inputs/static.txt (sandbox always))
  >  (action (chdir subdir (dynamic-run ../foo.exe sandbox))))
  > (rule
  >  (targets (dir tree))
  >  (action
  >   (progn
  >    (run mkdir -p tree/sub)
  >    (bash "printf first > tree/sub/first")
  >    (bash "printf second > tree/sub/second"))))
  > EOF
  $ cat > inputs/dune <<'EOF'
  > (rule
  >  (target picked.txt)
  >  (action (write-file %{target} picked)))
  > (rule
  >  (target other.txt)
  >  (action (write-file %{target} other)))
  > (rule
  >  (target unused)
  >  (action (bash "echo 'unmatched dependency was built'; exit 1")))
  > EOF

The second build should reuse the result. Changing a discovered dependency must
rerun the action with its new contents, without exposing unmatched dependencies.

  $ for mode in symlink hardlink; do
  >   echo "$mode:"
  >   printf ../inputs/picked.txt > selection
  >   if dune build result --sandbox "$mode" --build-dir "_build-$mode"; then
  >     cat "_build-$mode/default/result"
  >     dune build result --sandbox "$mode" --build-dir "_build-$mode"
  >     printf ../inputs/other.txt > selection
  >     dune build result --sandbox "$mode" --build-dir "_build-$mode"
  >     cat "_build-$mode/default/result"
  >     test ! -e "_build-$mode/default/inputs/unused" &&
  >       test "$(cat inputs/static.txt)" = original && echo 'checked isolation'
  >   else
  >     echo 'sandboxed build failed'
  >   fi
  > done 2>&1 | sed -E 's|\.sandbox/[a-f0-9]{32}|.sandbox/SANDBOX|g'
  symlink:
  File "dune", lines 5-8, characters 0-124:
  5 | (rule
  6 |  (target result)
  7 |  (deps inputs/static.txt (sandbox always))
  8 |  (action (chdir subdir (dynamic-run ../foo.exe sandbox))))
  starting sandboxed action
  No rule found for
  _build-symlink/.sandbox/SANDBOX/default/choice
  sandboxed build failed
  hardlink:
  File "dune", lines 5-8, characters 0-124:
  5 | (rule
  6 |  (target result)
  7 |  (deps inputs/static.txt (sandbox always))
  8 |  (action (chdir subdir (dynamic-run ../foo.exe sandbox))))
  starting sandboxed action
  No rule found for
  _build-hardlink/.sandbox/SANDBOX/default/choice
  sandboxed build failed
