If an action does not respect the dependency specification, it results in a broken
build. Dune fails to detect that:

  $ echo '(lang dune 1.12)' > dune-project
  $ true > dune
  $ echo '(rule (target a) (deps) (action (bash "echo a | tee a > b")))' >> dune
  $ echo '(rule (target b) (deps) (action (bash "echo b | tee a > b")))' >> dune
  $ echo '(rule (target c) (deps a b) (action (bash "cat a b > c")))' >> dune
  $ dune build c
  $ cat _build/default/c
  b
  b

  $ true > dune
  $ echo '(rule (target a) (deps) (action (bash "echo a > a")))' >> dune
  $ echo '(rule (target b) (deps) (action (bash "echo b > b")))' >> dune
  $ echo '(rule (target c) (deps a b) (action (bash "cat a b > c")))' >> dune
  $ dune build c
  $ cat _build/default/c
  b
  b

(it's not obvious what the correct result is on the first invocation, but the second
invocation is clearly broken (it uses a wrongly cached result))

These rules clearly depend on sandboxing. Specifying that makes the build
well-behaved:

  $ rm -rf _build
  $ true > dune
  $ echo '(rule (target a) (deps (sandbox always)) (action (bash "echo a | tee a > b")))' >> dune
  $ echo '(rule (target b) (deps (sandbox always)) (action (bash "echo b | tee a > b")))' >> dune
  $ echo '(rule (target c) (deps a b) (action (bash "cat a b > c")))' >> dune
  $ dune build c
  $ cat _build/default/c
  a
  b

Some errors:

  $ rm -rf _build
  $ true > dune
  $ echo '(rule (target a) (deps (sandbox always none)) (action (bash "echo a > a")))' >> dune
  $ dune build a
  File "dune", line 1, characters 32-43:
  1 | (rule (target a) (deps (sandbox always none)) (action (bash "echo a > a")))
                                      ^^^^^^^^^^^
  Error: Inconsistent sandboxing configuration. Sandboxing mode none is both
  allowed and disallowed
  [1]

When we don't pass [preserve_file_kind], the rules can observe the file kind
changing based on sandbox mode chosen:

  $ rm -rf _build
  $ echo text-file > text-file
  $ true > dune
  $ echo '(rule (deps text-file) (target t) (action (with-stdout-to %{target} (run file -h text-file))))' >> dune

  $ dune build t --sandbox symlink
  $ cat _build/default/t | grep -Eo 'link|ASCII'
  link

  $ dune build t --sandbox none
  $ cat _build/default/t | grep -Eo 'link|ASCII'
  ASCII

  $ dune build t --sandbox copy
  $ cat _build/default/t | grep -Eo 'link|ASCII'
  ASCII

  $ dune build t --sandbox hardlink
  $ cat _build/default/t | grep -Eo 'link|ASCII'
  ASCII

When we pass [preserve_file_kind], the file type seen by the rule is preserved:

  $ true > dune
  $ echo '(rule (target t) (deps text-file (sandbox preserve_file_kind)) (action (with-stdout-to %{target} (run file -h text-file))))' >> dune
  $ dune build t --sandbox symlink
  $ cat _build/default/t | grep -Eo 'link|ASCII'
  ASCII

  $ dune build t --sandbox none
  $ cat _build/default/t | grep -Eo 'link|ASCII'
  ASCII

  $ dune build t --sandbox copy
  $ cat _build/default/t | grep -Eo 'link|ASCII'
  ASCII

  $ dune build t --sandbox hardlink
  $ cat _build/default/t | grep -Eo 'link|ASCII'
  ASCII

If rule fails to generate targets, we give a good error message, even with sandboxing:

  $ true > dune
  $ echo '(rule (target t) (deps (sandbox always)) (action (run true)))' >> dune
  $ dune build t
  File "dune", line 1, characters 0-61:
  1 | (rule (target t) (deps (sandbox always)) (action (run true)))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Rule failed to generate the following targets:
  - t
  [1]

If rule is configured to require sandboxing, but clearly needs none, we sandbox
anyway.

  $ true > dune
  $ cat >dune <<EOF
  > (rule
  >  (target t)
  >  (deps (sandbox always))
  >  (action (write-file t "")))
  > EOF
  $ dune build t

If an action [chdir]s to a non-existing directory, it is created.

  $ echo '(lang dune 2.6)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >  (targets t)
  >  (deps (sandbox none))
  >  (action
  >  (chdir dir
  >   (progn
  >    (no-infer (write-file file hi))
  >    (write-file ../t hi)))))
  > EOF
  $ dune build t
  $ cat _build/default/dir/file
  hi
  $ cat _build/default/t
  hi

Now the same but with sandboxing. The action succeeds but the directory is not
re-created in the build directory.

  $ cat > dune <<EOF
  > (rule
  >  (targets t)
  >  (deps (sandbox always))
  >  (action
  >   (chdir dir
  >    (progn
  >     (run true)
  >     (no-infer (write-file file hi))
  >     (write-file ../t hi)))))
  > EOF
  $ dune build t
  $ cat _build/default/dir/file
  cat: _build/default/dir/file: No such file or directory
  [1]
  $ cat _build/default/t
  hi

Show errors when [chdir]ing outside of the build directory.

  $ cat > dune <<EOF
  > (rule
  >  (targets t)
  >  (deps (sandbox none))
  >  (action
  >   (chdir /dir
  >    (progn
  >     (no-infer (write-file file hi))
  >     (write-file ../t hi)))))
  > EOF
  $ dune build t
  File "dune", line 5, characters 9-13:
  5 |   (chdir /dir
               ^^^^
  Error: Directory /dir is outside the build directory. This is not allowed.
  [1]

  $ cat > dune <<EOF
  > (rule
  >  (targets t)
  >  (deps (sandbox none))
  >  (action (chdir ../../dir (progn (no-infer (write-file file hi)) (write-file ../t hi)))))
  > EOF
  $ dune build t
  File "dune", line 4, characters 16-25:
  4 |  (action (chdir ../../dir (progn (no-infer (write-file file hi)) (write-file ../t hi)))))
                      ^^^^^^^^^
  Error: Directory
  $TESTCASE_ROOT/../../dir
  is outside the build directory. This is not allowed.
  [1]

  $ cat > dune <<EOF
  > (rule
  >  (targets t)
  >  (deps (sandbox none))
  >  (action
  >   (chdir ../dir
  >    (progn
  >     (no-infer (write-file file hi))
  >     (write-file ../t hi)))))
  > EOF
  $ dune build t
  File "dune", line 5, characters 9-15:
  5 |   (chdir ../dir
               ^^^^^^
  Error: Directory
  $TESTCASE_ROOT/../dir
  is outside the build directory. This is not allowed.
  [1]

Sandboxing with hard links correctly resolves (follows) symbolic links.

  $ echo hi > file
  $ ln -s file link

  $ cat > dune <<EOF
  > (rule
  >  (targets t)
  >  (deps link)
  >  (action (bash "cp %{deps} t; dune_cmd stat kind %{deps}")))
  > EOF

The action observed [link] as a regular file with the right contents.

  $ dune build t --sandbox hardlink
  regular file
  $ cat _build/default/t
  hi

# CR-someday amokhov: Apparently, Dune turns [link] into a regular file when
# copying it into the build directory. This seems wrong. Fix this or document
# why this is the right thing to do.

  $ dune_cmd stat kind _build/default/link
  regular file

Now let's test what happens with external files.

  $ mkdir test; cd test
  $ echo '(lang dune 2.9)' > dune-project
  $ echo '(lang dune 2.9)' > dune-workspace

Now [../link] is an external file from the point of view of Dune, so it will not
be copied to the build directory.

  $ cat > dune <<EOF
  > (rule
  >   (targets t)
  >   (deps ../link)
  >   (action (bash "cp %{deps} t; dune_cmd stat kind %{deps}")))
  > EOF

# CR-someday amokhov: The action observe a symlink. It should not be the case
# since the symlink will surely not point to the right place.
  $ cat ../link
  hi
  $ dune build t --sandbox hardlink
  symbolic link

Using the absolute path [$PWD/../link].
  $ cat > dune <<EOF
  > (rule
  >   (targets t)
  >   (deps $PWD/../link)
  >   (action (bash "cp %{deps} t; dune_cmd stat kind %{deps}")))
  > EOF

Sandboxed actions can observe external dependencies as symbolic links.

  $ dune build t --sandbox hardlink
  $ cat _build/default/t
  hi

Now let's see what happens if the the symbolic link is produced by a rule.

  $ cat > dune <<EOF
  > (rule
  >  (deps file)
  >  (targets link)
  >  (action (run ln -s file link)))
  > (rule
  >  (targets t)
  >  (deps link)
  >  (action (system "cp %{deps} t; dune_cmd stat kind %{deps}")))
  > EOF

  $ echo hi again > file

The action observed [link] as a regular file with the right contents.

  $ dune build t --sandbox hardlink
  regular file
  $ cat _build/default/t
  hi again

Test that [link] is in fact a symbolic link.

  $ dune_cmd stat kind _build/default/link
  symbolic link


Test how Dune handles a loop with symbolic links.

  $ cat > dune <<EOF
  > (rule
  >  (targets link)
  >  (action (bash "ln -s link link")))
  > (rule
  >  (targets t)
  >  (deps link)
  >  (action (bash "cp %{deps} t; dune_cmd stat kind %{deps}")))
  > EOF

This loop is caught immediately after running the rule that creates it.

  $ dune build t --sandbox hardlink
  File "dune", lines 1-3, characters 0-57:
  1 | (rule
  2 |  (targets link)
  3 |  (action (bash "ln -s link link")))
  Error: Error trying to read targets after a rule was run:
  - link: Cyclic symbolic link
  [1]

Let's try to create it in another way. We'll link to the external [../link],
which is circular.

  $ cat > dune <<EOF
  > (rule
  >  (deps $PWD/../link)
  >  (targets link)
  >  (action (run ln -s $PWD/../link link)))
  > (rule
  >  (targets t)
  >  (deps link)
  >  (action (system "cp %{deps} t; dune_cmd stat kind %{deps}")))
  > EOF

  $ (cd ..; rm link; ln -s link link)

Still doesn't work: now the loop is detected when running [stat] on the
dependency.

  $ dune build t --sandbox hardlink 2>&1 | grep -v "lines 1"
  1 | (rule
  2 |  (deps $TESTCASE_ROOT/test/../link)
  3 |  (targets link)
  4 |  (action (run ln -s $TESTCASE_ROOT/test/../link link)))
  Error: File unavailable:
  $TESTCASE_ROOT/test/../link
  Cyclic symbolic link

So, it seems like we must play dirty to create a symbolic link loop.

  $ cat > dune <<EOF
  > (rule
  >  (targets link)
  >  (action (run ln -s $PWD/../link link)))
  > (rule
  >  (deps link (sandbox none))
  >  (targets dirty-rule)
  >  (action (system "(cd $PWD/..; rm link; ln -s link link); touch dirty-rule")))
  > (rule
  >  (targets t)
  >  (deps link dirty-rule)
  >  (action (system "cp %{deps} t; dune_cmd stat kind %{deps}")))
  > EOF

  $ (cd ..; rm link; ln -s file link)

Finally, we get to see the error message printed out at sandbox creation.

  $ dune build t --sandbox hardlink
  File "dune", lines 8-11, characters 0-105:
   8 | (rule
   9 |  (targets t)
  10 |  (deps link dirty-rule)
  11 |  (action (system "cp %{deps} t; dune_cmd stat kind %{deps}")))
  Error: Sandbox creation error: cannot resolve symbolic link
  "_build/default/link".
  Reason: Too many indirections; is this a cyclic symbolic link?
  [1]
