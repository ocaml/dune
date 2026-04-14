Sandboxing with hard links correctly resolves (follows) symbolic links.

  $ echo '(lang dune 1.12)' > dune-project
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

Now let's see what happens if the symbolic link is produced by a rule.

  $ cat > dune <<EOF
  > (rule
  >  (deps file)
  >  (targets link)
  >  (action (run ln -s file link)))
  > (rule
  >  (targets t)
  >  (deps link)
  >  (action (bash "cp %{deps} t; dune_cmd stat kind %{deps}")))
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
