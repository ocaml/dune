When we don't pass [preserve_file_kind], the rules can observe the file kind
changing based on sandbox mode chosen:

  $ echo '(lang dune 1.12)' > dune-project
  $ echo text-file > text-file
  $ true > dune
  $ echo '(rule (deps text-file) (target t) (action (with-stdout-to %{target} (run dune_cmd stat kind text-file))))' >> dune

  $ dune build t --sandbox symlink
  $ cat _build/default/t
  symbolic link

  $ dune build t --sandbox none
  $ cat _build/default/t
  regular file

  $ dune build t --sandbox copy
  $ cat _build/default/t
  regular file

  $ dune build t --sandbox hardlink
  $ cat _build/default/t
  regular file

When we pass [preserve_file_kind], the file type seen by the rule is preserved:

  $ true > dune
  $ echo '(rule (target t) (deps text-file (sandbox preserve_file_kind)) (action (with-stdout-to %{target} (run dune_cmd stat kind text-file))))' >> dune
  $ dune build t --sandbox symlink
  $ cat _build/default/t
  regular file

  $ dune build t --sandbox none
  $ cat _build/default/t
  regular file

  $ dune build t --sandbox copy
  $ cat _build/default/t
  regular file

  $ dune build t --sandbox hardlink
  $ cat _build/default/t
  regular file
