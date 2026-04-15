Test what happens when fetched archives contain valid symlinks.

Test symlink chains: A -> B -> C where intermediate links are also symlinks.
  $ mkdir -p _src/real_dir
  $ echo "content" > _src/real_dir/file.txt
  $ ln -s real_dir _src/link_a
  $ ln -s link_a _src/link_b
  $ ln -s link_b _src/link_c

  $ tar czf _src.tar.gz _src

  $ make_lockdir
  $ make_lockpkg bar <<EOF
  > (version 0.0.1)
  > (source
  >  (fetch
  >   (url file://$PWD/_src.tar.gz)))
  > (build (run cat real_dir/file.txt))
  > EOF

The works as expected
  $ build_pkg bar
  content

  $ ls _build/_private/default/.pkg/bar.*/source | sort
  link_a
  link_b
  link_c
  real_dir

Links are transformed into directories
  $ dune_cmd stat kind _build/_private/default/.pkg/bar.*/source/link_a
  directory

And their contents are accessible
  $ ls _build/_private/default/.pkg/bar.*/source/link_b
  file.txt
