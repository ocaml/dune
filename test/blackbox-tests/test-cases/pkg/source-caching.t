This test demonstrates that fetching package sources should be cached

  $ make_lockdir

  $ tarball=source.tar
  $ sources="sources/"
  $ mkdir $sources; touch $sources/dummy
  $ tar cf $tarball $sources
  $ checksum=$(md5sum $tarball | awk '{ print $1 }')
  $ echo $tarball > fake-curls
  $ port=1

  $ makepkg() {
  > make_lockpkg $1 <<EOF
  > (build (run echo building $1))
  > (source
  >  (fetch
  >   (url "http://0.0.0.0:$port")
  >   (checksum md5=$checksum)))
  > (version dev)
  > EOF
  > }
  $ makepkg foo

This command is expected to download the source:
  $ build_pkg foo
  building foo

  $ wait

  $ makepkg bar

This command isn't expected to download the source. It will not be available as
the server will disappear after serving the first command.
  $ build_pkg bar 2>&1
  building bar
