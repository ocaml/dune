  $ . ./helpers.sh
  $ mkrepo

Make a package with an extra-source field
  $ mkpkg with-extra-source <<EOF
  > extra-source "some/file" {
  >   src: "https://some-url"
  >   checksum: "sha256=8beda92f97cde6d4a55a836ca6dc9f860bb5f1a6b765b80be4594943288571cf"
  > }
  > EOF

Make a package with an extra-source field with an md5 checksum

  $ mkpkg with-extra-source-md5 <<EOF
  > extra-source "some/file" {
  >   src: "https://some-url"
  >   checksum: "md5=8beda92f97cde6d4a55a836ca6dc9f86"
  > }
  > EOF

Make a package with an extra-source field and multiple checksums

  $ mkpkg with-extra-source-multiple-checksums <<EOF
  > extra-source "some/file" {
  >   src: "https://some-url"
  >   checksum: [
  >     "sha256=8beda92f97cde6d4a55a836ca6dc9f860bb5f1a6b765b80be4594943288571cf"
  >     "md5=8beda92f97cde6d4a55a836ca6dc9f86"
  >   ]
  > }
  > EOF

  $ solve with-extra-source with-extra-source-md5 with-extra-source-multiple-checksums
  Solution for dune.lock:
  - with-extra-source.0.0.1
  - with-extra-source-md5.0.0.1
  - with-extra-source-multiple-checksums.0.0.1
  $ cat >>dune.lock/with-extra-source.pkg <<EOF
  > (source (copy $PWD/source))
  > EOF

The lockfile should contain the fetching of extra sources.

  $ cat dune.lock/with-extra-source.pkg 
  (version 0.0.1)
  
  (extra_sources
   (some/file
    (fetch
     (url https://some-url)
     (checksum
      sha256=8beda92f97cde6d4a55a836ca6dc9f860bb5f1a6b765b80be4594943288571cf))))
  (source (copy $TESTCASE_ROOT/source))


The lockfile should contain the fetching of extra sources with md5 checksums.

  $ cat dune.lock/with-extra-source-md5.pkg 
  (version 0.0.1)
  
  (extra_sources
   (some/file
    (fetch
     (url https://some-url)
     (checksum md5=8beda92f97cde6d4a55a836ca6dc9f86))))

The lockfile should contain the fetching of extra sources with the first checksum from the
list of checksums.

  $ cat dune.lock/with-extra-source-multiple-checksums.pkg 
  (version 0.0.1)
  
  (extra_sources
   (some/file
    (fetch
     (url https://some-url)
     (checksum
      sha256=8beda92f97cde6d4a55a836ca6dc9f860bb5f1a6b765b80be4594943288571cf))))
