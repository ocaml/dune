Solving for post dependencies:

  $ . ./helpers.sh
  $ mkrepo

  $ mkpkg bar

  $ barfile="bar.file"
  $ bardir="$mock_packages/bar/bar.0.0.1"
  $ mkdir -p $bardir/files/dir
  $ cat >$bardir/files/$barfile <<EOF
  > foo patch
  > EOF

  $ mkpkg foo <<EOF
  > depends: [ "bar" {post} ]
  > EOF

We don't need bar, so we skip it

  $ solve foo
  Solution for dune.lock:
  - foo.0.0.1

  $ cat ${default_lock_dir}/foo.0.0.1.pkg
  (version 0.0.1)

We should also skip any artifacts that bar references:

  $ [ -d ${default_lock_dir}/bar.files ] && ls -1 -x ${default_lock_dir}/bar.files
  [1]

Self dependency

  $ mkpkg foo <<EOF
  > depends: [ "foo" {post} ]
  > EOF

  $ solve foo
  Solution for dune.lock:
  - foo.0.0.1

  $ cat ${default_lock_dir}/foo.0.0.1.pkg
  (version 0.0.1)

Using post to break cycle:

  $ mkpkg foo <<EOF
  > depends: [ "bar" {post} ]
  > EOF

  $ mkpkg bar <<EOF
  > depends: [ "foo" ]
  > EOF

  $ solve bar
  Solution for dune.lock:
  - bar.0.0.1
  - foo.0.0.1

  $ cat ${default_lock_dir}/foo.0.0.1.pkg ${default_lock_dir}/bar.0.0.1.pkg
  (version 0.0.1)
  (version 0.0.1)
  
  (depends
   (all_platforms (foo)))

post "cycle":

  $ mkpkg foo <<EOF
  > depends: [ "bar" {post} ]
  > EOF

  $ mkpkg bar <<EOF
  > depends: [ "foo" {post} ]
  > EOF

  $ solve foo
  Solution for dune.lock:
  - foo.0.0.1

  $ cat ${default_lock_dir}/foo.0.0.1.pkg
  (version 0.0.1)

In depopts:

  $ mkpkg foo <<EOF
  > depopts: [ "bar" {post} ]
  > EOF

  $ mkpkg bar

  $ solve foo
  Solution for dune.lock:
  - foo.0.0.1
