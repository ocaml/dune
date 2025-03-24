Solving for post dependencies:

  $ . ./helpers.sh
  $ mkrepo

  $ mkpkg bar

  $ mkpkg foo <<EOF
  > depends: [ "bar" {post} ]
  > EOF

We don't need bar, so we skip it

  $ solve foo
  Solution for dune.lock:
  - foo.0.0.1

  $ cat dune.lock/foo.pkg
  (version 0.0.1)

Self dependency

  $ mkpkg foo <<EOF
  > depends: [ "foo" {post} ]
  > EOF

  $ solve foo
  Solution for dune.lock:
  - foo.0.0.1

  $ cat dune.lock/foo.pkg
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

  $ cat dune.lock/foo.pkg dune.lock/bar.pkg
  (version 0.0.1)
  (version 0.0.1)
  
  (depends foo)

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

  $ cat dune.lock/foo.pkg
  (version 0.0.1)

In depopts:

  $ mkpkg foo <<EOF
  > depopts: [ "bar" {post} ]
  > EOF

  $ mkpkg bar

  $ solve foo
  Solution for dune.lock:
  - foo.0.0.1
