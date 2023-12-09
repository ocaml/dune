We test how the solver chooses a solution for depopts with constraints.

  $ . ../helpers.sh
  $ mkrepo

We create a package "foo" that is the dependency of two packages "optional" and
"bar".

  $ mkpkg foo 1

The package "optional" has a constraint on a particular version of "foo". 

  $ mkpkg optional <<'EOF'
  > depends: [ "foo" {= "1"} ]
  > EOF

The package "bar" depends on "foo" and depends optionally on "optional".

  $ mkpkg bar <<'EOF'
  > depends: [ "foo" ]
  > depopts: [ "optional" ]
  > EOF

Here the solver could pick "bar" and "foo", and perphaps pick "optional",
however this is not required.

  $ solve bar
  Solution for dune.lock:
  - bar.0.0.1
  - foo.1

Since the version of "foo" is constrained by "optional", "optional" should be
excluded from the build plan if the latest version of "foo" is picked.

  $ mkpkg foo 2
  $ solve bar
  Solution for dune.lock:
  - bar.0.0.1
  - foo.2
