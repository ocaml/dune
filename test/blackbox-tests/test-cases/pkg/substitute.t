The test-source folder has a file to use substitution on.

  $ . ./helpers.sh

  $ mkdir test-source
  $ cat >test-source/foo.ml.in <<EOF
  > This file will be fed to the substitution mechanism
  > EOF
  $ make_lockdir
  $ cat >dune.lock/test.pkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/test-source))
  > (build
  >  (progn
  >   (substitute foo.ml.in foo.ml)
  >   (system "cat foo.ml")))
  > EOF

This should take the `foo.ml.in`, do the substitutions and create `foo.ml`:

  $ build_pkg test
  This file will be fed to the substitution mechanism

Demonstrate that the original sources aren't modified:

  $ src=_build/_private/default/.pkg/test/source/foo.ml; [ -e $src ] && cat $src
  [1]

This should also work with any other filename combination:

  $ cat >test-source/foo.ml.template <<EOF
  > This is using a different file suffix
  > EOF
  $ cat >dune.lock/test.pkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/test-source))
  > (build
  >  (progn
  >   (substitute foo.ml.template not-a-prefix)
  >   (system "cat not-a-prefix")))
  > EOF

This should take the `foo.ml.template`, do the substitution and create
`foo.ml`, thus be more flexible that the OPAM `substs` field:

  $ build_pkg test
  This is using a different file suffix

Undefined variables, how do they substitute?

  $ cat >test-source/variables.ml.in <<EOF
  > We substitute this '%%{var}%%' into '%{var}%'
  > EOF
  $ cat >dune.lock/test.pkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/test-source))
  > (build
  >  (progn
  >   (substitute variables.ml.in variables.ml)
  >   (system "cat variables.ml")))
  > EOF
  $ build_pkg test
  We substitute this '%{var}%' into ''

Now with variables set

  $ cat >test-source/defined.ml.in <<EOF
  > We substitute '%%{name}%%' into '%{name}%' and '%%{_:name}%%' into '%{_:name}%'
  > And '%%{version}%%' is set to '%{version}%'
  > There is also some paths set:
  > %%{lib}%% is '%{lib}%'
  > %%{libexec}%% is '%{libexec}%'
  > %%{bin}%% is '%{bin}%'
  > %%{sbin}%% is '%{sbin}%'
  > %%{toplevel}%% is '%{toplevel}%'
  > %%{share}%% is '%{share}%'
  > %%{etc}%% is '%{etc}%'
  > %%{doc}%% is '%{doc}%'
  > %%{stublibs}%% is '%{stublibs}%'
  > %%{man}%% is '%{man}%'
  > %%{with-test}%% is '%{with-test}%'
  > %%{os}%% is '%{os}%'
  > EOF
  $ cat >dune.lock/test.pkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/test-source))
  > (build
  >  (progn
  >   (substitute defined.ml.in defined.ml)
  >   (system "cat defined.ml")))
  > EOF
  $ build_pkg test 2>&1 | sed 's/%{os}% is.*/%{os}% is $REDACTED/g'
  We substitute '%{name}%' into 'test' and '%{_:name}%' into 'test'
  And '%{version}%' is set to '0.0.1'
  There is also some paths set:
  %{lib}% is '../target/lib'
  %{libexec}% is '../target/lib'
  %{bin}% is '../target/bin'
  %{sbin}% is '../target/sbin'
  %{toplevel}% is '../target/lib/toplevel'
  %{share}% is '../target/share'
  %{etc}% is '../target/etc'
  %{doc}% is '../target/doc'
  %{stublibs}% is '../target/lib/stublibs'
  %{man}% is '../target/man'
  %{with-test}% is ''
  %{os}% is $REDACTED

It is also possible to use variables of your dependencies:

  $ mkdir dependency-source
  $ cat >dune.lock/dependency.pkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/dependency-source))
  > EOF
  $ cat >dune.lock/test.pkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/test-source))
  > (depends dependency)
  > (build
  >  (progn
  >   (substitute dependencies.ml.in dependencies.ml)
  >   (system "cat dependencies.ml")))
  > EOF
  $ cat >test-source/dependencies.ml.in <<EOF
  > There is also some paths set:
  > '%%{dependency:lib}%%' is '%{dependency:lib}%'
  > EOF
  $ build_pkg test
  There is also some paths set:
  '%{dependency:lib}%' is '../../dependency/target/lib/dependency'
