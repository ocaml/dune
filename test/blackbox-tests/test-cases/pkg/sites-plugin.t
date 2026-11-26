Test sites plugins from another package

  $ mkrepo
  $ add_mock_repo_if_needed

Make an executable using dune-site (example mostly from the manual)
  $ mkdir app
  $ cd app
  $ write_sites_plugin_app_dune 3.20 "(package app)"
  $ write_sites_plugin_app_sources
  $ mkdir plugin
  $ cat > plugin/dune-project <<EOF
  > (lang dune 3.8)
  > (using dune_site 0.1)
  > (generate_opam_files true)
  > 
  > (package
  >  (depends app)
  >  (name plugin1))
  > EOF
  $ write_sites_plugin_dune
  $ cat >> plugin/dune <<EOF
  > 
  > (rule
  >  (alias runtest)
  >  (deps (package app) (package plugin1))
  >  (action
  >   (run app)))
  > EOF
  $ write_sites_plugin_impl
  $ dune build @all 2>&1 | dune_cmd sanitize
  $ dune build @runtest 2>&1 | dune_cmd sanitize
  Registration of Plugin1
  Main app starts...
  Plugin1 is doing something...
  $ tar cf ../plugin.tar plugin
  $ cd ..
  $ tar cf app.tar app
  $ rm -rf app

Configure our fake curl to serve the tarball:

  $ echo app.tar >> fake-curls
  $ APP_PORT=1
  $ echo plugin.tar >> fake-curls
  $ PLUGIN_PORT=2

Make a package for the executable and the plugin:
  $ mkpkg app <<EOF
  > build: [
  >   [
  >     "dune"
  >     "build"
  >     "-p"
  >     name
  >   ]
  > ]
  > url {
  >  src: "http://0.0.0.0:$APP_PORT"
  >  checksum: [
  >   "md5=$(md5sum app.tar | cut -f1 -d' ')"
  >  ]
  > }
  > EOF
  $ mkpkg plugin1 <<EOF
  > depends: [
  >   "app"
  > ]
  > build: [
  >   [
  >     "dune"
  >     "build"
  >     "-p"
  >     name
  >   ]
  > ]
  > url {
  >  src: "http://0.0.0.0:$PLUGIN_PORT"
  >  checksum: [
  >   "md5=$(md5sum plugin.tar | cut -f1 -d' ')"
  >  ]
  > }
  > EOF

Make a project that uses the executable in a dune rule.
We should observe the same behaviour that when running test above.

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (package
  >  (name foo)
  >  (depends
  >   (app :with-test)
  >   (plugin1 :with-test)))
  > EOF
  $ cat > foo.ml <<EOF
  > let foo = ()
  > EOF
  $ cat > dune <<EOF
  > (library
  >  (public_name foo))
  > 
  > (rule
  >  (alias runtest)
  >  (action
  >   (run app)))
  > EOF

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - app.0.0.1
  - plugin1.0.0.1
  $ dune build @runtest 2>&1 | dune_cmd sanitize
  Main app starts...

Should have printed:
```
Registration of Plugin1
Main app starts...
Plugin1 is doing something...
```

So, what went wrong?  The findlib library is now available:

  $ dune exec ocamlfind query plugin1.plugin1_impl | censor
  $PWD/_build/_private/default/.pkg/plugin1.0.0.1-$DIGEST/target/lib/plugin1/plugin1_impl

And the files have been installed, though maybe the app/plugins directory needs
to be located under the same target directory?

  $ find _build -path '*/target/*' | sort | censor
  _build/_private/default/.pkg/app.0.0.1-$DIGEST1/target/bin
  _build/_private/default/.pkg/app.0.0.1-$DIGEST1/target/bin/app
  _build/_private/default/.pkg/app.0.0.1-$DIGEST1/target/cookie
  _build/_private/default/.pkg/app.0.0.1-$DIGEST1/target/lib
  _build/_private/default/.pkg/app.0.0.1-$DIGEST1/target/lib/app
  _build/_private/default/.pkg/app.0.0.1-$DIGEST1/target/lib/app/META
  _build/_private/default/.pkg/app.0.0.1-$DIGEST1/target/lib/app/dune-package
  _build/_private/default/.pkg/app.0.0.1-$DIGEST1/target/lib/app/register
  _build/_private/default/.pkg/app.0.0.1-$DIGEST1/target/lib/app/register/registration.cma
  _build/_private/default/.pkg/app.0.0.1-$DIGEST1/target/lib/app/register/registration.cmi
  _build/_private/default/.pkg/app.0.0.1-$DIGEST1/target/lib/app/register/registration.cmt
  _build/_private/default/.pkg/app.0.0.1-$DIGEST1/target/lib/app/register/registration.ml
  _build/_private/default/.pkg/plugin1.0.0.1-$DIGEST2/target/cookie
  _build/_private/default/.pkg/plugin1.0.0.1-$DIGEST2/target/lib
  _build/_private/default/.pkg/plugin1.0.0.1-$DIGEST2/target/lib/app
  _build/_private/default/.pkg/plugin1.0.0.1-$DIGEST2/target/lib/app/plugins
  _build/_private/default/.pkg/plugin1.0.0.1-$DIGEST2/target/lib/app/plugins/plugin1
  _build/_private/default/.pkg/plugin1.0.0.1-$DIGEST2/target/lib/app/plugins/plugin1/META
  _build/_private/default/.pkg/plugin1.0.0.1-$DIGEST2/target/lib/plugin1
  _build/_private/default/.pkg/plugin1.0.0.1-$DIGEST2/target/lib/plugin1/META
  _build/_private/default/.pkg/plugin1.0.0.1-$DIGEST2/target/lib/plugin1/dune-package
  _build/_private/default/.pkg/plugin1.0.0.1-$DIGEST2/target/lib/plugin1/opam
  _build/_private/default/.pkg/plugin1.0.0.1-$DIGEST2/target/lib/plugin1/plugin1_impl
  _build/_private/default/.pkg/plugin1.0.0.1-$DIGEST2/target/lib/plugin1/plugin1_impl/plugin1_impl.a
  _build/_private/default/.pkg/plugin1.0.0.1-$DIGEST2/target/lib/plugin1/plugin1_impl/plugin1_impl.cma
  _build/_private/default/.pkg/plugin1.0.0.1-$DIGEST2/target/lib/plugin1/plugin1_impl/plugin1_impl.cmi
  _build/_private/default/.pkg/plugin1.0.0.1-$DIGEST2/target/lib/plugin1/plugin1_impl/plugin1_impl.cmt
  _build/_private/default/.pkg/plugin1.0.0.1-$DIGEST2/target/lib/plugin1/plugin1_impl/plugin1_impl.cmx
  _build/_private/default/.pkg/plugin1.0.0.1-$DIGEST2/target/lib/plugin1/plugin1_impl/plugin1_impl.cmxa
  _build/_private/default/.pkg/plugin1.0.0.1-$DIGEST2/target/lib/plugin1/plugin1_impl/plugin1_impl.cmxs
  _build/_private/default/.pkg/plugin1.0.0.1-$DIGEST2/target/lib/plugin1/plugin1_impl/plugin1_impl.ml
