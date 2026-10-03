Native discovery reads the prepared source view, including Dune configuration
created by substitution. Lock files and extra sources override the snapshot;
patches apply to that view without mutating any of the inputs.

  $ make_lockdir
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name main) (dir .) (depends foo))
  > EOF
  $ echo '(executable (name main) (libraries foo))' > dune
  $ echo 'let () = print_endline Foo.message' > main.ml
  $ mkdir _source
  $ cat > _source/dune-project.in <<'EOF'
  > (lang dune 3.25)
  > (package (name %{name}%))
  > EOF
  $ cat > _source/dune.in <<'EOF'
  > (library (public_name %{name}%))
  > (rule
  >  (target script.ml)
  >  (deps tool)
  >  (action (with-stdout-to %{target} (run ./tool))))
  > EOF
  $ echo 'let message = "original"' > _source/foo.ml
  $ echo 'let value = "source"' > _source/version.ml.in
  $ cat > _source/tool <<'EOF'
  > #!/bin/sh
  > echo 'let value = "original script"'
  > EOF
  $ chmod +x _source/tool
  $ make_lockpkg metadata <<'EOF'
  > (version 2.0)
  > (build (run false))
  > EOF
  $ make_lockpkg foo <<EOF
  > (version 1.0)
  > (depends metadata)
  > (source (copy $PWD/_source))
  > (extra_sources (version.ml.in (copy $PWD/_version.ml.in)))
  > (build
  >  (progn
  >   (substitute dune-project.in dune-project)
  >   (substitute dune.in dune)
  >   (substitute version.ml.in version.ml)
  >   (substitute update.patch.in update.patch)
  >   (when (= %{pkg-self:version} 1.0) (patch update.patch))
  >   (when (= %{pkg-self:version} 9.0) (patch missing.patch))
  >   (run false)))
  > EOF
  $ make_lockpkg_file foo version.ml.in <<'EOF'
  > let value = "lock file"
  > EOF
  $ cat > _version.ml.in <<'EOF'
  > let value = "extra/%{version}%/%{metadata:version}%"
  > EOF
  $ make_lockpkg_file foo update.patch.in <<'EOF'
  > --- a/foo.ml
  > +++ b/foo.ml
  > @@ -1 +1 @@
  > -let message = "original"
  > +let message = "%{name}%/" ^ Version.value ^ "/" ^ Script.value
  > --- a/tool
  > +++ b/tool
  > @@ -1,2 +1,2 @@
  >  #!/bin/sh
  > -echo 'let value = "original script"'
  > +echo 'let value = "patched script"'
  > EOF

The dependency's version is metadata, not a reason to execute its recipe.
The patched script retains its executable bit.

  $ dune build main.exe
  $ _build/default/main.exe
  foo/extra/1.0/2.0/patched script
  $ test ! -e _build/default/.lockfile/pkg/metadata/.opam/metadata/target
  $ test ! -e _build/default/.lockfile/pkg/foo/.opam/foo/target
  $ test -x _build/default/.lockfile/pkg/foo/tool
  $ cat _source/foo.ml _source/version.ml.in
  let message = "original"
  let value = "source"
  $ test ! -e _source/dune-project
  $ test ! -e _build/default/.lockfile/pkg/foo/dune-project
  $ find _build/_fetch -path '*/dir/foo.ml' -exec cat {} \;
  let message = "original"

Changing an overlay invalidates the prepared source and the native build.

  $ cat > _version.ml.in <<'EOF'
  > let value = "changed/%{version}%/%{metadata:version}%"
  > EOF
  $ dune build main.exe
  $ _build/default/main.exe
  foo/changed/1.0/2.0/patched script

Transformations depending on build-produced files remain opaque. Their Dune
configuration must not be interpreted by the outer graph.

  $ mkdir _opaque
  $ echo '(' > _opaque/dune-project
  $ make_lockpkg opaque <<EOF
  > (version 1.0)
  > (source (copy $PWD/_opaque))
  > (build
  >  (progn
  >   (write-file input "opaque/%{pkg-self:version}")
  >   (substitute input output)
  >   (run cat output)))
  > EOF
  $ build_pkg opaque
  opaque/1.0

OCaml configuration scripts require a physical prepared directory. Until native
registration supplies one, keep these packages opaque rather than evaluating
scripts against the unprepared backing snapshot.

  $ mkdir _script
  $ printf '(lang dune 3.25)\n(package (name scripted))\n' > _script/dune-project
  $ echo original > _script/value
  $ cat > _script/dune <<'EOF'
  > (* -*- tuareg -*- *)
  > let () =
  >   let input = open_in "value" in
  >   let value = input_line input in
  >   close_in input;
  >   Jbuild_plugin.V1.send
  >     ("(rule (target answer) (action (write-file %{target} "
  >      ^ Printf.sprintf "%S" value ^ ")))"
  >      ^ "(install (section share) (files answer))")
  > EOF
  $ make_lockpkg scripted <<EOF
  > (version 1.0)
  > (source (copy $PWD/_script))
  > (build
  >  (progn
  >   (system "echo opaque-script-build")
  >   (system "cp value answer")
  >   (write-file scripted.install "share: [ \"answer\" ]")))
  > EOF
  $ make_lockpkg_file scripted value <<'EOF'
  > overlay
  > EOF
  $ cat >> dune <<'EOF'
  > (rule
  >  (target scripted-result)
  >  (action (with-stdout-to %{target} (cat %{pkg:scripted:share:answer}))))
  > EOF
  $ dune build scripted-result
  opaque-script-build
  $ cat _build/default/scripted-result
  overlay
  $ test -f _build/default/.lockfile/pkg/scripted/.opam/scripted/target/cookie
  $ cat _script/value
  original

Changes to the overlay still invalidate the recipe inputs.

  $ make_lockpkg_file scripted value <<'EOF'
  > changed overlay
  > EOF
  $ dune build scripted-result
  opaque-script-build
  $ cat _build/default/scripted-result
  changed overlay
