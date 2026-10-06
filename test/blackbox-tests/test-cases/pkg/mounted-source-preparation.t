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
  $ dune trace cat | jq -rs '
  >   [.[] | select(.cat == "log") | .args
  >    | select(.message == "Package source candidates"
  >             or (.message == "Prepare package sources" and .package == "foo"))]
  >   | group_by(.message) | .[] | "\(.[0].message) evaluated once: \(length == 1)"'
  Package source candidates evaluated once: true
  Prepare package sources evaluated once: true
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

OCaml configuration scripts read a physical prepared view, not the backing
snapshot. Their ordinary rules run natively without invoking the opaque recipe.

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
  $ cat _build/default/scripted-result
  overlay
  $ test ! -e _build/default/.lockfile/pkg/scripted/.opam/scripted/target
  $ cat _script/value
  original

Changes to the overlay invalidate script evaluation and its generated rules.

  $ make_lockpkg_file scripted value <<'EOF'
  > changed overlay
  > EOF
  $ dune build scripted-result
  $ cat _build/default/scripted-result
  changed overlay

Nested scripts also see parent paths and inputs excluded from rule generation.
The snapshot includes substitutions, patched executable permissions, deletions,
and an overlaid script, without demanding unrelated native build targets.

  $ mkdir nested-script
  $ cd nested-script
  $ make_dune_project 3.25
  $ mkdir -p _source/tools/version _source/ignored
  $ cat > _source/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name nested))
  > EOF
  $ cat > _source/dune <<'EOF'
  > (dirs :standard \ ignored)
  > (install (section share) (files (tools/version/answer as answer)))
  > (rule (target unrelated) (action (run false)))
  > EOF
  $ cat > _source/tools/version/dune <<'EOF'
  > (* -*- tuareg -*- *)
  > let () = failwith "unprepared script"
  > EOF
  $ echo 'unprepared data' > _source/ignored/value
  $ echo '%{version}%' > _source/ignored/value.in
  $ echo remove-me > _source/removed
  $ cat > _source/ignored/tool <<'EOF'
  > #!/bin/sh
  > echo original
  > EOF
  $ chmod +x _source/ignored/tool
  $ cat > _script-overlay <<'EOF'
  > (* -*- tuareg -*- *)
  > let () =
  >   let input = open_in "../../ignored/value" in
  >   let value = input_line input in
  >   close_in input;
  >   let tool =
  >     Jbuild_plugin.V1.run_and_read_lines "../../ignored/tool" |> List.hd
  >   in
  >   let answer =
  >     Printf.sprintf "%s/%s/%b" value tool (Sys.file_exists "../../removed")
  >   in
  >   Jbuild_plugin.V1.send
  >     ("(rule (target answer) (action (write-file %{target} "
  >      ^ Printf.sprintf "%S" answer ^ ")))")
  > EOF
  $ make_lockdir
  $ make_lockpkg nested <<EOF
  > (version 1.0)
  > (source (copy $PWD/_source))
  > (extra_sources (tools/version/dune (copy $PWD/_script-overlay)))
  > (build
  >  (progn
  >   (patch prepare.patch)
  >   (substitute ignored/value.in ignored/value)
  >   (run false)))
  > EOF
  $ make_lockpkg_file nested prepare.patch <<'EOF'
  > --- a/ignored/tool
  > +++ b/ignored/tool
  > @@ -1,2 +1,2 @@
  >  #!/bin/sh
  > -echo original
  > +echo patched
  > --- a/removed
  > +++ /dev/null
  > @@ -1 +0,0 @@
  > -remove-me
  > EOF
  $ cat > dune <<'EOF'
  > (rule
  >  (target result)
  >  (action (copy %{pkg:nested:share:answer} %{target})))
  > (rule (target standalone) (action (write-file %{target} ok)))
  > EOF
  $ dune build result
  $ cat _build/default/result
  1.0/patched/false
  $ test ! -e _build/default/.lockfile/pkg/nested/.opam/nested/target
  $ test ! -e _build/default/.lockfile/pkg/nested/unrelated
  $ cat _source/ignored/value _source/removed
  unprepared data
  remove-me
  $ _source/ignored/tool
  original

Changes to the original inputs invalidate evaluation too. Removing the patch
must restore the deleted file and executable from the original source view.

  $ echo 'updated/%{version}%' > _source/ignored/value.in
  $ dune build result
  $ cat _build/default/result
  updated/1.0/patched/false
  $ make_lockpkg nested <<EOF
  > (version 1.0)
  > (source (copy $PWD/_source))
  > (extra_sources (tools/version/dune (copy $PWD/_script-overlay)))
  > (build (progn (substitute ignored/value.in ignored/value) (run false)))
  > EOF
  $ dune build result
  $ cat _build/default/result
  updated/1.0/original/true
  $ rm _source/removed
  $ dune build result
  $ cat _build/default/result
  updated/1.0/original/false

Errors in generated stanzas are saved for demand, not used to select the opaque
recipe. Repairing the script makes the native rules available again.

  $ cp _script-overlay _valid-script
  $ cat > _script-overlay <<'EOF'
  > (* -*- tuareg -*- *)
  > let () = Jbuild_plugin.V1.send "(broken-stanza)"
  > EOF
  $ dune build standalone
  $ dune build result 2>error
  [1]
  $ grep '^Error:' error
  Error: Unknown constructor broken-stanza
  $ cp _valid-script _script-overlay
  $ dune build result
  $ cat _build/default/result
  updated/1.0/original/false
