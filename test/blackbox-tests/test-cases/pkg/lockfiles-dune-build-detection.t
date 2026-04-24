Demonstrate which opam build command patterns are detected as dune builds,
resulting in "(dune)" in the lockfile rather than an explicit action.

Detection rules:
- Every command in the build field must start with "dune"
- The opam install: field must be empty (no install commands at all)
- At least one build command must be ["dune" "build" ... "-p" name ...]
  (using the opam `name` variable, not a literal)
- The opam patches: and substs: fields must be empty

  $ mkrepo

Standard modern dune opam format (dune subst + dune build with standard args):
  $ mkpkg standard-dune <<EOF
  > build: [
  >   ["dune" "subst"] {dev}
  >   ["dune" "build" "-p" name "-j" jobs "@install" "@runtest" {with-test} "@doc" {with-doc}]
  > ]
  > EOF

Without a dune subst step:
  $ mkpkg no-subst <<EOF
  > build: [
  >   ["dune" "build" "-p" name "-j" jobs "@install" "@runtest" {with-test} "@doc" {with-doc}]
  > ]
  > EOF

With dune subst using the pinned filter (pre-2.7 style):
  $ mkpkg pinned-subst <<EOF
  > build: [
  >   ["dune" "subst"] {pinned}
  >   ["dune" "build" "-p" name "-j" jobs "@install" "@runtest" {with-test} "@doc" {with-doc}]
  > ]
  > EOF

Multiple dune build steps with explicit targets (pre-1.11 style):
  $ mkpkg multi-step <<EOF
  > build: [
  >   ["dune" "build" "@install" "-p" name "-j" jobs]
  >   ["dune" "runtest" "-p" name "-j" jobs] {with-test}
  >   ["dune" "build" "@doc" "-p" name "-j" jobs] {with-doc}
  > ]
  > EOF

With dune install step in build field (from_2_9/sites format,
--promote-install-files variant):
  $ mkpkg with-sites <<EOF
  > build: [
  >   ["dune" "subst"] {dev}
  >   ["dune" "build" "-p" name "-j" jobs "--promote-install-files=false" "@install" "@runtest" {with-test} "@doc" {with-doc}]
  >   ["dune" "install" "-p" name "--create-install-files" name]
  > ]
  > EOF

Minimal dune build (no targets, no -j, no subst):
  $ mkpkg minimal <<EOF
  > build: [["dune" "build" "-p" name]]
  > EOF

Custom targets alongside standard ones:
  $ mkpkg custom-targets <<EOF
  > build: [["dune" "build" "-p" name "@install" "@myalias"]]
  > EOF

Without -j jobs argument:
  $ mkpkg no-jobs <<EOF
  > build: [["dune" "build" "-p" name "@install"]]
  > EOF

Non-dune build (configure + make):
  $ mkpkg make-build <<EOF
  > build: [["./configure" "--prefix=%{prefix}%"] [make "-j%{jobs}%"]]
  > install: [make "install"]
  > EOF

Mixed build: dune build but non-dune in build field (make in build):
  $ mkpkg mixed-build <<EOF
  > build: [["dune" "build" "-p" name] [make "install"]]
  > EOF

Only dune subst and dune runtest but no dune build:
  $ mkpkg no-dune-build <<EOF
  > build: [["dune" "subst"] {dev} ["dune" "runtest" "-p" name]]
  > EOF

dune build without -p name (missing package variable):
  $ mkpkg no-p-name <<EOF
  > build: [["dune" "build" "@install"]]
  > EOF

dune build with -p followed by a literal string (not the name variable):
  $ mkpkg p-literal <<EOF
  > build: [["dune" "build" "-p" "mypkg"]]
  > EOF

Non-empty opam install: field (even with only dune commands in build):
  $ mkpkg with-install-field <<EOF
  > build: [["dune" "build" "-p" name "-j" jobs "@install"]]
  > install: [["dune" "install" "-p" name]]
  > EOF

  $ add_mock_repo_if_needed
  $ solve standard-dune no-subst pinned-subst multi-step with-sites minimal \
  >   custom-targets no-jobs make-build mixed-build no-dune-build no-p-name \
  >   p-literal with-install-field
  Solution for dune.lock:
  - custom-targets.0.0.1
  - make-build.0.0.1
  - minimal.0.0.1
  - mixed-build.0.0.1
  - no-dune-build.0.0.1
  - no-jobs.0.0.1
  - no-p-name.0.0.1
  - no-subst.0.0.1
  - p-literal.0.0.1
  - pinned-subst.0.0.1
  - standard-dune.0.0.1
  - with-install-field.0.0.1
  - with-sites.0.0.1

Helper function to check whether a package has a (dune) build command:
  $ check_build() {
  > local pkg="$1"
  > if grep -q "(dune)" "${default_lock_dir}/${pkg}.0.0.1.pkg"; then
  >   echo "${pkg}: (dune)"
  > else
  >   echo "${pkg}: (action ...)"
  > fi
  > }

Packages that SHOULD be detected as dune builds:
  $ check_build standard-dune
  standard-dune: (dune)
  $ check_build no-subst
  no-subst: (dune)
  $ check_build pinned-subst
  pinned-subst: (dune)
  $ check_build multi-step
  multi-step: (dune)
  $ check_build with-sites
  with-sites: (dune)
  $ check_build minimal
  minimal: (dune)
  $ check_build custom-targets
  custom-targets: (dune)
  $ check_build no-jobs
  no-jobs: (dune)

Packages that should NOT be detected as dune builds:
  $ check_build make-build
  make-build: (action ...)
  $ check_build mixed-build
  mixed-build: (action ...)
  $ check_build no-dune-build
  no-dune-build: (action ...)
  $ check_build no-p-name
  no-p-name: (action ...)
  $ check_build p-literal
  p-literal: (action ...)
  $ check_build with-install-field
  with-install-field: (action ...)
