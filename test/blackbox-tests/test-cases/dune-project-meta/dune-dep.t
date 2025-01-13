Version constraint on dune deps
-------------------------------

Without the dune dependency declared in the dune-project file, we
generate a dune dependency with a constraint:

  $ cat > dune-project <<EOF
  > (lang dune 2.1)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo))
  > EOF

  $ dune build foo.opam
  $ grep -A2 ^depends: foo.opam
  depends: [
    "dune" {>= "2.1"}
  ]

With the dune dependency declared in the dune-project file and version
of the language < 2.6 we don't add the constraint:

  $ cat > dune-project <<EOF
  > (lang dune 2.5)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo) (depends dune))
  > EOF

  $ dune build foo.opam
  $ grep ^depends: foo.opam
  depends: ["dune"]

Same with version of the language >= 2.6, we now add the constraint:

  $ cat > dune-project <<EOF
  > (lang dune 2.6)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo) (depends dune))
  > EOF

  $ dune build foo.opam
  $ grep -A2 ^depends: foo.opam
  depends: [
    "dune" {>= "2.6"}
  ]

When the version of the language >= 2.7 we use dev instead of pinned
when calling dune subst:

  $ cat > dune-project <<EOF
  > (lang dune 2.7)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo))
  > EOF

  $ dune build foo.opam
  $ grep -A13 ^build: foo.opam
  build: [
    ["dune" "subst"] {dev}
    [
      "dune"
      "build"
      "-p"
      name
      "-j"
      jobs
      "@install"
      "@runtest" {with-test}
      "@doc" {with-doc}
    ]
  ]

When the version of the language >= 2.7, odoc is automatically added to
the doc dependencies:

  $ cat > dune-project <<EOF
  > (lang dune 2.7)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo))
  > EOF

  $ dune build foo.opam
  $ grep -A3 ^depends: foo.opam
  depends: [
    "dune" {>= "2.7"}
    "odoc" {with-doc}
  ]

  $ cat > dune-project <<EOF
  > (lang dune 2.7)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo) (depends something))
  > EOF

  $ dune build foo.opam
  $ grep -A4 ^depends: foo.opam
  depends: [
    "dune" {>= "2.7"}
    "something"
    "odoc" {with-doc}
  ]

  $ cat > dune-project <<EOF
  > (lang dune 2.7)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo) (depends odoc something))
  > EOF

  $ dune build foo.opam
  $ grep -A4 ^depends: foo.opam
  depends: [
    "dune" {>= "2.7"}
    "odoc"
    "something"
  ]

  $ cat > dune-project <<EOF
  > (lang dune 2.7)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo) (depends (odoc :with-doc) something))
  > EOF

  $ dune build foo.opam
  $ grep -A4 ^depends: foo.opam
  depends: [
    "dune" {>= "2.7"}
    "odoc" {with-doc}
    "something"
  ]

  $ cat > dune-project <<EOF
  > (lang dune 2.7)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo) (depends (odoc (and :with-doc (>= 1.5.0))) something))
  > EOF

  $ dune build foo.opam
  $ grep -A4 ^depends: foo.opam
  depends: [
    "dune" {>= "2.7"}
    "odoc" {with-doc & >= "1.5.0"}
    "something"
  ]

  $ cat > dune-project <<EOF
  > (lang dune 2.7)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo) (depends (odoc :with-test) something))
  > EOF

  $ dune build foo.opam
  $ grep -A5 ^depends: foo.opam
  depends: [
    "dune" {>= "2.7"}
    "odoc" {with-test}
    "something"
    "odoc" {with-doc}
  ]

  $ cat > dune-project <<EOF
  > (lang dune 2.9)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo))
  > EOF

  $ dune build foo.opam
  $ grep -A16 ^build: foo.opam
  build: [
    ["dune" "subst"] {dev}
    [
      "dune"
      "build"
      "-p"
      name
      "-j"
      jobs
      "--promote-install-files=false"
      "@install"
      "@runtest" {with-test}
      "@doc" {with-doc}
    ]
    ["dune" "install" "-p" name "--create-install-files" name]
  ]

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (name foo)
  > (generate_opam_files true)
  > (subst disabled)
  > (package (name foo) (depends (odoc :with-test) something))
  > EOF

  $ dune build foo.opam
  $ grep -A15 ^build: foo.opam
  build: [
    [
      "dune"
      "build"
      "-p"
      name
      "-j"
      jobs
      "@install"
      "@runtest" {with-test}
      "@doc" {with-doc}
    ]
  ]

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (name foo)
  > (generate_opam_files true)
  > (subst enabled)
  > (package (name foo) (depends (odoc :with-test) something))
  > EOF

  $ dune build foo.opam
  $ grep -A16 ^build: foo.opam
  build: [
    ["dune" "subst"] {dev}
    [
      "dune"
      "build"
      "-p"
      name
      "-j"
      jobs
      "@install"
      "@runtest" {with-test}
      "@doc" {with-doc}
    ]
  ]
