The interaction and order of overriding DUNE_PROFILE, --profile, and --release.

Bug #4632

  $ make_dune_project 3.13

  $ runtest() {
  > dune build $@
  > dune trace cat | jq_dune 'logs("Dune context") | .context.profile'
  > }

  $ runtest
  "Dev"

  $ runtest --release
  "Release"

  $ export DUNE_PROFILE=envvar

  $ runtest
  [
    "User_defined",
    "envvar"
  ]

  $ runtest --release
  "Release"

  $ runtest --profile cmdline
  [
    "User_defined",
    "cmdline"
  ]

Profiles selected in dune-workspace can match env stanzas from dune files. The
profile name is not defined by the env stanza; it is just selected by the
context and looked up when computing the directory environment.

  $ mkdir profile-scope
  $ cat > profile-scope/dune-project <<EOF
  > (lang dune 3.24)
  > EOF
  $ cat > profile-scope/dune <<EOF
  > (env
  >  (dbg
  >   (flags (:standard -DDBG)))
  >  (dbg-opt
  >   (flags (:standard -DDBGOPT))))
  > EOF
  $ cat > profile-scope/dune-workspace <<EOF
  > (lang dune 3.24)
  > (context (default (name dbg) (profile dbg)))
  > (context (default (name dbg-opt) (profile dbg-opt)))
  > EOF
  $ (cd profile-scope && dune printenv --field flags)
  Environment for context dbg:
    (flags (-DDBG))
  
  Environment for context dbg-opt:
    (flags (-DDBGOPT))
  

Default profile flags for projects using the current dune language.

  $ mkdir profile-defaults
  $ cat > profile-defaults/dune-project <<EOF
  > (lang dune 3.24)
  > EOF
  $ cat > profile-defaults/dune <<EOF
  > (library (name x))
  > EOF
  $ cat > profile-defaults/x.ml <<EOF
  > let x = 1
  > EOF
  $ show_env() {
  >   (cd profile-defaults && dune printenv --profile "$1" \
  >      --field flags --field ocamlc_flags --field ocamlopt_flags)
  > }

The dev profile adds development-oriented common flags. The release profile and
user-defined profiles do not add common flags. All profiles add -g to ocamlc and
ocamlopt flags by default.

  $ show_env dev
  (flags
   (-short-paths -keep-locs -warn-error +a))
  (ocamlc_flags (-g))
  (ocamlopt_flags (-g))
  $ show_env release
  (flags ())
  (ocamlc_flags (-g))
  (ocamlopt_flags (-g))
  $ show_env custom
  (flags ())
  (ocamlc_flags (-g))
  (ocamlopt_flags (-g))

The -g flags are part of the standard ocamlc/ocamlopt flags and can be replaced
by overriding those fields.

  $ mkdir profile-no-g
  $ cat > profile-no-g/dune-project <<EOF
  > (lang dune 3.24)
  > EOF
  $ cat > profile-no-g/dune <<EOF
  > (env
  >  (_
  >   (ocamlc_flags ())
  >   (ocamlopt_flags ())))
  > EOF
  $ (cd profile-no-g && dune printenv --profile release \
  >    --field ocamlc_flags --field ocamlopt_flags)
  (ocamlc_flags ())
  (ocamlopt_flags ())

Dune enables -opaque for local modules in the dev profile when the compiler
supports it. It does not enable -opaque in release or user-defined profiles, and
it does not add -O2 or -O3 optimization flags by default.

  $ show_compile_summary() {
  >   profile="$1"
  >   root="compile-$profile"
  >   mkdir "$root"
  >   cat > "$root/dune-project" <<EOF
  > (lang dune 3.24)
  > EOF
  >   cat > "$root/dune" <<EOF
  > (library (name x))
  > EOF
  >   cat > "$root/x.ml" <<EOF
  > let x = 1
  > EOF
  >   (cd "$root" && dune build x.cma --profile "$profile" >/dev/null && \
  >    dune trace cat | jq_dune -r '
  >      processes
  >      | select(.args.process_args | index("x.ml"))
  >      | .args.process_args as $args
  >      | [ "g=\($args | index("-g") != null)"
  >        , "opaque=\($args | index("-opaque") != null)"
  >        , "O2=\($args | index("-O2") != null)"
  >        , "O3=\($args | index("-O3") != null)"
  >        ]
  >      | join(" ")' \
  >    | sort -u)
  > }
  $ show_compile_summary dev
  g=true opaque=true O2=false O3=false
  $ show_compile_summary release
  g=true opaque=false O2=false O3=false
  $ show_compile_summary custom
  g=true opaque=false O2=false O3=false

The %{inline_tests} variable is disabled in release and enabled otherwise.

  $ mkdir inline-tests-profile
  $ cat > inline-tests-profile/dune-project <<EOF
  > (lang dune 3.24)
  > EOF
  $ cat > inline-tests-profile/dune <<EOF
  > (rule
  >  (target inline-tests-value)
  >  (action (with-stdout-to %{target} (echo "%{inline_tests}"))))
  > EOF
  $ show_inline_tests() {
  >   profile="$1"
  >   (cd inline-tests-profile && dune build inline-tests-value --profile "$profile" >/dev/null && \
  >    printf "%s=" "$profile" && cat _build/default/inline-tests-value)
  > }
  $ show_inline_tests dev
  dev=enabled
  $ show_inline_tests release
  release=disabled
  $ show_inline_tests custom
  custom=enabled
