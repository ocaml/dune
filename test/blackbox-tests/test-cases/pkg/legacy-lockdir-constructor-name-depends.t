Legacy lock dirs store dependencies as bare atoms. Package names that match
conditional-field constructors must remain unconditional dependencies rather
than being parsed as constructors.

  $ make_lockdir
  $ make_lockpkg root <<EOF
  > (version 0.0.1)
  > (depends choice all_platforms)
  > EOF
  $ make_lockpkg choice <<EOF
  > (version 0.0.1)
  > EOF
  $ make_lockpkg all_platforms <<EOF
  > (version 0.0.1)
  > EOF

  $ build_pkg root
