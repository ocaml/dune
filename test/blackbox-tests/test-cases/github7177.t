dune formats (pps ppx1 -- --flag true) in new lines when the line is too long
and it breaks on build

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name bug)
  >  (libraries
  >    bs_css
  >    bs_css_emotion
  >    reason_react
  >    webapi)
  >  (preprocess
  >  (pps
  >    bs-react-intl-ppx.lib
  >    bs-emotion-ppx.lib
  >    styled-ppx.lib -- --compat-with-bs-emotion-ppx true)))
  > EOF

  $ dune build @fmt --auto-promote
  File "dune", line 1, characters 0-0:
  Error: Files _build/default/dune and _build/default/.formatted/dune differ.
  Promoting _build/default/.formatted/dune to dune.
  [1]

  $ cat dune
  (library
   (name bug)
   (libraries bs_css bs_css_emotion reason_react webapi)
   (preprocess
    (pps
     bs-react-intl-ppx.lib
     bs-emotion-ppx.lib
     styled-ppx.lib
     --
     --compat-with-bs-emotion-ppx
     true)))
