let
  pkgs = (import <nixpkgs> { });
  local = (import ./default.nix { });
  inherit (pkgs) stdenv lib;
in with local;

pkgs.mkShell {
  inputsFrom = [
    dune
    dune-action-plugin
    dune-bench
    dune-build-info
    dune-configurator
    dune-private-libs
    dune-rpc
    dune-rpc-lwt
    dune-site
  ];

  buildInputs = (with pkgs; [
    coreutils
    # we prefer tools from outside our opam build plan to minimize conflicts
    ocamlformat_0_17_0
    git
    mercurial # for tests
    (if stdenv.isDarwin then fswatch else inotify-tools)
    opam
    nodejs-14_x
    patdiff
    gnugrep
    gnused
    gawk
    coq
  ]) ++ (with opam; [
    lwt
    bisect_ppx
    cinaps
    core_bench
    csexp
    js_of_ocaml
    js_of_ocaml-compiler
    mdx
    menhir
    merlin
    ocamlfind
    odoc
    ppx_expect
    ppx_inline_test
    ppxlib
    result
    utop
  ]);

}
