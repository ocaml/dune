let
  pkgs = (import <nixpkgs> { });
  opam2nix = (import ./nix/opam2nix.nix);
  local = pkgs.callPackage ./nix { inherit opam2nix; };
  inherit (pkgs) stdenv lib;

in pkgs.mkShell {
  # standard dependencies fetched from nixpkgs. essentially everything outside
  # of opam
  buildInputs = (with pkgs; ([
    coreutils
    # we prefer tools from outside our opam build plan to minimize conflicts
    ocamlformat_0_20_1
    ocaml-ng.ocamlPackages_4_13.ocaml-lsp
    git
    mercurial # for tests
    opam
    nodejs-14_x
    patdiff
    gnugrep
    gnused
    gawk
    # we can't use coq from nixpkgs because it doesn't include libraries
    # coq
    python38Packages.sphinx
    python38Packages.sphinx_rtd_theme
    # opam dependencies. the versions for these are solved for in
    # nix/opam-selection.nix
  ] ++ (if stdenv.isDarwin then [fswatch] else [])))
  ++ (with local.opam; [
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
    ctypes
  ]) ++ [ local.coq-core ];
}
