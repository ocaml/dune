# parameterized derivation with dependencies injected (callPackage style)

# We generate nix derivations corresponding to the opam packages from
# opam-repository that we are interested in. We do this by passing the desired
# package names to opam2nix, which runs the opam solve and spits out a build
# plan into opam-selection.nix
# The build plan can be regenerated with $ make nix/opam-selection.nix
{ pkgs, stdenv, opam2nix, fetchFromGitHub }:
let
  strings = pkgs.lib.strings;
  ocaml = pkgs.ocaml-ng.ocamlPackages_4_13.ocaml;
  coq = fetchFromGitHub {
    owner = "coq";
    repo = "coq";
    rev = "f16b7c75bcc8651e43ec1f0c8ae6744748665213";
    sha256 = "sha256-C+rk3CMUGypbsCgbHQUgaBIzOE0jUaeQ/YHZ0GYx8aI=";
  };
  args = {
    inherit ocaml;
    selection = ./opam-selection.nix;
    src = { coq-core = coq; };
  };
  opam-selection = opam2nix.build args;
  resolve = opam2nix.resolve args ([
    # test deps
    "lwt"
    "bisect_ppx"
    "cinaps"
    "core_bench"
    "csexp"
    "js_of_ocaml"
    "js_of_ocaml-compiler"
    "mdx"
    "menhir"
    "merlin"
    "ocamlfind"
    "odoc"
    "ppx_expect"
    "ppx_inline_test"
    "ppxlib"
    "result"
    "utop"
    "ctypes"
    "${coq}/coq-core.opam"
  ]);

  coq-core = opam-selection.coq-core.overrideAttrs (super: {
    buildInputs = (super.buildInputs or [ ]) ++ [ pkgs.bash pkgs.gnused pkgs.which ];
    configurePhase = ''
      patchShebangs dev/tools/ doc/stdlib
    '';
    preInstallCheck = ''
      patchShebangs tools/
      patchShebangs test-suite/
      export OCAMLPATH=$OCAMLFIND_DESTDIR:$OCAMLPATH
    '';
  });

in {
  inherit resolve;
  inherit coq-core;
  inherit ocaml;
  opam = opam-selection;
}
