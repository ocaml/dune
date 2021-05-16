# paramaterised derivation with dependencies injected (callPackage style)

# We generate nix derivations corresponding to the opam packages from
# opam-repository that we are interested in. We do this by passing the desired
# package names to opam2nix, which runs the opam solve and spits out a build
# plan into opam-selection.nix
# The build plan can be regenerated with $ make nix/opam-selection.nix
{ pkgs, stdenv, opam2nix }:
let
  strings = pkgs.lib.strings;
  args = {
    inherit (pkgs.ocaml-ng.ocamlPackages_4_12) ocaml;
    selection = ./opam-selection.nix;
    src = builtins.filterSource (path: type:
      if type == "directory" then
        (let name = baseNameOf path;
        in name != "_boot" && name != ".git" && name != "_build" && name
        != "_opam")
      else
        true) ../.;
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
  ]);

in {
  inherit resolve;
  opam = opam-selection;
}
