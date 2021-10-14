# standalone derivation, for nix-build, nix-shell, etc
{ pkgs ? import <nixpkgs> { }, opam2nix ? import ./nix/opam2nix.nix
, shell ? false }:

let opam = pkgs.callPackage ./nix { inherit opam2nix; };

in (pkgs.stdenv.mkDerivation rec {
  name = "dune";

  src = if shell then
    null
  else
    with builtins;
    filterSource (path: _:
      !elem (baseNameOf path) [
        ".git"
        "result"
        "_build"
        "nix"
        "_boot"
        "_opam"
      ]) ./.;

  buildInputs = [ opam.ocaml opam.opam.ocamlfind pkgs.gnumake ];

  buildFlags = "release";

  dontAddPrefix = true;

  installFlags =
    [ "PREFIX=${placeholder "out"}" "LIBDIR=$(OCAMLFIND_DESTDIR)" ];
}) // { resolve = opam.resolve; }
