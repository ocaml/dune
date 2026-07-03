# The dune package, built from the source tree at `src`.
#
# Returns `default` (native build) and `dune-static` (musl64 cross build).
{
  nixpkgs,
  ocaml-overlays,
  pkgs,
  src,
}:
let
  inherit (pkgs) lib;

  dune-source = lib.cleanSourceWith {
    inherit src;
    filter = pkgs.nix-gitignore.gitignoreFilterPure (_: _: true) [
      ".git"
      (src + "/.gitignore")
    ] src;
  };

  dune-build-files =
    let
      fs = lib.fileset;
    in
    fs.intersection (fs.fromSource dune-source) (
      fs.unions [
        (src + "/bin")
        (src + "/boot")
        (src + "/configure")
        (src + "/dune-project")
        (src + "/dune-file")
        (src + "/src")
        (src + "/plugin")
        (src + "/vendor")
        (src + "/otherlibs")
        (src + "/Makefile")
        (fs.fileFilter (file: file.hasExt "opam" || file.hasExt "template") src)
      ]
    );

  dune-static-overlay = self: super: {
    ocamlPackages = super.ocaml-ng.ocamlPackages_5_4.overrideScope (
      oself: osuper: {
        ocaml = osuper.ocaml.override {
          flambdaSupport = false;
          framePointerSupport = true;
        };
        dune_3 = osuper.dune_3.overrideAttrs (a: {
          src = dune-source;
          preBuild = "ocaml boot/bootstrap.ml --static";
        });
      }
    );
  };

  pkgs-static = nixpkgs.legacyPackages.${pkgs.stdenv.hostPlatform.system}.appendOverlays [
    ocaml-overlays.overlays.default
    dune-static-overlay
  ];
in
{
  default =
    with pkgs;
    stdenv.mkDerivation {
      pname = "dune";
      version = "3.x-n/a";
      src = lib.fileset.toSource {
        root = src;
        fileset = dune-build-files;
      };
      nativeBuildInputs = with ocamlPackages; [
        ocaml
        findlib
      ];
      strictDeps = true;
      buildFlags = [ "release" ];
      dontAddPrefix = true;
      dontAddStaticConfigureFlags = true;
      configurePlatforms = [ ];
      installFlags = [
        "PREFIX=${placeholder "out"}"
        "LIBDIR=$(OCAMLFIND_DESTDIR)"
      ];
    };
  dune-static = pkgs-static.pkgsCross.musl64.ocamlPackages.dune;
}
