# Rocq-related devShells. Each shell carries its own overlay,
# applied via `extraOCamlOverlays`, rather than relying on the
# universal pkgs overlay to set up the rocq packages.
{
  pkgs,
  makeDuneDevShell,
}:

{
  rocq = makeDuneDevShell {
    extraOCamlOverlays = [
      (oself: osuper: {
        rocq-core = pkgs.rocqPackages_9_2.rocq-core.override {
          customOCamlPackages = oself;
        };
        mkRocqDerivation = pkgs.rocqPackages_9_2.mkRocqDerivation.override {
          rocq-core = oself.rocq-core;
        };
        rocq-stdlib = pkgs.rocqPackages_9_2.stdlib.override {
          rocq-core = oself.rocq-core;
          mkRocqDerivation = oself.mkRocqDerivation;
        };
      })
    ];
    extraBuildInputs = pkgs: [
      pkgs.ocamlPackages.rocq-core
      pkgs.ocamlPackages.rocq-stdlib
    ];
    meta.description = ''
      Provides a minimal shell environment built purely from nixpkgs
      that can run the Rocq testsuite.
    '';
  };

  rocq-native = makeDuneDevShell {
    extraOCamlOverlays = [
      (oself: osuper: {
        rocq-core-native =
          (pkgs.rocqPackages_9_2.rocq-core.override {
            customOCamlPackages = oself;
          }).overrideAttrs
            (a: {
              configureFlags = (a.configureFlags or [ ]) ++ [
                "-native-compiler"
                "yes"
              ];
            });
        mkRocqDerivation-native = pkgs.rocqPackages_9_2.mkRocqDerivation.override {
          rocq-core = oself.rocq-core-native;
        };
        rocq-stdlib-native = pkgs.rocqPackages_9_2.stdlib.override {
          rocq-core = oself.rocq-core-native;
          mkRocqDerivation = oself.mkRocqDerivation-native;
        };
      })
    ];
    extraBuildInputs = pkgs: [
      pkgs.ocamlPackages.rocq-core-native
      pkgs.ocamlPackages.rocq-stdlib-native
    ];
    meta.description = ''
      Provides a minimal shell environment built purely from nixpkgs
      that can build Dune and run the Rocq testsuite with native compilation.
    '';
  };
}
