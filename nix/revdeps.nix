{ nixpkgs
, ocaml-overlays
, revdeps-dune
, pkgs
,
}:
let
  inherit (pkgs) lib;

  # Overlay that replaces dune and all its subpackages with revdeps-dune source
  duneOverlay = final: prev: {
    # Override top-level dune and dune_3
    dune_3 = prev.dune_3.overrideAttrs (old: {
      src = revdeps-dune;
    });
    dune = final.dune_3;

    ocamlPackages = prev.ocaml-ng.ocamlPackages_5_4.overrideScope (
      oself: osuper:
        let
          # Helper to build dune subpackages from revdeps-dune source
          mkDuneLib =
            { pname
            , buildInputs ? [ ]
            , propagatedBuildInputs ? [ ]
            , preBuild ? ""
            ,
            }:
            osuper.buildDunePackage {
              inherit pname preBuild;
              version = "dev";
              src = revdeps-dune;
              duneVersion = "3";
              dontAddPrefix = true;
              inherit buildInputs propagatedBuildInputs;
            };
        in
        {
          ocaml = osuper.ocaml.override {
            flambdaSupport = false;
          };

          dune_3 = osuper.dune_3.overrideAttrs (old: {
            src = revdeps-dune;
          });

          # Internal dune libraries not packaged in nixpkgs
          ordering = mkDuneLib { pname = "ordering"; };
          top-closure = mkDuneLib { pname = "top-closure"; };
          fs-io = mkDuneLib { pname = "fs-io"; };

          # Override existing dune subpackages to use revdeps-dune source
          dyn = mkDuneLib {
            pname = "dyn";
            propagatedBuildInputs = with oself; [
              pp
              ordering
            ];
          };
          stdune = mkDuneLib {
            pname = "stdune";
            propagatedBuildInputs = with oself; [
              csexp
              dyn
              ordering
              pp
              top-closure
              fs-io
            ];
            preBuild = "rm -rf vendor/csexp";
          };
          dune-private-libs = mkDuneLib {
            pname = "dune-private-libs";
            propagatedBuildInputs = with oself; [ stdune ];
            preBuild = "rm -rf vendor/csexp";
          };
          dune-site = mkDuneLib {
            pname = "dune-site";
            propagatedBuildInputs = with oself; [ dune-private-libs ];
            preBuild = "rm -rf vendor/csexp";
          };
          dune-configurator = mkDuneLib {
            pname = "dune-configurator";
            propagatedBuildInputs = with oself; [ dune-private-libs ];
            preBuild = "rm -rf vendor/csexp";
          };
          dune-build-info = mkDuneLib {
            pname = "dune-build-info";
            preBuild = "rm -rf vendor/csexp";
          };
          dune-glob = mkDuneLib {
            pname = "dune-glob";
            propagatedBuildInputs = with oself; [ stdune re ];
            preBuild = "rm -rf vendor/csexp";
          };
          dune-rpc = mkDuneLib {
            pname = "dune-rpc";
            propagatedBuildInputs = with oself; [
              stdune
              dyn
              ocamlc-loc
              ordering
              pp
              xdg
              csexp
            ];
            preBuild = "rm -rf vendor/csexp";
          };
          xdg = mkDuneLib { pname = "xdg"; };
          chrome-trace = mkDuneLib {
            pname = "chrome-trace";
            propagatedBuildInputs = with oself; [ pp ];
          };
          ocamlc-loc = mkDuneLib {
            pname = "ocamlc-loc";
            propagatedBuildInputs = with oself; [ dyn ];
          };
        }
    );
  };

  # Import nixpkgs with allowBroken so deps of broken pkgs can be evaluated
  pkgsPermissive = import nixpkgs {
    inherit (pkgs.stdenv.hostPlatform) system;
    config = {
      allowBroken = true;
      allowUnfree = true;
    };
    overlays = [
      ocaml-overlays.overlays.default
      duneOverlay
    ];
  };

  # Use the filter from nix-overlays
  filter = import "${ocaml-overlays}/ci/filter.nix" {
    inherit lib;
    inherit (pkgsPermissive) stdenv;
  };

  # Get filtered candidates using nix-overlays' logic
  candidates = filter.ocamlCandidates {
    pkgs = pkgsPermissive;
    ocamlVersion = "5_4";
  };

  # Filter to only packages available on current platform
  platformCompatible = lib.filterAttrs
    (name: pkg:
      lib.meta.availableOn pkgsPermissive.stdenv.hostPlatform pkg
    )
    candidates;

  allPkgs = lib.attrValues platformCompatible;
in
# Expose the whole scope for individual builds, plus 'all' for everything
pkgsPermissive.ocamlPackages
  // {
  all = pkgsPermissive.linkFarm "all-ocaml-revdeps" (
    map
      (drv: {
        name = drv.pname or drv.name;
        path = drv;
      })
      allPkgs
  );
}
