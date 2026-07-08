# The dune package, built from the source tree at `src`.
#
# Returns:
#   - `default`: native build
#   - `musl-static`: x86_64 musl static cross build (nixpkgs' dune with the
#     source tree's bootstrap invoked under `--static`)
#   - `windows-static`: mingw static cross build (via `dune build -x windows`,
#     driven from the `dune-target.nix` derivation)
#   - `dune-static`: alias for `musl-static`
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

  # Static musl build: nixpkgs' own dune, pointed at this source tree and
  # bootstrapped with `--static`. Matches the setup on `main`. The
  # `dune-target.nix` cross machinery is windows-only for now — once
  # `nix-overlays` applies its cross-overlay to `pkgsCross.musl64` (it
  # currently only applies the static-overlay) we can move this over too.
  dune-static-overlay = self: super: {
    ocamlPackages = super.ocaml-ng.ocamlPackages_5_4.overrideScope (
      oself: osuper: {
        ocaml = osuper.ocaml.override {
          flambdaSupport = false;
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

  # Used by `windows-static`. The `dune_target` overlay is applied only to
  # the cross pkgs we're building against, so it never leaks into the dev
  # shells. We also widen `meta.platforms` on `ocaml` and on every
  # `buildDunePackage` derivation in that scope: nixpkgs' ocaml-side metadata
  # excludes mingw even though those packages build fine there, and widening
  # avoids the `NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM=1` escape hatch at the call
  # site.
  duneFor =
    crossPkgs:
    let
      withDuneTarget = crossPkgs.appendOverlays [
        (self: super: {
          ocamlPackages = super.ocamlPackages.overrideScope (
            oself: osuper: {
              # ocaml itself excludes mingw via `meta.platforms`. Widen it
              # so cross builds against this scope evaluate.
              ocaml = osuper.ocaml.overrideAttrs (o: {
                meta = (o.meta or { }) // {
                  platforms = lib.platforms.all;
                };
              });
              buildDunePackage =
                arg:
                let
                  widen =
                    a:
                    a
                    // {
                      meta = (a.meta or { }) // {
                        platforms = lib.platforms.all;
                      };
                    };
                in
                # nix-overlays' `buildDunePackage` accepts either an attrset
                # or a `final: attrset` function (see `cross/ocaml.nix`).
                osuper.buildDunePackage (if builtins.isFunction arg then final: widen (arg final) else widen arg);
            }
          );
        })
      ];
    in
    withDuneTarget.ocamlPackages.dune_target.overrideAttrs { src = dune-source; };

  overrideOcaml =
    ocamlOverride: crossPkgs:
    crossPkgs.appendOverlays [
      (self: super: {
        ocamlPackages = super.ocamlPackages.overrideScope (
          oself: osuper: {
            ocaml = osuper.ocaml.override ocamlOverride;
          }
        );
      })
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

  musl-static = pkgs-static.pkgsCross.musl64.ocamlPackages.dune;

  # `framePointerSupport` is forced off because mingw can't build OCaml
  windows-static = duneFor (
    overrideOcaml { framePointerSupport = false; } pkgs.pkgsCross.mingwW64Static
  );
}
