# OxCaml-related devShells: `bootstrap-ox`, `ox-minimal`,
# `ox-minimal-trunk`, and `ox`. All consume `nix/oxcaml.nix` for the
# compiler + package-set wiring; only `ox-minimal-trunk` is impure
# (needs `--impure` because of fetchGit main).
{
  pkgs,
  makeDuneDevShell,
  INSIDE_NIX,
}:

let
  oxcaml-setup = import ../oxcaml.nix { inherit pkgs; };
in
{
  bootstrap-ox = pkgs.mkShell {
    inherit INSIDE_NIX;
    buildInputs = [
      pkgs.gnumake
      oxcaml-setup.compiler
    ];
    meta.description = ''
      Provides a minimal shell environment with OxCaml in order to
      test the bootstrapping script.
    '';
  };

  ox-minimal = makeDuneDevShell {
    includeTestDeps = false;
    packageOverrides =
      oself: osuper:
      (oxcaml-setup.packageSet oself osuper)
      // {
        ocaml = oxcaml-setup.compiler;
      };
    meta.description = ''
      Provides a minimal shell environment with OxCaml in order to
      run the OxCaml tests.
    '';
  };

  # Like ox-minimal but with the OxCaml compiler rebuilt from the
  # `main` branch. Doubles as the trunk-build smoke test:
  # `.github/workflows/revdeps-dev-build.yml` runs
  #   nix develop .#ox-minimal-trunk --impure -c true
  # which forces the trunk compiler to build while keeping the rest
  # of the closure to the same minimal set as `ox-minimal`. Requires
  # `--impure` because the fetchGit ref moves.
  ox-minimal-trunk = makeDuneDevShell {
    includeTestDeps = false;
    packageOverrides =
      oself: osuper:
      (oxcaml-setup.packageSet oself osuper)
      // {
        ocaml = oxcaml-setup.compiler.overrideAttrs (old: {
          src = builtins.fetchGit {
            url = "https://github.com/oxcaml/oxcaml.git";
            ref = "main";
          };
        });
      };
    meta.description = ''
      Like ox-minimal but with the OxCaml trunk compiler.
    '';
  };

  ox = makeDuneDevShell {
    excludeTestNativeBuildInputs = [ "opam" ];
    excludeOcamlLibs = [
      "mdx"
      "merlin"
      "ocaml-index"
      "ocaml-lsp-server"
      "odoc"
    ];
    extraBuildInputs =
      pkgs:
      [ pkgs.tree ]
      ++ (with pkgs.ocamlPackages; [
        js_of_ocaml
        js_of_ocaml-compiler
        wasm_of_ocaml-compiler
      ]);
    packageOverrides =
      oself: osuper:
      (oxcaml-setup.packageSet oself osuper)
      // {
        ocaml = oxcaml-setup.compiler;
      };
    meta.description = ''
      Provides a full shell environment with the OxCaml compiler to
      develop with Dune.
    '';
  };
}
