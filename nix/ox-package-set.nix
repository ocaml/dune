# OxCaml package set — builds packages from the oxcaml opam repository.
{ pkgs
, lib
, oxcamlOpamRepo
,
}:

oself: osuper:
let
  pins = builtins.fromJSON (builtins.readFile ./oxcaml.json);
  packagesDir = "${oxcamlOpamRepo}/packages";
  overlay = import ./opam-overlay.nix
    {
      inherit pkgs lib packagesDir;

      virtualPackages = [
        "ocaml"
        "dune"
        "ocaml-variants"
        "ocaml-options-vanilla"
        "ocaml-base-compiler"
        "base-unix"
        "base-bigarray"
        "base-threads"
        "base-domains"
        "base-nnp"
      ]
      ++ (builtins.filter (n: lib.hasPrefix "conf-" n) (
        builtins.attrNames (builtins.readDir packagesDir)
      ));

      nonDunePackages = [
        "obuild"
        "ocamlbuild"
        "ocamlfind"
        "topkg"
        "uutf"
        "zarith"
      ];

      skipPackages = [
        "mdx"
        "odoc"
        "odoc-parser"
      ];

      nameMap = {
        "ocamlfind" = "findlib";
      };
    }
    oself
    osuper;
in
overlay // {
  # menhir is a build tool but the opam file doesn't mark it {build},
  # so opam-overlay puts it in propagatedBuildInputs instead of
  # nativeBuildInputs, making it unavailable in PATH during the build.
  js_of_ocaml-compiler = overlay.js_of_ocaml-compiler.overrideAttrs (old: {
    nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ oself.menhir ];
  });
  # conf-binaryen is skipped by opam-overlay, but wasm-merge is needed
  # while building the runtime.
  wasm_of_ocaml-compiler = overlay.wasm_of_ocaml-compiler.overrideAttrs (old: {
    nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.binaryen ];
  });
  # Apply the upstream OxCaml port to regular odoc.
  odoc-parser = osuper.odoc-parser.overrideAttrs (_: {
    inherit (pins.odoc) version;
    src = pkgs.fetchFromGitHub {
      owner = "ocaml";
      repo = "odoc";
      inherit (pins.odoc) rev;
      hash = pins.odoc.narHash;
    };
  });
  odoc =
    (osuper.odoc.override {
      odoc-parser = oself.odoc-parser;
    }).overrideAttrs
      (old: {
        patches = (old.patches or [ ]) ++ [
          (pkgs.fetchurl {
            url = lib.concatStrings [
              "https://github.com/ocaml/odoc/compare/"
              "${pins.odoc.rev}...${pins.odoc.patch.rev}.diff"
            ];
            inherit (pins.odoc.patch) hash;
          })
        ];
        postPatch =
          (old.postPatch or "")
          + "\n"
          + ''
            while IFS= read -r -d "" file; do
              substituteInPlace "$file" \
                --replace-fail "%%VERSION%%" "${pins.odoc.version}"
            done < <(grep -rlZ '%%VERSION%%' src test)
          '';
      });
}
