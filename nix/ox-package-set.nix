# OxCaml package set — builds packages from the oxcaml opam repository.
{ pkgs
, lib
, oxcamlOpamRepo
,
}:

oself: osuper:
let
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
        "zarith"
      ];

      skipPackages = [
        "angstrom"
        "faraday"
        "mdx"
        "odoc"
        "odoc-parser"
        "uutf"
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
  # CR-soon Alizter: binaryen-bin is a conf package (skipped by opam-overlay)
  # that provides wasm-merge, needed at build time. Consider adding conf
  # package mapping to opam-overlay.
  #
  # runtime/wasm/args.ml derives module names from %{deps} via
  # Filename.chop_suffix on the raw path string. When dune emits same-
  # directory paths with a leading "./" this yields module names like
  # "./array" instead of "array", and the wasm linker fails to resolve
  # primitives. Patch routes through Filename.basename first. Drop once
  # the fix lands upstream in js_of_ocaml and the OxCaml opam-repository
  # pin updates.
  wasm_of_ocaml-compiler = overlay.wasm_of_ocaml-compiler.overrideAttrs (old: {
    nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.binaryen ];
    patches = (old.patches or [ ]) ++ [ ./patches/wasm_of_ocaml-args-basename.patch ];
  });
}
