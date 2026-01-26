How to Release a Binary Distribution of an Application on GitHub with Dune
==========================================================================

This guide will show you how to write a GitHub Action which builds a binary
distribution of an application for various platforms using Dune package
management, and uploads the compiled artifacts to a GitHub release.

We'll make a workflow called "Release" which will run every time a tag is pushed
to the repo on GitHub. The workflow will build the project for the targets
``x86_64-linux``, ``x86_64-macos``, and ``aarch64-macos``, and then upload a
gzipped tarball of the built artifacts to a GitHub release named after the tag.

Dune will be installed by the `setup-dune
<https://github.com/ocaml-dune/setup-dune>`_ action.

The following is the manifest for a GitHub Action workflow which releases a
binary distribution of a package named ``my_app``.

.. code:: yaml

    name: Release

    on:
      push:
        tags:
        - '*'

    jobs:
      release-unix:
        name: Release for ${{ matrix.name }}
        runs-on: ${{ matrix.os }}
        strategy:
          matrix:
            include:
              - os: macos-15-intel
                name: x86_64-macos
              - os: macos-15
                name: aarch64-macos
              - os: ubuntu-latest
                name: x86_64-linux
        steps:

          - uses: actions/checkout@v5

          - name: Install Dune
            uses: ocaml-dune/setup-dune@v0

          - name: Build the project
            run: dune build @install --release --only-packages my_app

          - name: Set environment variable to the archive name minus the file extension
            run: echo OUT_NAME=my_app-${{ github.ref_name }}-${{ matrix.name }} >> $GITHUB_ENV

          - name: Release a tarball of build outputs
            run: |
              mkdir -p "$OUT_NAME"
              cp -rlf _build/install/default/* "$OUT_NAME"
              tar czf "$OUT_NAME.tar.gz" "$OUT_NAME"

          - name: Upload artifacts
            uses: ncipollo/release-action@v1
            with:
              allowUpdates: true
              artifacts: "*.tar.gz"

With the above manifest in a file ``.github/workflows/release.yml``, whenever a
tag is pushed to the repo, gzipped tarballs will be uploaded to a new release of
the project named after the tag, in files named
``my_app-<TAG>-x86_64-linux.tar.gz``, ``my_app-<TAG>-x86_64-macos.tar.gz``, and
``my_app-<TAG>-aarch64-macos.tar.gz``.
