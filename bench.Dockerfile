FROM ocaml/opam:debian-12-ocaml-4.14
RUN opam depext -u patdiff.v0.15.0
COPY --chown=opam:opam . bench-dir
WORKDIR bench-dir
