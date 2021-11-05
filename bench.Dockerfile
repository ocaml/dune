FROM ocaml/opam:debian-10-ocaml-4.12
RUN opam depext patdiff.v0.14.0
COPY --chown=opam:opam . bench-dir
WORKDIR bench-dir
