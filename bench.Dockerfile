FROM ocaml/opam:debian-12-ocaml-4.14
RUN opam depext -u patdiff.v0.15.0
RUN opam install csexp pp re spawn uutf
COPY --chown=opam:opam . bench-dir
WORKDIR bench-dir
