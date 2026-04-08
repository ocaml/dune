FROM ocaml/opam:debian-13-ocaml-5.4
RUN opam install csexp pp re spawn uutf
COPY --chown=opam:opam . bench-dir
WORKDIR bench-dir
