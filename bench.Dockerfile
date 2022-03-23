FROM ocaml/opam:debian-10-ocaml-4.12
RUN opam depext patdiff.v0.14.0
COPY --chown=opam:opam . bench-dir
WORKDIR bench-dir
RUN sed -i s/false/true/ bench/micro/dune
RUN opam exec -- make dune.exe
RUN rm -r vendor/spawn/
RUN opam install core_bench yojson ppx_yojson_conv
