FROM ocaml/opam:debian-13-ocaml-5.5
RUN opam install csexp pp re spawn uutf ppx_expect lwt
COPY --chown=opam:opam . bench-dir
WORKDIR bench-dir
