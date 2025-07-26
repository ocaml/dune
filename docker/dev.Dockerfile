# little dockerfile to debug CI issues
FROM ocaml/opam
RUN sudo ln -f /usr/bin/opam-2.4 /usr/bin/opam
RUN opam init --reinit -ni
RUN mkdir -p /home/opam/dune/_boot /home/opam/dune/_build && chown opam:opam /home/opam/dune/_boot /home/opam/dune/_build
COPY Makefile Makefile
COPY .ocamlformat .ocamlformat
RUN --mount=type=cache,target=/var/cache/apt sudo apt-get update && sudo apt-get install -y pkg-config nodejs strace file
# XXX not really correct as we should copy dune's source and pin it first. But
# this docker file is mostly useful for quickly figuring out CI issues, so we
# aren't too concerned
RUN opam update && opam install . --deps-only --with-test  && rm .ocamlformat Makefile
WORKDIR /home/opam/dune

COPY docker/entrypoint.sh /usr/local/bin/
ENTRYPOINT ["entrypoint.sh"]

# Default to bash
CMD ["bash"]

USER root
