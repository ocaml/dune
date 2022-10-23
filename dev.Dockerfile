# little dockerfile to debug CI issues
FROM ocaml/opam
COPY Makefile Makefile
COPY .ocamlformat .ocamlformat
RUN sudo apt-get install -y pkg-config nodejs strace file && make dev-depext
# XXX not really correct as we should copy dune's source and pin it first. But
# this docker file is mostly useful for quickly figuring out CI issues, so we
# aren't too concerned
RUN make dev-deps && rm .ocamlformat Makefile
