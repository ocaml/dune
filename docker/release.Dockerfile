ARG BASE_IMAGE=debian:13
FROM ${BASE_IMAGE} AS build

RUN apt-get update && apt-get upgrade -y && apt-get install -y adduser build-essential ocaml

RUN addgroup --gid 1000 dune && adduser --uid 1000 --ingroup dune dune

USER dune:dune
WORKDIR /home/dune

# Use release assets from Docker context
RUN tar -xf dune*.tbz && mv dune-*/ dune

# Build and install dune
RUN cd dune && ./configure --prefix=/home/dune/install && make bootstrap && make release && make install

FROM ${BASE_IMAGE} AS run

RUN apt-get update && apt-get upgrade -y && apt-get install -y adduser curl ocaml git && rm -rf /var/lib/{apt,dpkg,cache,log}/
RUN addgroup --gid 1000 dune && adduser --uid 1000 --ingroup dune dune

COPY --from=build /home/dune/install/bin/dune /usr/local/bin
USER dune:dune
