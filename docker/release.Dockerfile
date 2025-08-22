ARG BASE_IMAGE=debian:latest
FROM ${BASE_IMAGE}

RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y build-essential curl git sudo ocaml wget

RUN git config --global user.email "docker@example.com"
RUN git config --global user.name "Docker"

RUN addgroup --system --gid 1000 dune
RUN adduser --system --uid 1000 --ingroup dune dune
WORKDIR /home/dune

# Use release assets from Docker context
RUN tar -xvjf dune*.tbz
RUN mv dune-[0-9]*/ dune
WORKDIR /home/dune/dune

# Build and install dune
RUN ./configure --prefix=/usr/local
RUN make bootstrap
RUN make release
RUN make install

# Verify installation
RUN dune --version
