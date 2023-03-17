# Creates a monorepo out of packages in opam and builds it with dune

FROM debian

# Enable non-free packages
RUN sed -i '/^deb/ s/$/ non-free/' /etc/apt/sources.list

# Install tools and system dependencies of packages
RUN apt-get update -y && DEBIAN_FRONTEND=noninteractive apt-get install -y \
  build-essential \
  sudo \
  pkg-config \
  opam \
  wget \
  autoconf \
  zlib1g-dev \
  libcairo2-dev \
  libcurl4-gnutls-dev \
  libsnmp-dev \
  libgmp-dev \
  libbluetooth-dev \
  cmake \
  libfarmhash-dev \
  libgl-dev \
  libnlopt-dev \
  libmpfr-dev \
  r-base-core \
  libjemalloc-dev \
  libsnappy-dev \
  libpapi-dev \
  libgles2 \
  libgles2-mesa-dev \
  fswatch \
  librdkafka-dev \
  google-perftools \
  libgoogle-perftools-dev \
  libglew-dev \
  guile-3.0-dev \
  portaudio19-dev \
  libglpk-dev \
  libportmidi-dev \
  libmpg123-dev \
  libgtksourceview-3.0-dev \
  libhidapi-dev \
  libfftw3-dev \
  libasound2-dev \
  libzmq3-dev \
  r-base-dev \
  libgtk2.0-dev \
  libsoundtouch-dev \
  libmp3lame-dev \
  libplplot-dev \
  libogg-dev \
  libavutil-dev \
  libavfilter-dev \
  libswresample-dev \
  libavcodec-dev \
  libfdk-aac-dev \
  libfaad2 \
  libsamplerate0-dev \
  libao-dev \
  liblmdb-dev \
  libnl-3-dev \
  libnl-route-3-dev \
  sqlite3 \
  libsqlite3-dev \
  cargo \
  libtool \
  libopenimageio-dev \
  libtidy-dev \
  libleveldb-dev \
  libgtkspell-dev \
  libtag1-dev \
  libsrt-openssl-dev \
  liblo-dev \
  libmad0-dev \
  frei0r-plugins-dev \
  libavdevice-dev \
  libfaad-dev \
  libglfw3-dev \
  protobuf-compiler \
  libuv1-dev \
  libxen-dev \
  libflac-dev \
  libpq-dev \
  libtheora-dev \
  libonig-dev \
  libglib2.0-dev \
  libgoocanvas-2.0-dev \
  libgtkspell3-3-dev \
  libpulse-dev \
  libdlm-dev \
  capnproto \
  libtorch-dev \
  libqrencode-dev \
  libshine-dev \
  libopus-dev \
  libspeex-dev \
  libvorbis-dev \
  libgstreamer1.0-dev \
  libgstreamer-plugins-base1.0-dev \
  liblz4-dev \
  liblilv-dev \
  libopenexr-dev \
  llvm \
  libclang-dev \
  libmaxminddb-dev \
  libsecp256k1-dev \
  libstring-shellquote-perl \
  libopenblas-dev \
  qt5-qmake \
  libqt5quick5 \
  qtdeclarative5-dev \
  libgpiod-dev \
  libzstd-dev \
  ;

# create a non-root user
RUN useradd --create-home --shell /bin/bash --gid users --groups sudo user
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
ENV HOME=/home/user
USER user
WORKDIR $HOME

# set up opam
RUN opam init --disable-sandboxing --auto-setup

# make an opam switch for running benchmarks
RUN opam switch create bench 4.14.1
RUN opam install -y dune ocamlbuild

# make an opam switch for preparing the files for the benchmark
RUN opam switch create prepare 4.14.1
RUN opam install -y opam-monorepo ppx_sexp_conv ocamlfind ctypes ctypes-foreign re sexplib menhir camlp-streams zarith stdcompat refl

# Make a directory to store the monorepo benchmark project
RUN mkdir -p $HOME/monorepo-bench

# Download the monorepo benchmark and copy files into the benchmark project
ENV MONOREPO_BENCHMARK_TAG=2023-03-16.0
RUN wget https://github.com/ocaml-dune/ocaml-monorepo-benchmark/archive/refs/tags/$MONOREPO_BENCHMARK_TAG.tar.gz -O ocaml-monorepo-benchmark.tar.gz && tar xf ocaml-monorepo-benchmark.tar.gz && mv ocaml-monorepo-benchmark-$MONOREPO_BENCHMARK_TAG ocaml-monorepo-benchmark
WORKDIR $HOME/monorepo-bench
RUN mkdir -p monorepo && cp -r $HOME/ocaml-monorepo-benchmark/benchmark/dune $HOME/ocaml-monorepo-benchmark/benchmark/monorepo.ml monorepo && cp -r $HOME/ocaml-monorepo-benchmark/benchmark/monorepo-bench.opam $HOME/ocaml-monorepo-benchmark/benchmark/monorepo-bench.opam.locked $HOME/ocaml-monorepo-benchmark/benchmark/patches .

# Running `opam monorepo pull` with a large package set is very likely to fail on at least
# one package in a non-deterministic manner. Repeating it several times reduces the chance
# that all attempts fail.
RUN opam monorepo pull || opam monorepo pull || opam monorepo pull

# Initialize some projects' source code
RUN . ~/.profile && cd duniverse/clangml && ./configure
RUN cd duniverse/zelus && ./configure
RUN rm -rf duniverse/magic-trace/vendor
RUN cd duniverse/cpu && autoconf && autoheader && ./configure
RUN cd duniverse/setcore && autoconf && autoheader && ./configure
RUN cd duniverse/batsat-ocaml && ./build_rust.sh

# Some packages define conflicting definitions of libraries so they must be removed for the build to succeed
RUN rm -r duniverse/coq-of-ocaml
RUN rm -r duniverse/coq

# Bulid the dune binary that we'll be benchmarking
RUN mkdir -p $HOME/dune
WORKDIR $HOME/dune
ADD --chown=user:users . .
RUN . ~/.profile && dune build bin/main.exe --release

# Copy the remaininder of the files needed for the monorepo benchmark
WORKDIR $HOME/monorepo-bench
ADD --chown=user:users bench/monorepo .

# Apply some custom packages to some packages
RUN bash -c 'for f in patches/*; do p=$(basename ${f%.diff}); patch -p1 -d duniverse/$p < $f; done'

# Change to the benchmarking switch to run the benchmark
RUN opam switch bench
