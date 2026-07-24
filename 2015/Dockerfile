FROM ubuntu:24.04

ENV HOME="/home/build/"

# Noninteractive frontend so tzdata doesn't block
ENV DEBIAN_FRONTEND=noninteractive

# setup buildenv
RUN apt-get update && apt-get install -y \
    gcc g++ binutils patch bzip2 make \
    unzip zlib1g-dev libc6-dev subversion libncurses5-dev gawk \
    sharutils curl libxml-parser-perl ocaml-nox ocaml zstd ocaml-findlib \
    python3-yaml libssl-dev libfdt-dev lzop coccinelle cproto \
    build-essential git autoconf ocamlbuild wget python-is-python3 \
    vim file rsync curl gcc libudev-dev libusb-1.0-0-dev \
    automake autotools-dev python3 libmpc-dev libmpfr-dev libgmp-dev \
    bison flex texinfo gperf libtool patchutils bc libexpat-dev \
    pkg-config


RUN mkdir -p /home/build && chown $USER:$USER /home/build/

WORKDIR /home/build

RUN cd /home/build && curl -sSL https://get.haskellstack.org/ | sh
RUN stack ghci

RUN cd /home/build && git clone https://github.com/moocfi/haskell-mooc.git

RUN cd /home/build/haskell-mooc/exercises/ && stack build

RUN rm /home/build/haskell-mooc -r

# Default command
CMD ["/bin/bash"]

#build:
#docker build -t hs .

#run:
#docker run --rm -it -w /home/build -v "$PWD:/home/build/my/" hs
