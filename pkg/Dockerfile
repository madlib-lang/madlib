# Should use alpine:3.13 to get ghc 8.8.4
FROM alpine:3.13 as build-stage


# LLVM START

# install llvm 9.0.1
RUN wget https://github.com/llvm/llvm-project/releases/download/llvmorg-9.0.1/llvm-9.0.1.src.tar.xz && tar xvf llvm-9.0.1.src.tar.xz

# install dependencies
RUN apk --no-cache add \
  autoconf \
  automake \
  freetype-dev \
  g++ \
  gcc \
  cmake \
  make \
  libxml2-dev \
  python2 \
  npm \
  ghc \
  cabal \
  wget \
  musl-dev \
  zlib-dev \
  zlib-static \
  ncurses-dev \
  ncurses-static \
  libc-dev \
  curl

# Configure with cmake
RUN cd llvm-9.0.1.src && mkdir build && cd build && cmake .. -G "Unix Makefiles" -DLLVM_TARGETS_TO_BUILD="X86" -DCMAKE_BUILD_TYPE=MinSizeRel

# Build and install
RUN cd llvm-9.0.1.src/build && make -j$(nproc) && make install

# LLVM END

# install dependencies
# RUN apk add --no-cache npm ghc cabal wget musl-dev zlib-dev zlib-static ncurses-dev ncurses-static curl

# install node deps
RUN npm i -g rollup@2.62.0 @rollup/plugin-node-resolve

# install stack
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN PATH="/root/.local/bin:$PATH"
# tell stack to use the global ghc, installing GHC with stack fails
RUN stack config set system-ghc --global true



# build app
COPY . .

RUN stack install alex happy
RUN stack build --jobs 1 --flag madlib:static --ghc-options='-fPIC'
# RUN stack build --jobs 1 --ghc-options='-fPIC'

RUN cp "$(stack path --dist-dir)/build/madlib/madlib" .


FROM scratch AS export-stage
COPY --from=build-stage ./madlib /
