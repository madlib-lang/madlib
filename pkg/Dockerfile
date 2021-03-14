# Should use alpine:3.13 to get ghc 8.8.4
FROM alpine:3.13 as build-stage

# install dependencies
RUN apk add --no-cache npm ghc cabal wget musl-dev zlib-dev zlib-static ncurses-dev ncurses-static curl

# install node deps
RUN npm i -g rollup @rollup/plugin-node-resolve

# install stack
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN PATH="/root/.local/bin:$PATH"
# tell stack to use the global ghc, installing GHC with stack fails
RUN stack config set system-ghc --global true



# build app
COPY . .

RUN stack install alex happy
RUN stack build --jobs 1 --flag madlib:static --ghc-options='-fPIC'

RUN cp "$(stack path --dist-dir)/build/madlib/madlib" .


FROM scratch AS export-stage
COPY --from=build-stage ./madlib /
