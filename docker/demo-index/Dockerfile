FROM alpine AS fetcher
RUN apk add --no-cache git

WORKDIR /indexer
RUN git clone --depth=1 https://github.com/TreeTide/zoekt-underhood && \
    git clone --depth=1 https://github.com/TreeTide/underhood && \
    git clone --depth=1 https://github.com/sourcegraph/zoekt

FROM sourcegraph/zoekt-indexserver AS indexer
COPY --from=fetcher /indexer /src
RUN zoekt-git-index -index ./idx /src/zoekt-underhood /src/underhood /src/zoekt

# Change image so we can copy with root perms
FROM alpine
COPY --from=indexer /home/sourcegraph/idx /idx
COPY docker/demo-index/copy-index.sh /

ENTRYPOINT /copy-index.sh
