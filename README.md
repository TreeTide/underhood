# UnderHood

TreeTide UnderHood is a source-code browsing UI.

## Getting started

Using podman compose and the prebuilt JS distributables (feel free to use
`docker` wherever you see `podman` if that is what you have installed).

### Using a demo index

For a quick demo which bakes and index with zoekt and underhood repos in it:

```
cd docker
podman compose -f docker-compose-demo.yml up

```
### Using your own Zoekt index

```
go install github.com/sourcegraph/zoekt/cmd/zoekt-index@latest  # or zoekt-git-index etc
~/go/bin/zoekt-index -index /tmp/indexdir my-source-dir
cd docker
ZOEKT_INDEX_DIR=/tmp/indexdir podman compose up

```

### Running without docker

Start the zoekt-underhood API server over your index dir:

```
# create your zoekt index like shown above
cd zoekt-underhood
go build cmd/zoekt-underhood/main.go
./main -index /tmp/indexdir  # by default listens on :6080

```

And start the UI frontend in dev hot-reload mode.

```
npm ci
npm run start:dev  # will listen on :9000, browse to it

```

## UI navigation and usage

Navigate to http://localhost:8082 (for the dockers) or http://localhost:9000 (for hot-reload UI mode).

Quick usage:

### Filetree

Just click around in the filetree on the left.

### Search bar

The top search bar passes on search to the Zoekt searcher, and populates the
bottom results panel (can adjust context size with the top-right dropdown).

Use the search bar just like you would for Zoekt searcher, for example `r:under
Option f:go$` restricts to repos containing 'under' and files ending in `go`,
with a string match for Option. An other useful thing is excluding noise like
`-f:test -f:node_modules` etc. Can use `b:` to restrict which branches to
search (for multi-branch indices). Study the
    [zoekt search syntax](https://github.com/sourcegraph/zoekt/blob/main/doc/query_syntax.md).

### Source code navigation

For now:

  - Ctrl+click to find lax matches for word under cursor (lax ~ ignores case, need not be separate token)
  - Hover with mouse + X: same lax matches as Ctrl+click
  - Hover with mouse + C: exact-word matches (exact ~ case match and is a standalone token)
  - Select text + V: lax matches on selection
  - Select text + B: exact-word matches on selection

Triggering these interactive searches populates the bottom result pane.

### Matchlist abbreviations

  - SNIP means line is same as in other hit, but file differs
  - DUP means whole file is same as for some other hit

