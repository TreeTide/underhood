# UnderHood

TreeTide UnderHood is a source-code browsing UI.

## Getting started

Using podman compose and the prebuilt JS distributables.

### Using a demo index

For a quick demo with some pre-baked source repos:

```
$ cd docker
$ podman compose -f docker-compose-demo.yml up

```
### Using your own Zoekt index

```
$ cd docker
$ ZOEKT_INDEX_DIR=/path/to/zoekt/indexdir/ podman compose up

```

## UI navigation and usage

Navigate to http://localhost:8080. Quick usage:

  - Click around in the filetree
  - Use top search bar just like you would for Zoekt searcher, for example
    `r:under Option f:go$`

Source code navigation (could be better, but for now):

  - Ctrl+click to find lax matches for word under cursor
  - Hover with mouse + X: same lax matches as Ctrl+click
  - Hover with mouse + C: exact-word matches
  - Select text + V: lax matches on selection
  - Select text + B: exact-word matches on selection

Matchlist abbreviations:

  - SNIP means line is same as in other hit, but file differs
  - DUP means whole file is same as for some other hit

