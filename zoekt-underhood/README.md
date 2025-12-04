# zoekt-underhood

An Underhood server that serves source information based on [Zoekt](https://github.com/sourcegraph/zoekt) indices.

## Build

Run `scripts/build.sh` to get a local binary.

Use `podman build --tag 'zoekt-underhood:local'` to build a local image. Note:
podman will prefix short-named or missing-registry tags with `localhost/`. Use
a fully-qualified registry to avoid that if you wish.

## Related utilities

### Zoekt tooling

Install the various tooling as required, for example

```
go install github.com/sourcegraph/zoekt/cmd/zoekt-git-index@52a28e3f9e9e

```

Prefer using the version pinned by `go.mod`.
