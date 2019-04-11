# Deploying stuff with nixops

Nixops takes nixpkgs version from the `NIX_PATH` env var, or from the `-I`
switch. Which means it has to be modded to match the version we want (usually
our nixpkgs version).

Theoretically the version at which we build binaries/resources and the version
where we take other service dependencies from could differ, but that sounds
like a good source of confusion. So let's just pin everything to the same
version.

To use the same version for deployment as our fetch.nix:

```
NIX_PATH=nixpkgs=$(nix-instantiate --eval -E "(import $TREETIDE/nix/fetch.nix).nixpkgs" | tr -d '"') nixops ...
```

But rather get a `nix-shell` in this directory, and use `ttops` which is a
drop-in replacement for `nixops` with above path already set up.
