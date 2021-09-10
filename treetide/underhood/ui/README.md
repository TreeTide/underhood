# Underhood UI

## Development

Use `nix-shell` in this directory to get `npm` installed, or install it
in some other way. For the first time, do `npm ci` to fetch `node_modules`
at the pinned versions. Then can do `npm run start:dev`.

## Building a release

Generate nix files for npm packages using

```
node2nix -c node-default.nix -l package-lock.json -d
```

then build a source distrib by

```
nix-build -A package node-default.nix
```

and build into js using

```
nix-build -I tt=/path/to/underhood -A static_resources
cp -r result/* ../../../distrib/ui/
```

