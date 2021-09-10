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

Note: for now this builds a development build, see TODO in default.nix.

## Running local nginx

Go to the `production/underhood` directory, and execute

```
nginx -c nginx.conf -g 'daemon off;' -p .
```

then open `http://localhost:9000` in the browser. Make sure you have a
compatible gateway server, like `zoekt-underhood`, or this repo's (slightly
outdated) Kythe `frontend_server` running.

