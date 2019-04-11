# UnderHood
Mono-repo for TreeTide UnderHood.

Note for UnderHood: `treetide-workspace` refers to the repository root with the
bazel `WORKSPACE` file. Reason: UnderHood was forked out from the treetide
monorepo.

## File structure

```
treetide-workspace
  |
  +- nix                -- Pinned nixpkgs versions used throughout.
  |
  +- treetide           -- Source and data root, along with build rules.
  |    |
  |    +- experimental  -- Throw-away code.
  |    |
  |    +- thirdparty   -- Checked in third party code, with licenses.
  |    |
  |    +- ...           -- Various projects, libs.
  |
  +- production         -- Files related to running services in various envs.
       |
       +- ...           -- Bringing up projects, grouped by project.
```

## Git workflow

Follow https://nvie.com/posts/a-successful-git-branching-model/.

## Nix

Most parts of the repo use Nix / Nixpkgs: Development stuff uses nix-shell to
bring dev tools quickly in scope. Bazel is sourced through nix, so bringing it
up on workstations (or build envs) is painless and reproducible. Inside Bazel,
`rules_nixpkgs` is used to bring in tools. Production stuff uses nixos configs
to describe and compose services.

Currently `nixops` is used to deploy some of these nixos systems. Note however
that the general consensus is that nixops is convoluted and buggy, which has
two implications:

 - Non-machine resources (queues, etc) might not be the best idea to manage with
   nixops.

 - Large-scale machine deployments might better be done without nixops, just
   using a thin layer upon `nix-copy-closure`. See
   http://www.haskellforall.com/2018/08/nixos-in-production.html.

You might need to define the `tt` path alias pointing to `treetide-workspace`.
This is needed so nix expressions don't need relative imports and so are easier
to move. Define it using:

```
export NIX_PATH=tt=/path/to/treetide-workspace:$NIX_PATH
```

or pass using `-I tt=...` on nix invocations.

## Build tooling

Use Bazel (http://bazel.io/) for projects if it has language support.
This includes most server-side languages.

Don't use Bazel for JS - as of early 2019, it doesn't give a good tradeoff
for smaller projects.

## Third-party code

Normally third-party code should go in treetide/thirdparty.

## Random links

- About mono-repos: http://danluu.com/monorepo/.
