# UnderHood
Mono-repo for TreeTide UnderHood.

Note for UnderHood: `treetide-workspace` refers to the repository root with the
bazel `WORKSPACE` file. Reason: UnderHood was forked out from the treetide
monorepo.

## Getting started quickly

### Building backend

Get a `nix-shell` in repo root. Get a coffee when you do this the first time.

Execute `bazel build -c opt //treetide/...`.

### Building frontend.

Get a `nix-shell` in `treetide/underhood/ui`.

For the first time, do `npm ci` to get `node_modules` pulled.

### Running underhood locally.

TODO(robinp): automate. In the mean time use
https://github.com/TreeTide/kythe/tree/nix (see
https://github.com/TreeTide/kythe/blob/nix/README.treetide.md) to compile
Kythe.

Once Kythe tools are compiled, TLDR of getting an index (on Kythe itself):
https://gist.github.com/robinp/a7a10db5b38630a9a0ffc805fc402500

#### Running underhood

With backend and frontend built like above, use `bazel run -c opt
//treetide/underhood/frontend_server` to start the frontend API server (from a
repo-rooted `nix-shell`, as always).

In `treetide/underhood/ui`, execute `npm run start:dev` to start up the
hot-reloading UI server.

Visit `http://localhost:9000`.

### Deploying underhood using NixOps

#### To a VirtualBox VM

First, build the backend using above instructions. This step is not yet hermetic
in the deploy process. Then get a `nix-shell` in `production`. Now you have the
`ttops` command, which is just an alias to a version-pinned `nixops`.

The `ttops create -d my-uh-vm underhood/local/vbox.ops.nix` command will set
up the deployment metadata, which you can verify with `ttops list`.

Use `ttops deploy -d my-uh-vm` to deploy a new version to the VM.

Finally execute `ttops ssh -d my-uh-vm underhood-main -L
0.0.0.0:9001:127.0.0.1:80` to tunnel the VM's web server to your port.

Use `ttops scp -d my-uh-vm ...` to copy kythe binaries and indices to the VM
for the time being. Start the kythe api server in the vm.

Bring up `frontend_server` in the VM using `systemctl start
underhood_frontend_server`, check its status using `systemctl status` and
`journalctl`.

Visit `http://localhost:9001`.

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
