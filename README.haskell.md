# Haskell Style

Don't stress, just type whatever compiles, then use `brittany`. Might want to
make a git commit before applying inplace rewrite, just in case. Can squash it
later.

If you got a `nix-shell` from the root of `treetide-workspace`, then formatter
tools are already in scope.

```
brittany --write-mode inplace path/to/source.hs
```

Note: `brittany` won't format data decls or typeclasses. Use 4-indent in these
if possible. But again, don't stress.

Before running it, you might want to create a commit (to be squashed later) to
be safe. After the commit, run

```
git log --stat HEAD~.. --name-only --format=format:"" | grep '\.hs$' | xargs -n1 brittany --write-mode inplace
git diff
```

to see the performed changes (available as `brittanyPreviousCommit` in the
nix-shell).  If you are brave, go ahead without a commit:

```
git status --porcelain | grep '^\s*[AM]' | awk '{print $2}' | grep '\.hs$' | xargs -n1 brittany --write-mode inplace
```

which is available as `brittanyChanged` in the nix-shell.

# Linter checking

Use the handy `hlint`.

```
hlint path/to/rootdir/to/check
```

If it suggests something odd, can add it to `.hlint.yaml` to ignore. Run
`hlint path/to/rootdir --default` to omit a config that ignores all remaining
lint warning types. Merge those with the existing ones.

## Interactive lint replace mode

To do interactive inplace suggestion application with `hlint`, execute:

```
hlint path/to/rootdir --reformat --reformat-options="-i -s" some/root
```

which is available as `hlintApply` in the nix-shell. Note that not all the
suggestions can be acted upon by the reformatter. So check `hlint` output after
reformat as well.

## Ambiguous suggestions

Hlint likes to suggest `eta-reductions`, which sometimes make sense, sometimes
not (for readability). Use your judgement.

(Extend as needed.)

# Running tests with Bazel

Bazel by default caches invocation results, and only reruns if deps changed.
But for `quickcheck` / `hedgehog` this may not be what we want. In that case
use `--nocache_test_results` flag:

```
bazel test -c opt treetide/path/to:a_test --test_output=streamed --nocache_test_results
```

