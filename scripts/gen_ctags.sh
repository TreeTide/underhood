#!/usr/bin/env bash

set -euo pipefail

THISDIR=$(dirname "$0")
REPO=$(realpath $THISDIR/..)
ctags --options=$THISDIR/vue.ctags --exclude="@$REPO/.gitignore" --exclude="@$REPO/.ctagsignore" -R $REPO
