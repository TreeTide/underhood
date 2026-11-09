#!/usr/bin/env bash

# Note: run build_dockers.sh first to get the images
docker run --rm -it  --network=host -e ZOEKT_UNDERHOOD_HOST=localhost underhood-nginx:local
