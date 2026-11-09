#!/usr/bin/env bash

podman image ls | grep ghcr.io/treetide | awk '{print $3}' | xargs podman image rm --force
podman volume rm --force zuhdemo_demoindex
