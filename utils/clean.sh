#!/usr/bin/env bash

REPOSITORY="$(pwd)"

source "$(pwd)/utils/arrays.sh"

for i in "${!files[@]}"; do
    rm -rf "${REPOSITORY}/${i}"
done
