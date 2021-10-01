#!/usr/bin/env bash

REPOSITORY="$(pwd)"

source "$(pwd)/utils/arrays.sh"

# shellcheck disable=SC2154
for i in "${!files[@]}"; do
    rm -rf "${REPOSITORY:?}/${i}"
done
