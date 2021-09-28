#!/usr/bin/env bash

REPOSITORY=$(pwd)

source $(pwd)/utils/arrays.sh

for i in "${!files[@]}"; do
    # Get the destination path
    destpath="${files[${i}]}"

    # Create directory
    if [[ "${destpath}" != "${REPOSITORY}" ]]; then
        mkdir -p "${destpath}"
    fi

    # Synchronize files from the home dir with the repository dir
    rsync -av --copy-links "${HOME}/${i}" "${REPOSITORY}/${files[${i}]}"
done

# Cleanup
rm -rf "${REPOSITORY}/.config/nvim/lua/init/packer_compiled.lua"
