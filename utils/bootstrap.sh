#!/usr/bin/env bash

REPOSITORY="$(pwd)"

source "$(pwd)/utils/arrays.sh"

# shellcheck disable=SC2154
for i in "${!files[@]}"; do
    # Get the destination path
    destpath="${files[${i}]}"

    # Create directory
    if [[ "${destpath}" != "" ]]; then
        mkdir -p "${HOME}/${destpath}"
    fi

    # Synchronize files from the repository dir to home dir
    rsync -av --copy-links "${REPOSITORY:?}/${i}" "${HOME:?}/${files[${i}]}"
done

# ~/.ssh
[[ ! -d ${HOME:?}/.ssh ]] && mkdir -m "${HOME:?}/.ssh"
sed "s,@@HOSTNAME@@,$(hostname -s),g" "${REPOSITORY:?}/.ssh/config" > \
    "${HOME:?}/.ssh/config"
