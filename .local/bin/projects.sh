#!/usr/bin/env bash

error_enter_dir() {
    echo "Can't enter to directory, exiting..."
}

PROJECT_DIR="$(ghq list | fzf)"

if [ -n "${PROJECT_DIR}" ]; then
    cd "${HOME}/Workspaces/${PROJECT_DIR}" || error_enter_dir
fi
