#!/usr/bin/env bash

for cmd in ghq fzf; do
    if ! command -v "$cmd" &>/dev/null; then
        echo "Error: $cmd not found."
        exit 1
    fi
done

error_enter_dir() {
    echo "Can't enter to directory, exiting..."
}

PROJECT_DIR="$(ghq list | fzf)"

if [ -n "${PROJECT_DIR}" ]; then
    cd "${HOME}/wrk/${PROJECT_DIR}" || error_enter_dir
fi
