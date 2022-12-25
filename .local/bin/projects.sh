#!/usr/bin/env bash

# This is a simple script to change to a project directory, but it is very
# error prone. It is better to rewrite it in C with support for reading
# hotkeys. For example, with curses that attempt to provide portable keyboard
# input facilities.

error_enter_dir() {
    echo "Can't enter to directory, exiting..."
    exit 1
}

WORKSPACES_DIR="${HOME}/Workspaces"
cd "${WORKSPACES_DIR}" || error_enter_dir

PROJ_INSTANCES_LIST=( \
    "gitlab.com" \
    "bitbucket.org" \
    "github.com" \
    "study" \
    "local" \
)

cd "$(printf "%s\n" "${PROJ_INSTANCES_LIST[@]}" | fzf)" || error_enter_dir

# shellcheck disable=SC2144
while [ ! -d "./.git" ] || [ ! -f "./README"* ] ;
do
    cd "$(find . -maxdepth 1 -mindepth 1 -type d | fzf)" || error_enter_dir
    sleep 0.1
done
