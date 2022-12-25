#!/usr/bin/env bash

# This is a simple script to change to a project directory, but it is very
# error prone. It is better to rewrite it in C with support for reading
# hotkeys. For example, with curses that attempt to provide portable keyboard
# input facilities.

WORKSPACES_DIR="${HOME}/Workspaces"
cd ${WORKSPACES_DIR}

PROJ_INSTANCES_LIST=( \
    "gitlab.com" \
    "bitbucket.org" \
    "github.com" \
    "study" \
    "local" \
)

cd $(printf "%s\n" "${PROJ_INSTANCES_LIST[@]}" | fzf)

while [ ! -d "./.git" ] || [ ! -f "./README"* ] ;
do
    cd "$(ls . | fzf)"
    sleep 0.1
done
