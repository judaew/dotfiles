#!/usr/bin/env bash

COMPLETION_PATH="${XDG_DATA_HOME:?}/bash-completion/completions"
mkdir -p ${COMPLETION_PATH}

TMUX_URL="https://raw.githubusercontent.com/imomaliev/tmux-bash-completion/master/completions/tmux"
DOCKER_URL="https://raw.githubusercontent.com/docker/cli/master/contrib/completion/bash/docker"
DOCKER_COMPOSE_URL="https://raw.githubusercontent.com/docker/compose/master/contrib/completion/bash/docker-compose"
GO2PORT_URL="https://gist.githubusercontent.com/judaew/76b3df37ca11c079506105ffc6fe1440/raw/cedf0302db9a22b4a6a704323642473112cfc426/go2port"

declare -A urls=(
    ["tmux"]="${TMUX_URL}"
    ["docker"]="${DOCKER_URL}"
    ["docker-compose"]="${DOCKER_COMPOSE_URL}"
    ["go2port"]="${GO2PORT_URL}"
)

for i in "${!urls[@]}"; do
    wget ${urls[${i}]} -O "${COMPLETION_PATH}/${i}" &> /dev/null && \
        echo "${i} downloaded" || \
        echo "${i} failed"
done

warp-cli generate-completions bash > ${COMPLETION_PATH}/warp-cli
